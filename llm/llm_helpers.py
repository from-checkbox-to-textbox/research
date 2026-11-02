# ------------------------------------------------------------------------------
# Title:   Helpers for the three-provider LLM coding
# Purpose: Functions shared by llm_01_design_prompts.py and
#          llm_02_code_responses.py: API keys and settings, one function per
#          provider to send a prompt, the coding rules ported from
#          R/ai_process_responses.R (template fill, code parsing, validation,
#          modal code), and a small pacer that keeps each run under the
#          provider's rate limit.
# ------------------------------------------------------------------------------

import asyncio
import json
import os
import random
import re
import sys
import time
import tomllib
from pathlib import Path

import anthropic
import httpx
import openai
from google import genai
from google.genai import types

HERE = Path(__file__).parent
PROVIDERS = ["google", "anthropic", "openai"]


# Keys and settings ------------------------------------------------------------

def load_keys():
    # Keys come from a text file of "label line, then key line" pairs (path in
    # C2T_KEYS_FILE), or from the usual environment variables. GPT-OSS and the
    # OpenRouter route for Gemini both use the OpenRouter key; the OpenAI key
    # is optional (OpenAI's own API does not serve GPT-OSS to every account).
    key_file = os.environ.get("C2T_KEYS_FILE")
    if key_file is None:
        found = {"Anthropic": os.environ.get("ANTHROPIC_API_KEY"),
                 "Gemini": os.environ.get("GEMINI_API_KEY"),
                 "OpenRouter": os.environ.get("OPENROUTER_API_KEY"),
                 "OpenAI": os.environ.get("OPENAI_API_KEY")}
    else:
        lines = [l.strip() for l in Path(key_file).expanduser().read_text().splitlines() if l.strip()]
        found = dict(zip(lines[0::2], lines[1::2]))

    prefixes = {"Anthropic": "sk-ant-", "Gemini": "AIza", "OpenRouter": "sk-or-", "OpenAI": "sk-"}
    for label, prefix in prefixes.items():
        if label == "OpenAI" and not found.get(label):
            continue
        if not found.get(label):
            raise ValueError(f"No {label} key (file: {key_file})")
        if not found[label].startswith(prefix) or (label == "OpenAI" and found[label].startswith("sk-or-")):
            raise ValueError(f"The {label} key does not look like a {label} key (file: {key_file})")
    return {"anthropic": found["Anthropic"],
            "google": found["Gemini"],
            "openai": found["OpenRouter"],             # GPT-OSS through OpenRouter
            "google_openrouter": found["OpenRouter"],  # Gemini through OpenRouter
            "openai_direct": found.get("OpenAI")}


def load_settings(provider):
    with open(HERE / "models.toml", "rb") as f:
        return tomllib.load(f)[provider]


def make_client(provider, key, timeout=120):
    # Retries are handled in ask() so that rate-limit errors can be counted
    # and the pace adjusted; the SDKs' own retries are turned off.
    if provider == "anthropic":
        return anthropic.AsyncAnthropic(api_key=key, max_retries=0, timeout=timeout)
    if provider in ("openai", "google_openrouter"):
        s = load_settings(provider)
        return openai.AsyncOpenAI(api_key=key, base_url=s["base_url"], max_retries=0, timeout=timeout)
    if provider == "google":
        return genai.Client(api_key=key, http_options=types.HttpOptions(timeout=timeout * 1000))
    raise ValueError(provider)


# Sending one prompt -----------------------------------------------------------
# Every request is a single user message holding one prompt, with no history.
# Each function returns the reply text and what we need to log about the call.

async def send_anthropic(client, s, role, prompt):
    if role == "designer":
        msg = await client.messages.create(
            model=s["designer"], max_tokens=s["designer_max_tokens"],
            output_config={"effort": s["designer_effort"]},
            messages=[{"role": "user", "content": prompt}])
        price = s["designer_price"]
    else:
        msg = await client.messages.create(
            model=s["coder"], max_tokens=s["coder_max_tokens"],
            thinking={"type": "disabled"},
            messages=[{"role": "user", "content": prompt}])
        price = s["coder_price"]
    u = msg.usage
    details = getattr(u, "output_tokens_details", None)
    return {"text": "".join(b.text for b in msg.content if b.type == "text"),
            "input_tokens": u.input_tokens,
            "output_tokens": u.output_tokens,
            "reasoning_tokens": getattr(details, "thinking_tokens", 0) or 0,
            "cost": (u.input_tokens * price[0] + u.output_tokens * price[1]) / 1e6,
            "stop_reason": msg.stop_reason,
            "model_version": msg.model,
            "served_by": "anthropic",
            "request_id": msg._request_id}


async def send_google(client, s, role, prompt):
    if role == "designer":
        model, config, price = s["designer"], None, s["designer_price"]
    else:
        model, price = s["coder"], s["coder_price"]
        config = types.GenerateContentConfig(
            temperature=s["coder_temperature"],
            max_output_tokens=s["coder_max_tokens"],
            thinking_config=types.ThinkingConfig(thinking_level=s["coder_thinking_level"]))
    resp = await client.aio.models.generate_content(model=model, contents=prompt, config=config)
    u = resp.usage_metadata
    thoughts = u.thoughts_token_count or 0
    output = (u.candidates_token_count or 0) + thoughts
    parts = resp.candidates[0].content.parts if resp.candidates and resp.candidates[0].content else None
    text = "".join(p.text for p in parts or [] if p.text and not p.thought)
    return {"text": text,
            "input_tokens": u.prompt_token_count,
            "output_tokens": output,
            "reasoning_tokens": thoughts,
            "cost": (u.prompt_token_count * price[0] + output * price[1]) / 1e6,
            "stop_reason": str(resp.candidates[0].finish_reason) if resp.candidates else None,
            "model_version": resp.model_version,
            "served_by": "google",
            "request_id": resp.response_id}


async def send_openai(client, s, role, prompt):
    # OpenRouter, pinned to a single host with no fallback to other hosts
    route = {"order": [s["host"]], "allow_fallbacks": False}
    if "quantization" in s:
        route["quantizations"] = [s["quantization"]]
    if role == "designer":
        resp = await client.chat.completions.create(
            model=s["designer"], max_tokens=s["designer_max_tokens"],
            messages=[{"role": "user", "content": prompt}],
            extra_body={"provider": route, "usage": {"include": True}})
        price = s["designer_price"]
    else:
        sampling = {"temperature": s["coder_temperature"]}
        if "coder_top_p" in s:
            sampling["top_p"] = s["coder_top_p"]
        resp = await client.chat.completions.create(
            model=s["coder"], max_tokens=s["coder_max_tokens"], **sampling,
            messages=[{"role": "user", "content": prompt}],
            extra_body={"provider": route, "reasoning": {"effort": s["coder_reasoning_effort"]},
                        "usage": {"include": True}})
        price = s["coder_price"]
    u = resp.usage
    details = getattr(u, "completion_tokens_details", None)
    cost = getattr(u, "cost", None)
    if cost is None:
        cost = (u.prompt_tokens * price[0] + u.completion_tokens * price[1]) / 1e6
    choice = resp.choices[0]
    return {"text": choice.message.content or "",
            "input_tokens": u.prompt_tokens,
            "output_tokens": u.completion_tokens,
            "reasoning_tokens": getattr(details, "reasoning_tokens", 0) or 0,
            "cost": cost,
            "stop_reason": choice.finish_reason,
            "model_version": resp.model,
            "served_by": getattr(resp, "provider", None),
            "request_id": resp.id}


SENDERS = {"anthropic": send_anthropic, "google": send_google, "openai": send_openai,
           "google_openrouter": send_openai}

# Counts of rate-limit and server errors during a run, for the summary
transient_errors = {"rate_limit": 0, "server": 0, "connection": 0}


def error_kind(err):
    status = getattr(err, "status_code", None) or getattr(err, "code", None)
    if status == 429:
        return "rate_limit"
    if status in (408, 409, 500, 502, 503, 504, 529):
        return "server"
    if isinstance(err, (anthropic.APIConnectionError, openai.APIConnectionError,
                        httpx.TransportError, asyncio.TimeoutError)):
        return "connection"
    return None


async def ask(provider, client, s, role, prompt, pacer=None, tries=8):
    # Sends one prompt. Rate-limit, server and connection errors are retried
    # with a growing wait; they do not count as coding attempts. Any other
    # error (a bad request, for instance) stops the run.
    for k in range(tries):
        if pacer is not None:
            await pacer.wait()
        start = time.monotonic()
        try:
            reply = await SENDERS[provider](client, s, role, prompt)
            reply["seconds"] = round(time.monotonic() - start, 3)
            return reply
        except Exception as err:
            kind = error_kind(err)
            if kind is None or k == tries - 1:
                raise
            transient_errors[kind] += 1
            if transient_errors[kind] <= 3:   # show the first few, so the cause is visible during a run
                print(f"\n{kind} error from {provider}: {str(err)[:300]}", file=sys.stderr)
            if kind == "rate_limit" and pacer is not None:
                pacer.slow_down()
            await asyncio.sleep(min(60, 2 ** k) + random.random())


# Pacing -------------------------------------------------------------------------

class Pacer:
    # Spaces requests evenly under a per-minute ceiling. The run starts at 20%
    # of the ceiling and speeds up by 10% every 15 seconds; rate-limit errors
    # cut the pace by 10% (at most once every 5 seconds). Starting slowly
    # avoids the errors providers return when traffic jumps suddenly.
    def __init__(self, requests_per_minute):
        self.ceiling = requests_per_minute / 60
        self.rate = max(1.0, self.ceiling * 0.2)
        self.next_slot = time.monotonic()
        self.last_change = time.monotonic()
        self.last_cut = 0.0

    async def wait(self):
        now = time.monotonic()
        if now - self.last_change > 15:
            self.rate = min(self.ceiling, self.rate * 1.1)
            self.last_change = now
        slot = max(self.next_slot, now)
        self.next_slot = slot + 1 / self.rate
        await asyncio.sleep(slot - now)

    def slow_down(self):
        # Many requests are in flight at once, so one burst can return many
        # rate-limit errors together; cut the pace at most once every 5 seconds
        now = time.monotonic()
        if now - self.last_cut > 5:
            # never below a quarter of the ceiling: rate-limit replies cost
            # nothing, and each failed request already waits before retrying
            self.rate = max(1.0, self.ceiling * 0.25, self.rate * 0.9)
            self.last_cut = now
            self.last_change = now


# Coding rules, ported from R/ai_process_responses.R -----------------------------

def fill_template(template, values):
    # Same result as glue::glue_data() for these templates: {name} is replaced
    # by its value and {{ }} stand for literal braces. Any other {...} would be
    # evaluated as R code by glue, so it is an error here.
    def replace(m):
        if m.group(0) == "{{":
            return "{"
        if m.group(0) == "}}":
            return "}"
        name = m.group(1).strip()
        if name not in values:
            raise ValueError(f"Unknown placeholder {{{name}}} in template")
        return str(values[name])
    return re.sub(r"\{\{|\}\}|\{([^{}]*)\}", replace, template)


def build_prompt(system_message, user_template, variable, task):
    user_message = fill_template(user_template, {
        "variable_name": variable["variable_name"],
        "question_text": variable["question_text"],
        "options_block": variable["options_block"],
        "open_response": task["open_response"]})
    # paste(system_msg, "\n\n", user_msg) in R joins with spaces
    return system_message + " \n\n " + user_message, user_message


def parse_code(text):
    # First whole number in the reply, as regexpr("\\b[0-9]+\\b") in R
    m = re.search(r"\b[0-9]+\b", text or "")
    return int(m.group(0)) if m else None


def is_valid(code, variable):
    if code is None:
        return False
    if variable["is_numeric"]:
        return min(variable["codes"]) <= code <= max(variable["codes"])
    return code in variable["codes"]


def modal_code(codes):
    # which.max(table(codes)) in R: the most frequent code, and the smallest
    # code when two or more are tied
    counts = {}
    for c in codes:
        counts[c] = counts.get(c, 0) + 1
    best = max(counts.values())
    return min(c for c, n in counts.items() if n == best), best


def parse_designer_reply(text):
    # Same steps as parse_ai_response() in R/ai_prompt_generator.R: a fenced
    # block that contains "system_message", otherwise the outermost {...}
    candidate = None
    if "```" in text:
        for part in text.split("```"):
            if "{" in part and '"system_message"' in part:
                candidate = re.sub(r"^\s*(json)?\s*", "", part).strip()
                break
    if candidate is None:
        m = re.search(r"\{.*\}", text, re.S)
        candidate = m.group(0) if m else None
    if candidate is None:
        raise ValueError("No JSON object in the designer's reply")
    parsed = json.loads(candidate)
    return parsed["system_message"], parsed["user_template"]


# Files -------------------------------------------------------------------------

def read_jsonl(path):
    if not Path(path).exists():
        return []
    with open(path, encoding="utf-8") as f:
        return [json.loads(line) for line in f if line.strip()]
