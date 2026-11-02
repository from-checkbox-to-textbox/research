# Builds the article from the raw survey data.
#
#   make            from the saved coded answers: analysis, figures and tables
#                   copied into the article, PDF. No API call; reproduces the article exactly.
#   make full       reruns the API calls first (coding of the open-ended answers
#                   by the three providers, then the strategic-vote coding), then
#                   everything else. Needs the API keys and costs about 100
#                   dollars; the codes will differ slightly from the saved ones.
#   make analysis   raw data and analysis only (outputs in results/)
#   make coding     rerun the coding of the open-ended answers only (APIs)
#   make strategic  rerun the exploratory strategic-vote coding only (API)
#   make clean      remove the outputs and the compiled PDF
#
# The article is an Overleaf project, expected in article/ (a clone or a link
# to one; not part of this repository). The raw survey data are not included
# (see DATA_AVAILABILITY.md) and are read from data/raw/survey.rds.

ARTICLE = article
FIGURES = fig_factor_analysis_dumbell.png fig_regression_difference.png \
          fig_scree_by_scale.png fig_factor_analysis_dumbell_pairwise.png
TABLES = completer_composition.tex robustness_coders.tex questionnaire.tex \
         regression_interactions.tex nonresponse_decomposition.tex

.PHONY: all full analysis figures tables pdf coding strategic clean

all: pdf

# Everything, API calls included
full: coding strategic
	$(MAKE) all

# Raw survey data, downloaded once
data/raw/survey.rds:
	Rscript R/00_get_survey.R

# Consensus, cleaning, figures, tables and numbers
analysis: data/raw/survey.rds
	Rscript run_all.R

# Copy the figures into the article, only those that changed
figures: analysis
	@for f in $(FIGURES); do \
	  cmp -s results/figures/$$f $(ARTICLE)/$$f || { cp results/figures/$$f $(ARTICLE)/$$f; echo "updated $$f"; }; \
	done

# Copy the appendix tables into the article (article/tables/), only those that changed
tables: analysis
	@mkdir -p $(ARTICLE)/tables
	@for f in $(TABLES); do \
	  cmp -s results/tables/$$f $(ARTICLE)/tables/$$f || { cp results/tables/$$f $(ARTICLE)/tables/$$f; echo "updated tables/$$f"; }; \
	done

# Compile the article outside the Overleaf folder
pdf: figures tables
	mkdir -p build
	cd $(ARTICLE) && latexmk -pdf -interaction=nonstopmode -outdir=$(CURDIR)/build main.tex
	@echo "Article: build/main.pdf"

# Coding of the open-ended answers by the three providers (needs the API keys,
# see llm/llm_helpers.py)
coding: data/raw/survey.rds
	Rscript R/llm_00_export_inputs.R
	uv run llm/llm_01_design_prompts.py --provider all
	uv run llm/llm_02_code_responses.py --provider google
	uv run llm/llm_02_code_responses.py --provider anthropic
	uv run llm/llm_02_code_responses.py --provider openai
	Rscript R/assemble_json.R

# Exploratory strategic-vote coding (needs OPENROUTER_API_KEY)
strategic: data/raw/survey.rds
	Rscript R/sup_vote_strategic.R

clean:
	rm -rf results build
