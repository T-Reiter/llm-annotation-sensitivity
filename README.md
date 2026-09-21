# Annotation Sensitivity — Data & Code Pipeline

Reproducibility package for *"Reliable but Sensitive: Evaluating LLM Annotation Beyond Performance."*

Everything in the paper except `method6.png` (a hand-drawn diagram of the prompt
conditions) is produced by the R scripts in `data_work/src/`.

## Quick start

All paths are resolved from the repository root by the
[`here`](https://here.r-lib.org/) package, so **run the scripts with the
repository root as your working directory**. No path in this repository is
absolute; a fresh clone or an unzipped release works unchanged.

A clone contains the code, the raw LLM annotations (`data_work/raw/`) and the generated
figures and tables. It does **not** contain `data_work/processed/` — every file there is
rebuilt by `process_raw_data.R`. Rebuilding also needs the Kern et al. human
annotations, which are not redistributed here; see *Input data* below for how to
download them.

```bash
cd /path/to/llm-annotation-sensitivity

# 1. Download the Kern et al. reference files into data_work/Tweets_CK/
mkdir -p data_work/Tweets_CK
curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_train_s.csv \
  -o data_work/Tweets_CK/full_train_s.csv
curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_test_s.csv \
  -o data_work/Tweets_CK/full_test_s.csv

# 2. Raw data -> processed data -> every table and figure in the paper:
Rscript data_work/src/run_from_raw.R

# Same, but reuse any model fits already cached in data_work/outputs/cache/:
USE_CACHE=1 Rscript data_work/src/run_from_raw.R
```

Once `data_work/processed/` exists you can re-run the analysis alone, without touching the
raw data:

```bash
Rscript data_work/src/run_all.R                      # stages 01-08, cached fits
PIPELINE_REFRESH=1 Rscript data_work/src/run_all.R   # force a full refit
```

`data_work/src/recreate_all_results.R` is a standalone alternative that recomputes the
headline numbers from the processed data in a single process, without the
caching or the LaTeX writers. It is a cross-check on `run_all.R`, not a
replacement for it.

## Requirements

- R ≥ 4.1 (`02_regressions.R` uses the native `|>` pipe)
- Packages: `here`, `tidyverse` (`dplyr`, `tidyr`, `readr`, `purrr`, `tibble`,
  `ggplot2`), `data.table`, `lme4`, `lmerTest`, `lmtest`, `sandwich`, `pROC`,
  `patchwork`

```r
install.packages(c("here", "tidyverse", "data.table", "lme4", "lmerTest",
                   "lmtest", "sandwich", "pROC", "patchwork"))
```

Figures are written with `grDevices::pdf()` and `grDevices::png()`, so no cairo
device is required.

## Input data

### LLM annotations — `data_work/raw/`

36 CSVs across nine model directories. Folder names map to canonical model
names in `process_raw_data.R`:

| Folder | Model |
|---|---|
| `GPT4o_mini` | GPT-4o-mini |
| `GPT5.4` | GPT-5.4 |
| `GPT5.4_mini` | GPT-5.4-mini |
| `GPT54_mini_run1_actually_4o_mini` | GPT-4o-mini_run2 |
| `Llama3.1_8B_new` | Llama-3.1-8B |
| `Llama3.1_70B_new` | Llama-3.1-70B |
| `Llama4` | Llama-4 |
| `MistralLarge3` | Mistral-Large-3 |
| `MistralMedium3.5_new` | Mistral-Medium-3.5 |

Seven of these are reported in the paper (`MODEL_ORDER` in `00_utils.R`).
`GPT-4o-mini_run2` is a re-collection ten months later, used only for the
temporal-drift check; `GPT-5.4-mini` is excluded from every reported result.

Each model was run over 3,000 tweets in 16 conditions × 3 replicate runs. The
16 conditions cross four task structures with batching and confidence
elicitation:

- `A` — joint call, offensive language asked first
- `B` — joint call, hate speech asked first
- `C.OL` — separate call, offensive language only
- `C.HS` — separate call, hate speech only

each optionally suffixed `_batch` (several tweets per API call) and `_conf`
(confidence elicited alongside the label).

### Tweet text — where it comes from

The 3,000 tweets annotated here originate in the Davidson et al. (2017)
hate-speech and offensive language corpus:

> <https://github.com/t-davidson/hate-speech-and-offensive-language>

They reach this repository through the subsample that Kern et al. (2023) had
annotated by human coders. The corpus is MIT licensed, so the tweet text is
redistributed here with Davidson et al.'s notice retained (see `data_work/LICENSE`).
Screen names are masked and URLs are stripped in every copy. If you
use this data, cite the original corpus:

> Davidson, T., Warmsley, D., Macy, M., & Weber, I. (2017). Automated Hate
> Speech Detection and the Problem of Offensive Language. *Proceedings of the
> 11th International AAAI Conference on Web and Social Media* (ICWSM '17),
> 512–515.

Be aware of what the corpus contains: it was assembled to study hate speech and
offensive language, and the tweet text is racist, sexist, homophobic and
otherwise offensive throughout.

### Human reference annotations — `data_work/Tweets_CK/` (not included)

The Kern et al. (2023) human reference annotations are **not** redistributed in
this repository.

Before running the pipeline, download the two reference CSV files (`full_train_s.csv`
and `full_test_s.csv`, $n=44,900$ combined) into `data_work/Tweets_CK/`:

```bash
mkdir -p data_work/Tweets_CK
curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_train_s.csv \
  -o data_work/Tweets_CK/full_train_s.csv
curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_test_s.csv \
  -o data_work/Tweets_CK/full_test_s.csv
```

For full details on this reference dataset, its schema, expected fingerprints, or
how to generate it directly from the Hugging Face release, see
[`data_work/Tweets_CK/README.md`](data_work/Tweets_CK/README.md).


## Directory layout

```
llm-annotation-sensitivity/
├── README.md                  ← this file
├── LICENSE                    ← MIT license for code
├── plots/                     ← generated figures (pdf & png)
└── data_work/
    ├── README.md              ← data pipeline notes
    ├── LICENSE                ← CC BY 4.0 for data (with Davidson et al. notice)
    ├── raw/                   ← 36 LLM annotation CSVs in 9 model directories
    ├── Tweets_CK/             ← Kern et al. reference data (gitignored, not shipped)
    ├── processed/             ← built by process_raw_data.R (gitignored)
    │   ├── df_long.csv        ← tweet × condition × run × model, ~756k rows
    │   ├── df_agg.csv         ← modal labels per tweet × condition × model
    │   ├── prevalences.csv    ← prevalence per model × condition × run
    │   └── kern_full.csv      ← combined Kern reference annotations
    ├── outputs/               ← LaTeX fragments, coefficient exports, cache/
    └── src/                   ← all analysis code
```

Nothing in `data_work/processed/` is committed — all four files are rebuilt by
`process_raw_data.R`, and `kern_full.csv` in particular is derived from data this
repository does not redistribute. Stages 01–06 read `df_long.csv`; stage 04 also
reads `kern_full.csv`. Run `process_raw_data.R` (or `run_from_raw.R`) before
`run_all.R` on a fresh clone.

## Pipeline stages

`run_all.R` executes stages 01–08, each in its own R process, then verifies that
every expected figure and table exists.

| # | Script | Tables / CSVs | Figures |
|---|---|---|---|
| 0 | `process_raw_data.R` | `processed/df_long.csv`, `df_agg.csv`, `prevalences.csv`, `kern_full.csv` | — |
| 1 | `01_agreement.R` | `table_agreement_ladder.tex`, `ladder_{fleiss_within,cohen_across,cohen_human,icc,per_model}.csv` | `agreements_few_new_conditions_2x1` |
| 2 | `02_regressions.R` | `table_taskeffects_all.tex`, `table_task_structure_glmer.tex`, `table_permodel.tex`, `permodel_coefs.csv`, `pooled_coefs.csv` | — |
| 3 | `03_deff.R` | `table_deff.tex`, `deff_{run_prevalence,per_model,components,reml_components_raw}.csv` | — |
| 4 | `04_human_design.R` | `table_human_llm.tex`, `human_llm_components.csv`, `kern_version_prevalence.csv` | — |
| 5 | `05_confidence.R` | `table_confidence_permodel.tex`, `confidence_per_model.csv` | `confidence_calibration_and_flips`, `confidence_calibration_two_panel`, `confidence_calibration_per_model` |
| 6 | `06_prevalence.R` | — | `prevalence_heatmap_all_models_vs_model_mean_stacked`, `prevalence_stability_runs` |
| 7 | `07_figures.R` | — | `design_effects_families` (from stage 2's coefficient exports) |
| 8 | `08_paper_tables.R` | `table_framework.tex`, `table_costs.tex`, `annotation_costs.csv` | — |

Every figure is written as both `.pdf` and `.png` into `plots/`.

`00_utils.R` is sourced by every stage. It defines the paths, the model
constants and plotting theme, the agreement and calibration estimators, the
LaTeX table writer, and the eligibility filter described below. It runs no
analysis of its own.

## Two things worth knowing before you read the numbers

### The eligibility filter

`process_raw_data.R` records an outcome that was never elicited as `0`, not
`NA`. In the separate-call conditions only one construct is asked for: `C.OL`
elicits offensive language only, `C.HS` hate speech only. The parser therefore
writes `HS = 0` for every tweet in `C.OL` and `OL = 0` for every tweet in
`C.HS`.

Any analysis that pools conditions without filtering treats those structural
zeros as substantive negatives, which is fatal for cross-design comparison — it
inflated apparent instability for GPT-4o-mini to 88.5% against the correct
27.1%. `eligible()` in `00_utils.R` is the single canonical filter; use it
rather than rolling your own.

### Caching

Expensive fits are memoised to `data_work/outputs/cache/` as `.rds` files. `PIPELINE_REFRESH=1`
invalidates them and refits from scratch. Stage 02 is the slow one — eight mixed
logistic regressions over ~756k rows, roughly an hour cold and seconds warm.
Delete `data_work/outputs/cache/` if you change anything upstream of a fit and want to be
certain the cache is not stale.

## Licensing

Code is MIT ([`LICENSE`](LICENSE)). The LLM annotation data and derived outputs are
CC BY 4.0 ([`data_work/LICENSE`](data_work/LICENSE)), which also carries the Davidson et al.
MIT notice covering the redistributed tweet text. The Kern et al. reference
annotations are not redistributed here and remain under their original terms.
