################################################################################
### build_wide_table()
###
### Assembles a single wide Table 1 (one column-pair per model) from a named
### list of run_inference_analysis() results (see run_inference_analysis_v2.R).
################################################################################

build_wide_table <- function(lpm_results, model_order, model_labels) {

  library(tidyverse)

  row_order <- c(
    "Joint: OL first",
    "Joint: HS first",
    "Separate Labeling",
    "With Confidence (vs. Without)",
    "Batch Prompt (vs. Indiv. Prompts)",
    "$\\sigma^2$",
    "$\\tau_{00}$\\textsubscript{tweet\\_id}",
    "ICC",
    "$N$\\textsubscript{tweet\\_id}",
    "Observations",
    "Marginal $R^2$",
    "Conditional $R^2$"
  )

  per_model <- map2(model_order, model_labels, function(key, label) {
    lpm_results[[key]]$paper_table %>%
      transmute(
        term_clean,
        !!paste0(label, "_OL") := OL_estimate_fmt,
        !!paste0(label, "_HS") := HS_estimate_fmt
      )
  })

  wide <- reduce(per_model, full_join, by = "term_clean") %>%
    mutate(term_clean = factor(term_clean, levels = row_order)) %>%
    arrange(term_clean)

  wide
}

################################################################################
### wide_table_to_latex()
###
### Renders the wide table as tab-separated LaTeX rows, chunked into groups of
### `models_per_block` model-columns so it can be split across multiple
### \resizebox / landscape blocks if needed.
################################################################################

wide_table_to_latex <- function(wide, model_labels, models_per_block = length(model_labels)) {

  library(tidyverse)

  blocks <- split(model_labels, ceiling(seq_along(model_labels) / models_per_block))

  map_chr(blocks, function(labels_block) {
    cols <- as.vector(rbind(paste0(labels_block, "_OL"), paste0(labels_block, "_HS")))
    rows <- apply(wide, 1, function(r) {
      vals <- ifelse(is.na(r[cols]), "--", r[cols])
      paste(c(as.character(r[["term_clean"]]), vals), collapse = " & ")
    })
    paste(paste0(rows, " \\\\"), collapse = "\n")
  })
}

################################################################################
### paper_table_to_latex()
###
### Renders a single-model-style paper table (term_clean, OL_estimate_fmt,
### OL_se_fmt, HS_estimate_fmt, HS_se_fmt -- as returned by
### run_inference_analysis() or run_pooled_analysis()) as LaTeX rows, one row
### per term, same convention used for the per-model appendix tables.
################################################################################

paper_table_to_latex <- function(paper_table) {

  library(tidyverse)

  latex_rows <- paper_table %>%
    mutate(
      latex_row = paste0(
        term_clean, " & ",
        OL_estimate_fmt, " & ", OL_se_fmt, " & ",
        HS_estimate_fmt, " & ", HS_se_fmt, " \\\\"
      )
    ) %>%
    pull(latex_row)

  paste(latex_rows, collapse = "\n")
}
