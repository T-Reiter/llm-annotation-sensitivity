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

################################################################################
### wide_table_to_standalone_tex()
###
### Wraps wide_table_to_latex()'s row output in a compilable standalone .tex
### document (own \documentclass, header row, tabular), so the table can be
### checked with pdflatex without pulling it into the full paper.
################################################################################

wide_table_to_standalone_tex <- function(wide, model_labels, models_per_block = length(model_labels)) {

  library(tidyverse)

  body_blocks <- wide_table_to_latex(wide, model_labels, models_per_block)

  header1 <- paste(
    c("Term", map_chr(model_labels, ~ paste0("\\multicolumn{2}{c}{", .x, "}"))),
    collapse = " & "
  )
  header2 <- paste(c("", rep(c("OL", "HS"), length(model_labels))), collapse = " & ")
  colspec <- paste0("l", strrep("cc", length(model_labels)))

  c(
    "\\documentclass{article}",
    "\\usepackage[margin=0.5in]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage{amsmath}",
    "\\usepackage{graphicx}",
    "\\begin{document}",
    "\\begin{table}",
    "\\centering",
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    paste0(header1, " \\\\"),
    paste0(header2, " \\\\"),
    "\\midrule",
    body_blocks,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{table}",
    "\\end{document}"
  )
}

################################################################################
### paper_table_to_standalone_tex()
###
### Wraps paper_table_to_latex()'s row output in a compilable standalone .tex
### document (own \documentclass, header row, tabular).
################################################################################

paper_table_to_standalone_tex <- function(paper_table) {

  body_rows <- paper_table_to_latex(paper_table)

  c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{amsmath}",
    "\\begin{document}",
    "\\begin{table}",
    "\\centering",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Term & OL Estimate & OL SE & HS Estimate & HS SE \\\\",
    "\\midrule",
    body_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\end{document}"
  )
}

################################################################################
### combined_pooled_table_to_latex()
###
### Renders the combined pooled table (term_clean plus four side-by-side
### estimate+SE column pairs -- OL_Main, OL_Interaction, HS_Main,
### HS_Interaction, as returned by run_pooled_analysis()$combined_paper_table)
### as LaTeX rows, one row per term.
################################################################################

combined_pooled_table_to_latex <- function(combined) {

  library(tidyverse)

  cols <- c(
    "OL_Main_estimate_fmt", "OL_Main_se_fmt",
    "OL_Interaction_estimate_fmt", "OL_Interaction_se_fmt",
    "HS_Main_estimate_fmt", "HS_Main_se_fmt",
    "HS_Interaction_estimate_fmt", "HS_Interaction_se_fmt"
  )

  rows <- apply(combined, 1, function(r) {
    vals <- ifelse(is.na(r[cols]), "--", r[cols])
    paste(c(r[["term_clean"]], vals), collapse = " & ")
  })

  paste(paste0(rows, " \\\\"), collapse = "\n")
}

################################################################################
### combined_pooled_table_to_standalone_tex()
###
### Wraps combined_pooled_table_to_latex()'s row output in a compilable
### standalone .tex document. `reference_label`, if given, is added as a
### plain table note below the tabular (not a LaTeX \footnote{}).
################################################################################

combined_pooled_table_to_standalone_tex <- function(combined, reference_label = NULL) {

  body_rows <- combined_pooled_table_to_latex(combined)

  header1 <- paste(
    c(
      "Term",
      "\\multicolumn{2}{c}{OL Main}",
      "\\multicolumn{2}{c}{OL Interaction}",
      "\\multicolumn{2}{c}{HS Main}",
      "\\multicolumn{2}{c}{HS Interaction}"
    ),
    collapse = " & "
  )
  header2 <- paste(c("", rep(c("Est.", "SE"), 4)), collapse = " & ")

  note <- if (is.null(reference_label)) {
    character(0)
  } else {
    c(
      "\\smallskip",
      paste0(
        "\\footnotesize \\textit{Note}: \"Model:\" rows are estimated relative to ",
        reference_label, ", the reference model."
      )
    )
  }

  c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{amsmath}",
    "\\usepackage{graphicx}",
    "\\begin{document}",
    "\\begin{table}",
    "\\centering",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccccc}",
    "\\toprule",
    paste0(header1, " \\\\"),
    paste0(header2, " \\\\"),
    "\\midrule",
    body_rows,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    note,
    "\\end{table}",
    "\\end{document}"
  )
}
