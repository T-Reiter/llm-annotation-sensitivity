################################################################################
### run_pooled_analysis()
###
### Pooled LPM across all models: one regression per outcome (OL, HS) with
### task structure, batch, confidence, AND model as fixed effects (sum-coded
### task and model, so the omitted reference level's coefficient is the
### negative sum of the others -- "recovered" post hoc). Complements the
### per-model wide Table 1 with a single-model view of how models differ.
###
### `model_dfs` is a named list of combined_df (as returned by load_model()),
### names are used as the model factor's levels. `model_labels` gives the
### display label per model key (same order).
################################################################################

run_pooled_analysis <- function(
    model_dfs,
    model_labels
) {

  library(tidyverse)
  library(stringr)
  library(lme4)
  library(lmerTest)
  library(broom.mixed)

  model_keys <- names(model_dfs)

  df_all <- imap_dfr(model_dfs, function(df, key) {
    df %>%
      select(batch_id, tweet_in_batch, tweet_id, tweet, condition, shot, starts_with("R")) %>%
      pivot_longer(
        cols      = starts_with("R"),
        names_to  = c("responder", ".value"),
        names_sep = "_"
      ) %>%
      mutate(
        OL    = as.integer(str_detect(label, "(?<!N)OL")),
        HS    = as.integer(str_detect(label, "(?<!N)HS")),
        batch = ifelse(grepl("batch", condition), 1, 0),
        conf  = ifelse(grepl("conf", condition), 1, 0),
        task  = case_when(
          grepl("C\\.OL", condition) ~ "C.OL",
          grepl("C\\.HS", condition) ~ "C.HS",
          grepl("^A", condition)     ~ "A",
          grepl("^B", condition)     ~ "B",
          TRUE                       ~ NA_character_
        ),
        model = key
      )
  })

  model_levels <- model_keys
  df_all <- df_all %>%
    mutate(model = factor(model, levels = model_levels, labels = model_labels[model_levels]))

  fit_one <- function(outcome) {

    drop_task <- if (outcome == "OL") "C\\.HS" else "C\\.OL"

    df_out <- df_all %>%
      filter(!grepl(drop_task, condition)) %>%
      mutate(
        task = factor(
          task,
          levels = if (outcome == "OL") c("A", "B", "C.OL") else c("A", "B", "C.HS"),
          labels = c("Joint: OL first", "Joint: HS first", "Separate Labeling")
        ),
        batch = factor(batch),
        conf  = factor(conf)
      )

    contrasts(df_out$task)  <- contr.sum(levels(df_out$task))
    contrasts(df_out$model) <- contr.sum(levels(df_out$model))

    fml <- as.formula(paste0(outcome, " ~ 0 + task + batch + conf + model + (1 | tweet_id)"))
    lmer(fml, data = df_out)
  }

  model_OL <- fit_one("OL")
  model_HS <- fit_one("HS")

  # contr.sum() drops level names -- "model1".."model(k-1)" refer to the first
  # (k-1) levels of the model factor, in factor-level order (the last level is
  # the omitted/recovered one). Map the numeric suffix back to the display name.
  model_level_labels <- unname(model_labels[model_keys])

  extract_fixed <- function(model, outcome) {
    broom.mixed::tidy(model, effects = "fixed") %>%
      mutate(
        outcome = outcome,
        stars = case_when(
          p.value < .001 ~ "***",
          p.value < .01  ~ "**",
          p.value < .05  ~ "*",
          TRUE           ~ ""
        ),
        term_clean = case_when(
          term == "taskJoint: OL first"   ~ "Joint: OL first",
          term == "taskJoint: HS first"   ~ "Joint: HS first",
          term == "taskSeparate Labeling" ~ "Separate Labeling",
          term == "batch1"                ~ "Batch Prompt (vs. Indiv. Prompts)",
          term == "conf1"                 ~ "With Confidence (vs. Without)",
          str_starts(term, "model")       ~ paste0(
            "Model: ",
            model_level_labels[as.integer(str_remove(term, "^model"))]
          ),
          TRUE ~ term
        )
      ) %>%
      select(outcome, term_clean, estimate, std.error, statistic, p.value, stars)
  }

  fixed_table <- bind_rows(
    extract_fixed(model_OL, "OL"),
    extract_fixed(model_HS, "HS")
  )

  ##############################################################################
  ### Recover the omitted (last) model level's effect from sum-to-zero coding:
  ### effect_omitted = -sum(effects of the other levels)
  ##############################################################################

  omitted_label <- model_labels[[model_keys[length(model_keys)]]]

  recovered <- fixed_table %>%
    filter(str_starts(term_clean, "Model: ")) %>%
    group_by(outcome) %>%
    summarise(estimate = -sum(estimate), .groups = "drop") %>%
    mutate(
      term      = "model_recovered",
      term_clean = paste0("Model: ", omitted_label),
      std.error = NA_real_,
      statistic = NA_real_,
      p.value   = NA_real_,
      stars     = ""
    )

  fixed_table_full <- bind_rows(fixed_table, recovered)

  ##############################################################################
  ### Paper-style wide table (term_clean, OL/HS estimate + SE)
  ##############################################################################

  row_order <- c(
    "Joint: OL first",
    "Joint: HS first",
    "Separate Labeling",
    "With Confidence (vs. Without)",
    "Batch Prompt (vs. Indiv. Prompts)",
    paste0("Model: ", model_level_labels)
  )

  paper_table <- fixed_table_full %>%
    mutate(
      estimate_fmt = ifelse(
        is.na(std.error),
        sprintf("%.3f", estimate),
        sprintf("%.3f%s", estimate, stars)
      ),
      se_fmt = ifelse(is.na(std.error), "--", sprintf("%.3f", std.error))
    ) %>%
    select(term_clean, outcome, estimate_fmt, se_fmt) %>%
    pivot_wider(
      names_from = outcome,
      values_from = c(estimate_fmt, se_fmt),
      names_glue = "{outcome}_{.value}"
    ) %>%
    select(term_clean, OL_estimate_fmt, OL_se_fmt, HS_estimate_fmt, HS_se_fmt) %>%
    mutate(term_clean = factor(term_clean, levels = row_order)) %>%
    arrange(term_clean) %>%
    mutate(term_clean = as.character(term_clean))

  # Saving to disk (CSV + LaTeX) is done by the caller (see main.R), same as
  # for the wide Table 1.
  list(
    model_OL = model_OL,
    model_HS = model_HS,
    fixed_table = fixed_table,
    fixed_table_full = fixed_table_full,
    paper_table = paper_table
  )
}
