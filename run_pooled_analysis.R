################################################################################
### run_pooled_analysis()
###
### Pooled LPM across all models: one regression per outcome (OL, HS) with
### task structure, batch, confidence, AND model as fixed effects. `task`
### keeps the no-intercept full-dummy coding used throughout the paper (every
### level gets a directly-estimated baseline probability). `model` uses plain
### treatment (reference) coding against `reference_model`, so every
### non-reference model's coefficient is a directly-estimated difference from
### it -- no sum-coding recovery needed.
###
### `model_dfs` is a named list of combined_df (as returned by load_model()),
### names are used as the model factor's levels. `model_labels` gives the
### display label per model key (same order). `reference_model` (a key in
### `model_dfs`) is the omitted/reference category for the model factor.
################################################################################

run_pooled_analysis <- function(
    model_dfs,
    model_labels,
    reference_model = "gpt4o_mini"
) {

  library(tidyverse)
  library(stringr)
  library(lme4)
  library(lmerTest)
  library(broom.mixed)
  library(performance)

  model_keys <- names(model_dfs)
  model_keys <- c(reference_model, setdiff(model_keys, reference_model))

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

    contrasts(df_out$task) <- contr.sum(levels(df_out$task))
    # model keeps default treatment contrasts (reference = first level = reference_model)

    fml <- as.formula(paste0(outcome, " ~ 0 + task + batch + conf + model + (1 | tweet_id)"))
    lmer(fml, data = df_out)
  }

  model_OL <- fit_one("OL")
  model_HS <- fit_one("HS")

  reference_label <- model_labels[[reference_model]]

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
            "Model: ", str_remove(term, "^model")
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

  fixed_table_full <- fixed_table

  ##############################################################################
  ### Paper-style wide table (term_clean, OL/HS estimate + SE)
  ##############################################################################

  model_level_labels <- unname(model_labels[setdiff(model_keys, reference_model)])

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

  ##############################################################################
  ### Interaction models: does the "asked first" task effect differ by model?
  ###
  ### These need a normal intercept -- bolting `task:model` onto the
  ### no-intercept `task` dummy above is exactly collinear with it (verified
  ### empirically: R silently drops a column and the fit is meaningless).
  ### So `task` here is split into two explicit 0/1 mains (`ol_first`,
  ### `hs_first`, both vs. "Separate Labeling"), and only the relevant one is
  ### interacted with `model` -- OL model gets ol_first:model, HS model gets
  ### hs_first:model.
  ##############################################################################

  fit_interaction <- function(outcome) {

    drop_task <- if (outcome == "OL") "C\\.HS" else "C\\.OL"
    interaction_var <- if (outcome == "OL") "ol_first" else "hs_first"

    df_out <- df_all %>%
      filter(!grepl(drop_task, condition)) %>%
      mutate(
        task = factor(
          task,
          levels = if (outcome == "OL") c("A", "B", "C.OL") else c("A", "B", "C.HS"),
          labels = c("Joint: OL first", "Joint: HS first", "Separate Labeling")
        ),
        batch    = factor(batch),
        conf     = factor(conf),
        ol_first = as.integer(task == "Joint: OL first"),
        hs_first = as.integer(task == "Joint: HS first")
      )

    fml <- as.formula(paste0(
      outcome, " ~ ol_first + hs_first + batch + conf + model + ",
      interaction_var, ":model + (1 | tweet_id)"
    ))
    lmer(fml, data = df_out)
  }

  model_OL_interaction <- fit_interaction("OL")
  model_HS_interaction <- fit_interaction("HS")

  extract_fixed_interaction <- function(model, outcome) {
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
          term == "(Intercept)" ~ "Separate Labeling",
          term == "ol_first"    ~ "Joint: OL first (vs. Separate Labeling)",
          term == "hs_first"    ~ "Joint: HS first (vs. Separate Labeling)",
          term == "batch1"      ~ "Batch Prompt (vs. Indiv. Prompts)",
          term == "conf1"       ~ "With Confidence (vs. Without)",
          str_detect(term, ":model") ~ paste0(
            "Model: ", str_remove(term, "^(ol_first|hs_first):model"), " x Asked First"
          ),
          str_starts(term, "model") ~ paste0(
            "Model: ", str_remove(term, "^model")
          ),
          TRUE ~ term
        )
      ) %>%
      select(outcome, term_clean, estimate, std.error, statistic, p.value, stars)
  }

  interaction_fixed_table <- bind_rows(
    extract_fixed_interaction(model_OL_interaction, "OL"),
    extract_fixed_interaction(model_HS_interaction, "HS")
  )

  interaction_row_order <- c(
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)",
    "With Confidence (vs. Without)",
    "Batch Prompt (vs. Indiv. Prompts)",
    paste0("Model: ", model_level_labels),
    paste0("Model: ", model_level_labels, " x Asked First")
  )

  interaction_paper_table <- interaction_fixed_table %>%
    mutate(
      estimate_fmt = sprintf("%.3f%s", estimate, stars),
      se_fmt = sprintf("%.3f", std.error)
    ) %>%
    select(term_clean, outcome, estimate_fmt, se_fmt) %>%
    pivot_wider(
      names_from = outcome,
      values_from = c(estimate_fmt, se_fmt),
      names_glue = "{outcome}_{.value}"
    ) %>%
    select(term_clean, OL_estimate_fmt, OL_se_fmt, HS_estimate_fmt, HS_se_fmt) %>%
    mutate(term_clean = factor(term_clean, levels = interaction_row_order)) %>%
    arrange(term_clean) %>%
    mutate(term_clean = as.character(term_clean))

  ##############################################################################
  ### Combined table: one row per term, 4 side-by-side column groups in order
  ### OL Main, OL Interaction, HS Main, HS Interaction. Terms not used by a
  ### given spec are left blank ("--") for that column group. The interaction
  ### models' intercept is the same estimand as "Separate Labeling" in the
  ### main models (baseline probability at batch=0, conf=0, reference model),
  ### so it shares that row rather than getting its own "Intercept" row --
  ### note the two specs' numbers can differ slightly since each model's
  ### REML variance components are estimated independently.
  ##############################################################################

  combined_row_order <- c(
    "Joint: OL first",
    "Joint: HS first",
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)",
    "With Confidence (vs. Without)",
    "Batch Prompt (vs. Indiv. Prompts)",
    paste0("Model: ", model_level_labels),
    paste0("Model: ", model_level_labels, " x Asked First")
  )

  combined_long <- bind_rows(
    fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Main"),
    interaction_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Interaction"),
    fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Main"),
    interaction_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Interaction")
  ) %>%
    mutate(
      spec = factor(spec, levels = c("OL_Main", "OL_Interaction", "HS_Main", "HS_Interaction")),
      estimate_fmt = ifelse(
        is.na(std.error),
        sprintf("%.3f", estimate),
        sprintf("%.3f%s", estimate, stars)
      ),
      se_fmt = ifelse(is.na(std.error), "--", sprintf("%.3f", std.error))
    )

  combined_coef_table <- combined_long %>%
    select(term_clean, spec, estimate_fmt, se_fmt) %>%
    pivot_wider(
      names_from = spec,
      values_from = c(estimate_fmt, se_fmt),
      names_glue = "{spec}_{.value}",
      values_fill = "--"
    ) %>%
    select(
      term_clean,
      OL_Main_estimate_fmt, OL_Main_se_fmt,
      OL_Interaction_estimate_fmt, OL_Interaction_se_fmt,
      HS_Main_estimate_fmt, HS_Main_se_fmt,
      HS_Interaction_estimate_fmt, HS_Interaction_se_fmt
    )

  ##############################################################################
  ### Model fit info (sigma^2, tau00, ICC, N, Observations, R^2), same
  ### statistics/formatting as the per-model wide Table 1
  ### (see run_inference_analysis_v2.R::extract_model_stats()), appended as
  ### extra rows below the coefficients for each of the 4 specs.
  ##############################################################################

  extract_model_stats <- function(model, outcome) {

    vc <- as.data.frame(VarCorr(model))
    sigma2 <- sigma(model)^2
    tau00 <- vc %>% filter(grp == "tweet_id") %>% pull(vcov)
    icc <- tau00 / (tau00 + sigma2)
    r2 <- performance::r2_nakagawa(model)

    tibble(
      outcome = outcome,
      sigma2 = sigma2,
      tau00_tweet_id = tau00,
      ICC = icc,
      N_tweet_id = n_distinct(model.frame(model)[["tweet_id"]]),
      Observations = nobs(model),
      Marginal_R2 = as.numeric(r2$R2_marginal),
      Conditional_R2 = as.numeric(r2$R2_conditional)
    )
  }

  stats_row_order <- c(
    "$\\sigma^2$",
    "$\\tau_{00}$\\textsubscript{tweet\\_id}",
    "ICC",
    "$N$\\textsubscript{tweet\\_id}",
    "Observations",
    "Marginal $R^2$",
    "Conditional $R^2$"
  )

  combined_stats_table <- bind_rows(
    extract_model_stats(model_OL, "OL") %>% mutate(spec = "OL_Main"),
    extract_model_stats(model_OL_interaction, "OL") %>% mutate(spec = "OL_Interaction"),
    extract_model_stats(model_HS, "HS") %>% mutate(spec = "HS_Main"),
    extract_model_stats(model_HS_interaction, "HS") %>% mutate(spec = "HS_Interaction")
  ) %>%
    pivot_longer(
      cols = c(sigma2, tau00_tweet_id, ICC, N_tweet_id, Observations, Marginal_R2, Conditional_R2),
      names_to = "term_clean",
      values_to = "value"
    ) %>%
    mutate(
      spec = factor(spec, levels = c("OL_Main", "OL_Interaction", "HS_Main", "HS_Interaction")),
      estimate_fmt = case_when(
        term_clean %in% c("N_tweet_id", "Observations") ~ format(round(value), big.mark = ",", scientific = FALSE),
        TRUE ~ sprintf("%.3f", value)
      ),
      se_fmt = "--",
      term_clean = recode(
        term_clean,
        "sigma2" = "$\\sigma^2$",
        "tau00_tweet_id" = "$\\tau_{00}$\\textsubscript{tweet\\_id}",
        "N_tweet_id" = "$N$\\textsubscript{tweet\\_id}",
        "Marginal_R2" = "Marginal $R^2$",
        "Conditional_R2" = "Conditional $R^2$"
      )
    ) %>%
    select(term_clean, spec, estimate_fmt, se_fmt) %>%
    pivot_wider(
      names_from = spec,
      values_from = c(estimate_fmt, se_fmt),
      names_glue = "{spec}_{.value}",
      values_fill = "--"
    ) %>%
    select(
      term_clean,
      OL_Main_estimate_fmt, OL_Main_se_fmt,
      OL_Interaction_estimate_fmt, OL_Interaction_se_fmt,
      HS_Main_estimate_fmt, HS_Main_se_fmt,
      HS_Interaction_estimate_fmt, HS_Interaction_se_fmt
    )

  combined_paper_table <- bind_rows(combined_coef_table, combined_stats_table) %>%
    mutate(term_clean = factor(term_clean, levels = c(combined_row_order, stats_row_order))) %>%
    arrange(term_clean) %>%
    mutate(term_clean = as.character(term_clean))

  # Saving to disk (CSV + LaTeX) is done by the caller (see main.R), same as
  # for the wide Table 1.
  list(
    model_OL = model_OL,
    model_HS = model_HS,
    fixed_table = fixed_table,
    fixed_table_full = fixed_table_full,
    paper_table = paper_table,
    model_OL_interaction = model_OL_interaction,
    model_HS_interaction = model_HS_interaction,
    interaction_fixed_table = interaction_fixed_table,
    interaction_paper_table = interaction_paper_table,
    combined_paper_table = combined_paper_table,
    reference_model = reference_model,
    reference_label = reference_label
  )
}
