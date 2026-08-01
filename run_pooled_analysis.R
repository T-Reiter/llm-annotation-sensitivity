run_pooled_analysis <- function(
    model_dfs,
    model_labels,
    reference_model = "gpt4o_mini",
    family = c("gaussian", "binomial"),
    n_cores = min(4, max(1, parallel::detectCores() - 1)),
    glmer_optCtrl = list(maxit = 200, factr = 1e8)
) {

  family <- match.arg(family)

  library(tidyverse)
  library(stringr)
  library(lme4)
  library(lmerTest)
  library(broom.mixed)
  library(performance)

  glmer_ctrl <- NULL
  if (family == "binomial") {
    library(optimParallel)
    cl <- parallel::makeCluster(n_cores)
    parallel::setDefaultCluster(cl)
    on.exit({
      parallel::setDefaultCluster(NULL)
      parallel::stopCluster(cl)
    }, add = TRUE)

    glmer_ctrl <- glmerControl(
      optimizer   = "optimParallel",
      calc.derivs = FALSE,
      optCtrl     = glmer_optCtrl
    )
  }

  model_keys <- c(reference_model, setdiff(names(model_dfs), reference_model))

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

  df_all <- df_all %>%
    mutate(model = factor(model, levels = model_keys, labels = model_labels[model_keys]))

  # task is coded as ol_first/hs_first (vs. Separate Labeling) throughout, so all specs
  # below share one representation: an intercept for Separate Labeling plus deviations.
  make_task_df <- function(outcome) {
    drop_task <- if (outcome == "OL") "C\\.HS" else "C\\.OL"

    df_all %>%
      filter(!grepl(drop_task, condition)) %>%
      mutate(
        task = factor(
          task,
          levels = if (outcome == "OL") c("A", "B", "C.OL") else c("A", "B", "C.HS"),
          labels = c("Joint: OL first", "Joint: HS first", "Separate Labeling")
        ),
        ol_first = as.integer(task == "Joint: OL first"),
        hs_first = as.integer(task == "Joint: HS first")
      )
  }

  fit_no_model <- function(outcome) {

    df_out <- make_task_df(outcome) %>%
      mutate(batch = factor(batch), conf = factor(conf))

    fml <- as.formula(paste0(outcome, " ~ ol_first + hs_first + batch + conf + (1 | tweet_id)"))
    if (family == "binomial") {
      glmer(fml, data = df_out, family = binomial(link = "logit"), verbose = 1, control = glmer_ctrl)
    } else {
      lmer(fml, data = df_out, verbose = 1)
    }
  }

  fit_one <- function(outcome) {

    df_out <- make_task_df(outcome) %>%
      mutate(batch = factor(batch), conf = factor(conf))

    fml <- as.formula(paste0(outcome, " ~ ol_first + hs_first + batch + conf + model + (1 | tweet_id)"))
    if (family == "binomial") {
      glmer(fml, data = df_out, family = binomial(link = "logit"), verbose = 1, control = glmer_ctrl)
    } else {
      lmer(fml, data = df_out, verbose = 1)
    }
  }

  model_OL_nomodel <- fit_no_model("OL")
  model_HS_nomodel <- fit_no_model("HS")

  model_OL <- fit_one("OL")
  model_HS <- fit_one("HS")

  reference_label <- model_labels[[reference_model]]

  # shared by NoModel/Main/Interaction: all three code batch/conf as factors and
  # task as intercept + ol_first/hs_first; the ":model" case only fires for Interaction
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
          term == "(Intercept)" ~ "Separate Labeling",
          term == "ol_first"    ~ "Joint: OL first (vs. Separate Labeling)",
          term == "hs_first"    ~ "Joint: HS first (vs. Separate Labeling)",
          term == "batch1"      ~ "Batch Prompt (vs. Indiv. Prompts)",
          term == "conf1"       ~ "With Confidence (vs. Without)",
          str_detect(term, ":model") ~ paste0(
            str_remove(term, "^(ol_first|hs_first):model"), " x Asked First"
          ),
          str_starts(term, "model") ~ str_remove(term, "^model"),
          TRUE ~ term
        )
      ) %>%
      select(outcome, term_clean, estimate, std.error, statistic, p.value, stars)
  }

  extract_model_stats <- function(model, outcome) {

    vc <- as.data.frame(VarCorr(model))
    sigma2 <- if (family == "binomial") (pi^2) / 3 else sigma(model)^2
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
      Conditional_R2 = as.numeric(r2$R2_conditional),
      logLik = as.numeric(logLik(model)),
      AIC = AIC(model),
      BIC = BIC(model)
    )
  }

  nomodel_fixed_table <- bind_rows(
    extract_fixed(model_OL_nomodel, "OL"),
    extract_fixed(model_HS_nomodel, "HS")
  )

  fixed_table <- bind_rows(
    extract_fixed(model_OL, "OL"),
    extract_fixed(model_HS, "HS")
  )

  model_level_labels <- unname(model_labels[setdiff(model_keys, reference_model)])

  row_order <- c(
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)",
    "With Confidence (vs. Without)",
    "Batch Prompt (vs. Indiv. Prompts)",
    model_level_labels
  )

  paper_table <- fixed_table %>%
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

  # interaction model is a heterogeneity test superseded by random slopes for the LPM;
  # for logit it's skipped entirely (not just unreported) to save fit time
  model_OL_interaction <- NULL
  model_HS_interaction <- NULL
  interaction_fixed_table <- NULL
  interaction_paper_table <- NULL

  if (family == "gaussian") {

    fit_interaction <- function(outcome) {
      interaction_var <- if (outcome == "OL") "ol_first" else "hs_first"

      df_out <- make_task_df(outcome) %>%
        mutate(batch = factor(batch), conf = factor(conf))

      fml <- as.formula(paste0(
        outcome, " ~ ol_first + hs_first + batch + conf + model + ",
        interaction_var, ":model + (1 | tweet_id)"
      ))
      lmer(fml, data = df_out, verbose = 1)
    }

    model_OL_interaction <- fit_interaction("OL")
    model_HS_interaction <- fit_interaction("HS")

    interaction_fixed_table <- bind_rows(
      extract_fixed(model_OL_interaction, "OL"),
      extract_fixed(model_HS_interaction, "HS")
    )

    interaction_row_order <- c(
      "Separate Labeling",
      "Joint: OL first (vs. Separate Labeling)",
      "Joint: HS first (vs. Separate Labeling)",
      "With Confidence (vs. Without)",
      "Batch Prompt (vs. Indiv. Prompts)",
      model_level_labels,
      paste0(model_level_labels, " x Asked First")
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
  }

  # combined_paper_table has 3 spec columns (NoModel/Main/Interaction) for the LPM,
  # 2 (NoModel/Main) for logit, since the interaction model is gaussian-only above
  if (family == "gaussian") {
    combined_row_order <- c(
      "Separate Labeling",
      "Joint: OL first (vs. Separate Labeling)",
      "Joint: HS first (vs. Separate Labeling)",
      "With Confidence (vs. Without)",
      "Batch Prompt (vs. Indiv. Prompts)",
      model_level_labels,
      paste0(model_level_labels, " x Asked First")
    )

    spec_levels <- c("OL_NoModel", "OL_Main", "OL_Interaction", "HS_NoModel", "HS_Main", "HS_Interaction")
    spec_cols <- c(
      "OL_NoModel_estimate_fmt", "OL_NoModel_se_fmt",
      "OL_Main_estimate_fmt", "OL_Main_se_fmt",
      "OL_Interaction_estimate_fmt", "OL_Interaction_se_fmt",
      "HS_NoModel_estimate_fmt", "HS_NoModel_se_fmt",
      "HS_Main_estimate_fmt", "HS_Main_se_fmt",
      "HS_Interaction_estimate_fmt", "HS_Interaction_se_fmt"
    )

    combined_long <- bind_rows(
      nomodel_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_NoModel"),
      fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Main"),
      interaction_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Interaction"),
      nomodel_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_NoModel"),
      fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Main"),
      interaction_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Interaction")
    )

    stats_models <- bind_rows(
      extract_model_stats(model_OL_nomodel, "OL") %>% mutate(spec = "OL_NoModel"),
      extract_model_stats(model_OL, "OL") %>% mutate(spec = "OL_Main"),
      extract_model_stats(model_OL_interaction, "OL") %>% mutate(spec = "OL_Interaction"),
      extract_model_stats(model_HS_nomodel, "HS") %>% mutate(spec = "HS_NoModel"),
      extract_model_stats(model_HS, "HS") %>% mutate(spec = "HS_Main"),
      extract_model_stats(model_HS_interaction, "HS") %>% mutate(spec = "HS_Interaction")
    )
  } else {
    combined_row_order <- c(
      "Separate Labeling",
      "Joint: OL first (vs. Separate Labeling)",
      "Joint: HS first (vs. Separate Labeling)",
      "With Confidence (vs. Without)",
      "Batch Prompt (vs. Indiv. Prompts)",
      model_level_labels
    )

    spec_levels <- c("OL_NoModel", "OL_Main", "HS_NoModel", "HS_Main")
    spec_cols <- c(
      "OL_NoModel_estimate_fmt", "OL_NoModel_se_fmt",
      "OL_Main_estimate_fmt", "OL_Main_se_fmt",
      "HS_NoModel_estimate_fmt", "HS_NoModel_se_fmt",
      "HS_Main_estimate_fmt", "HS_Main_se_fmt"
    )

    combined_long <- bind_rows(
      nomodel_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_NoModel"),
      fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Main"),
      nomodel_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_NoModel"),
      fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Main")
    )

    stats_models <- bind_rows(
      extract_model_stats(model_OL_nomodel, "OL") %>% mutate(spec = "OL_NoModel"),
      extract_model_stats(model_OL, "OL") %>% mutate(spec = "OL_Main"),
      extract_model_stats(model_HS_nomodel, "HS") %>% mutate(spec = "HS_NoModel"),
      extract_model_stats(model_HS, "HS") %>% mutate(spec = "HS_Main")
    )
  }

  combined_long <- combined_long %>%
    mutate(
      spec = factor(spec, levels = spec_levels),
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
    select(term_clean, all_of(spec_cols))

  stats_row_order <- c(
    "$\\sigma^2$",
    "$\\tau_{00}$\\textsubscript{tweet\\_id}",
    "ICC",
    "$N$\\textsubscript{tweet\\_id}",
    "Observations",
    "Marginal $R^2$",
    "Conditional $R^2$",
    "logLik",
    "AIC",
    "BIC"
  )

  combined_stats_table <- stats_models %>%
    pivot_longer(
      cols = c(sigma2, tau00_tweet_id, ICC, N_tweet_id, Observations, Marginal_R2, Conditional_R2, logLik, AIC, BIC),
      names_to = "term_clean",
      values_to = "value"
    ) %>%
    mutate(
      spec = factor(spec, levels = spec_levels),
      estimate_fmt = case_when(
        term_clean %in% c("N_tweet_id", "Observations") ~ format(round(value), big.mark = ",", scientific = FALSE),
        term_clean %in% c("logLik", "AIC", "BIC") ~ sprintf("%.1f", value),
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
    select(term_clean, all_of(spec_cols))

  combined_paper_table <- bind_rows(combined_coef_table, combined_stats_table) %>%
    mutate(term_clean = factor(term_clean, levels = c(combined_row_order, stats_row_order))) %>%
    arrange(term_clean) %>%
    mutate(term_clean = as.character(term_clean))

  model_OL_randomslopes <- NULL
  model_HS_randomslopes <- NULL
  combined_paper_table_randomslopes <- NULL

  if (family == "gaussian") {

    fit_random_slopes <- function(outcome) {

      df_out <- make_task_df(outcome)

      # `model` is both a fixed effect (its own average level, like Main) and the random-slopes
      # grouping factor; the random term is slopes-only (0 +, no intercept) so it isn't redundant
      # with the fixed model effect - it captures how much each model's own slope deviates,
      # net of that model's average level.
      fml <- as.formula(paste0(
        outcome, " ~ ol_first + hs_first + batch + conf + model + ",
        "(0 + ol_first + hs_first + batch + conf || model) + (1 | tweet_id)"
      ))

      lmer(fml, data = df_out, verbose = 1)
    }

    model_OL_randomslopes <- fit_random_slopes("OL")
    model_HS_randomslopes <- fit_random_slopes("HS")

    # random-slopes keeps batch/conf numeric (0/1), so its terms are "batch"/"conf", not "batch1"/"conf1"
    extract_fixed_randomslopes <- function(model, outcome) {
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
            term == "batch"       ~ "Batch Prompt (vs. Indiv. Prompts)",
            term == "conf"        ~ "With Confidence (vs. Without)",
            str_starts(term, "model") ~ str_remove(term, "^model"),
            TRUE ~ term
          )
        ) %>%
        select(outcome, term_clean, estimate, std.error, statistic, p.value, stars)
    }

    randomslopes_fixed_table <- bind_rows(
      extract_fixed_randomslopes(model_OL_randomslopes, "OL"),
      extract_fixed_randomslopes(model_HS_randomslopes, "HS")
    )

    combined_row_order_rs <- c(
      "Separate Labeling",
      "Joint: OL first (vs. Separate Labeling)",
      "Joint: HS first (vs. Separate Labeling)",
      "With Confidence (vs. Without)",
      "Batch Prompt (vs. Indiv. Prompts)",
      model_level_labels
    )

    spec_levels_rs <- c("OL_NoModel", "OL_Main", "OL_RandomSlopes", "HS_NoModel", "HS_Main", "HS_RandomSlopes")
    spec_cols_rs <- c(
      "OL_NoModel_estimate_fmt", "OL_NoModel_se_fmt",
      "OL_Main_estimate_fmt", "OL_Main_se_fmt",
      "OL_RandomSlopes_estimate_fmt", "OL_RandomSlopes_se_fmt",
      "HS_NoModel_estimate_fmt", "HS_NoModel_se_fmt",
      "HS_Main_estimate_fmt", "HS_Main_se_fmt",
      "HS_RandomSlopes_estimate_fmt", "HS_RandomSlopes_se_fmt"
    )

    combined_long_rs <- bind_rows(
      nomodel_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_NoModel"),
      fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_Main"),
      randomslopes_fixed_table %>% filter(outcome == "OL") %>% mutate(spec = "OL_RandomSlopes"),
      nomodel_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_NoModel"),
      fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_Main"),
      randomslopes_fixed_table %>% filter(outcome == "HS") %>% mutate(spec = "HS_RandomSlopes")
    ) %>%
      mutate(
        spec = factor(spec, levels = spec_levels_rs),
        estimate_fmt = ifelse(
          is.na(std.error),
          sprintf("%.3f", estimate),
          sprintf("%.3f%s", estimate, stars)
        ),
        se_fmt = ifelse(is.na(std.error), "--", sprintf("%.3f", std.error))
      )

    combined_coef_table_rs <- combined_long_rs %>%
      select(term_clean, spec, estimate_fmt, se_fmt) %>%
      pivot_wider(
        names_from = spec,
        values_from = c(estimate_fmt, se_fmt),
        names_glue = "{spec}_{.value}",
        values_fill = "--"
      ) %>%
      select(term_clean, all_of(spec_cols_rs))

    combined_stats_table_rs <- bind_rows(
      extract_model_stats(model_OL_nomodel, "OL") %>% mutate(spec = "OL_NoModel"),
      extract_model_stats(model_OL, "OL") %>% mutate(spec = "OL_Main"),
      extract_model_stats(model_OL_randomslopes, "OL") %>% mutate(spec = "OL_RandomSlopes"),
      extract_model_stats(model_HS_nomodel, "HS") %>% mutate(spec = "HS_NoModel"),
      extract_model_stats(model_HS, "HS") %>% mutate(spec = "HS_Main"),
      extract_model_stats(model_HS_randomslopes, "HS") %>% mutate(spec = "HS_RandomSlopes")
    ) %>%
      pivot_longer(
        cols = c(sigma2, tau00_tweet_id, ICC, N_tweet_id, Observations, Marginal_R2, Conditional_R2, logLik, AIC, BIC),
        names_to = "term_clean",
        values_to = "value"
      ) %>%
      mutate(
        spec = factor(spec, levels = spec_levels_rs),
        estimate_fmt = case_when(
          term_clean %in% c("N_tweet_id", "Observations") ~ format(round(value), big.mark = ",", scientific = FALSE),
          term_clean %in% c("logLik", "AIC", "BIC") ~ sprintf("%.1f", value),
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
      select(term_clean, all_of(spec_cols_rs))

    # 4 uncorrelated random-slope variance components (across `model`); no random intercept
    # since the model-level average is already captured by the fixed `model` effect above
    variance_row_order <- c(
      "Var: Joint OL first",
      "Var: Joint HS first",
      "Var: Batch Prompt",
      "Var: With Confidence"
    )

    extract_variance_components <- function(model, outcome) {
      as.data.frame(VarCorr(model)) %>%
        filter(str_starts(grp, "model"), is.na(var2)) %>%
        transmute(
          outcome = outcome,
          term_clean = recode(
            var1,
            "ol_first"    = "Var: Joint OL first",
            "hs_first"    = "Var: Joint HS first",
            "batch"       = "Var: Batch Prompt",
            "conf"        = "Var: With Confidence"
          ),
          estimate_fmt = sprintf("%.4f", vcov),
          se_fmt       = sprintf("%.4f", sqrt(vcov))
        )
    }

    variance_table_rs <- bind_rows(
      extract_variance_components(model_OL_randomslopes, "OL") %>% mutate(spec = "OL_RandomSlopes"),
      extract_variance_components(model_HS_randomslopes, "HS") %>% mutate(spec = "HS_RandomSlopes")
    ) %>%
      select(term_clean, spec, estimate_fmt, se_fmt) %>%
      pivot_wider(
        names_from = spec,
        values_from = c(estimate_fmt, se_fmt),
        names_glue = "{spec}_{.value}"
      ) %>%
      mutate(
        OL_NoModel_estimate_fmt = "--", OL_NoModel_se_fmt = "--",
        OL_Main_estimate_fmt = "--", OL_Main_se_fmt = "--",
        HS_NoModel_estimate_fmt = "--", HS_NoModel_se_fmt = "--",
        HS_Main_estimate_fmt = "--", HS_Main_se_fmt = "--"
      ) %>%
      select(term_clean, all_of(spec_cols_rs))

    # LRT of RandomSlopes vs. Main: since the two share the exact same fixed-effects
    # formula and differ only in the random-effects structure (the 4 uncorrelated
    # model-level slopes), the REML fits are directly comparable and refit=FALSE
    # avoids the (here unnecessary) cost of refitting both models under ML.
    extract_lrt_vs_main <- function(model_main, model_randomslopes, outcome) {
      lrt <- suppressMessages(anova(model_main, model_randomslopes, refit = FALSE))
      tibble(
        outcome = outcome,
        chisq = lrt$Chisq[2],
        df    = lrt$Df[2],
        p     = lrt[["Pr(>Chisq)"]][2]
      )
    }

    lrt_table_rs <- bind_rows(
      extract_lrt_vs_main(model_OL, model_OL_randomslopes, "OL"),
      extract_lrt_vs_main(model_HS, model_HS_randomslopes, "HS")
    ) %>%
      mutate(
        chisq_fmt = sprintf("%.2f", chisq),
        p_fmt     = ifelse(p < .001, "<.001", sprintf("%.3f", p))
      )

    # df is a structural property of the two nested formulas (4 uncorrelated random-slope
    # variances added), so it's identical for OL/HS and gets folded into the chi-sq row label
    # rather than reported as its own line.
    stopifnot(length(unique(lrt_table_rs$df)) == 1)
    lrt_chisq_label <- paste0("LRT $\\chi^2_{", unique(lrt_table_rs$df), "}$ (vs. Models)")

    lrt_row_order <- c(
      lrt_chisq_label,
      "LRT $p$"
    )

    lrt_long_rs <- bind_rows(
      lrt_table_rs %>% transmute(outcome, term_clean = lrt_chisq_label, estimate_fmt = chisq_fmt),
      lrt_table_rs %>% transmute(outcome, term_clean = "LRT $p$",       estimate_fmt = p_fmt)
    ) %>%
      mutate(spec = paste0(outcome, "_RandomSlopes"), se_fmt = "--")

    lrt_table_rs_wide <- lrt_long_rs %>%
      select(term_clean, spec, estimate_fmt, se_fmt) %>%
      pivot_wider(
        names_from = spec,
        values_from = c(estimate_fmt, se_fmt),
        names_glue = "{spec}_{.value}"
      ) %>%
      mutate(
        OL_NoModel_estimate_fmt = "--", OL_NoModel_se_fmt = "--",
        OL_Main_estimate_fmt = "--", OL_Main_se_fmt = "--",
        HS_NoModel_estimate_fmt = "--", HS_NoModel_se_fmt = "--",
        HS_Main_estimate_fmt = "--", HS_Main_se_fmt = "--"
      ) %>%
      select(term_clean, all_of(spec_cols_rs))

    combined_paper_table_randomslopes <- bind_rows(
      combined_coef_table_rs, combined_stats_table_rs, variance_table_rs, lrt_table_rs_wide
    ) %>%
      mutate(term_clean = factor(term_clean, levels = c(combined_row_order_rs, stats_row_order, variance_row_order, lrt_row_order))) %>%
      arrange(term_clean) %>%
      mutate(term_clean = as.character(term_clean))
  }

  list(
    model_OL_nomodel = model_OL_nomodel,
    model_HS_nomodel = model_HS_nomodel,
    nomodel_fixed_table = nomodel_fixed_table,
    model_OL = model_OL,
    model_HS = model_HS,
    fixed_table = fixed_table,
    paper_table = paper_table,
    model_OL_interaction = model_OL_interaction,
    model_HS_interaction = model_HS_interaction,
    interaction_fixed_table = interaction_fixed_table,
    interaction_paper_table = interaction_paper_table,
    combined_paper_table = combined_paper_table,
    model_OL_randomslopes = model_OL_randomslopes,
    model_HS_randomslopes = model_HS_randomslopes,
    combined_paper_table_randomslopes = combined_paper_table_randomslopes,
    model_level_labels = model_level_labels,
    reference_model = reference_model,
    reference_label = reference_label
  )
}

combined_pooled_table_to_latex <- function(combined, model_level_labels) {

  library(tidyverse)

  cols <- c(
    "OL_NoModel_estimate_fmt", "OL_Main_estimate_fmt", "OL_Interaction_estimate_fmt",
    "HS_NoModel_estimate_fmt", "HS_Main_estimate_fmt", "HS_Interaction_estimate_fmt"
  )

  row_fmt <- function(term) {
    i <- match(term, combined$term_clean)
    vals <- unlist(combined[i, cols])
    vals <- ifelse(is.na(vals), "--", vals)
    paste(c(term, vals), collapse = " & ")
  }

  subheader <- function(label) paste0("\\multicolumn{7}{l}{\\textbf{", label, "}} \\\\")

  task_terms <- c(
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)"
  )
  design_terms <- c("With Confidence (vs. Without)", "Batch Prompt (vs. Indiv. Prompts)")
  model_terms     <- model_level_labels
  model_int_terms <- paste0(model_level_labels, " x Asked First")
  random_effect_terms <- c("$\\tau_{00}$\\textsubscript{tweet\\_id}")
  model_stats_terms   <- c("ICC", "Marginal $R^2$", "Conditional $R^2$", "AIC", "BIC")

  body <- c(
    subheader("Task Structure Effects"),
    paste0(map_chr(task_terms, row_fmt), " \\\\"),
    paste0(map_chr(design_terms, row_fmt), " \\\\"),
    subheader("Model Fixed Effects"),
    paste0(map_chr(model_terms, row_fmt), " \\\\"),
    subheader("Model $\\times$ Task Structure Interaction"),
    paste0(map_chr(model_int_terms, row_fmt), " \\\\"),
    subheader("Random Effects"),
    paste0(map_chr(random_effect_terms, row_fmt), " \\\\"),
    "\\midrule",
    paste0(map_chr(model_stats_terms, row_fmt), " \\\\")
  )

  paste(body, collapse = "\n")
}

combined_pooled_table_to_standalone_tex <- function(combined, model_level_labels, reference_label = NULL) {

  body_rows <- combined_pooled_table_to_latex(combined, model_level_labels)

  header <- paste(
    c("Term", "OL Task", "OL Models", "OL Interaction", "HS Task", "HS Models", "HS Interaction"),
    collapse = " & "
  )

  n_tweet <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "$N$\\textsubscript{tweet\\_id}"])
  obs     <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "Observations"])

  note <- c(
    "\\smallskip",
    paste0(
      "\\footnotesize \\textit{Note}: All models: $N$\\textsubscript{tweet\\_id} = ", n_tweet,
      "; Observations = ", obs, ". *** $p<.001$, ** $p<.01$, * $p<.05$. ",
      "All specifications include an intercept, with Separate Labeling as the reference task-structure category. ",
      "Task omits the per-model fixed effect (pooled average, ignoring model identity)."
    )
  )

  if (!is.null(reference_label)) {
    note <- c(
      note,
      paste0(
        "\\footnotesize \"Model:\" rows are estimated relative to ",
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
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    paste0(header, " \\\\"),
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

combined_pooled_table_randomslopes_to_latex <- function(combined, model_level_labels) {

  library(tidyverse)

  cols <- c(
    "OL_NoModel_estimate_fmt", "OL_Main_estimate_fmt", "OL_RandomSlopes_estimate_fmt",
    "HS_NoModel_estimate_fmt", "HS_Main_estimate_fmt", "HS_RandomSlopes_estimate_fmt"
  )

  row_fmt <- function(term) {
    i <- match(term, combined$term_clean)
    vals <- unlist(combined[i, cols])
    vals <- ifelse(is.na(vals), "--", vals)
    paste(c(term, vals), collapse = " & ")
  }

  subheader <- function(label) paste0("\\multicolumn{7}{l}{\\textbf{", label, "}} \\\\")

  task_terms <- c(
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)"
  )
  design_terms   <- c("With Confidence (vs. Without)", "Batch Prompt (vs. Indiv. Prompts)")
  model_terms    <- model_level_labels
  variance_terms <- combined$term_clean[str_starts(combined$term_clean, "Var: ")]
  random_effect_terms <- c("$\\tau_{00}$\\textsubscript{tweet\\_id}")
  model_stats_terms   <- c("ICC", "Marginal $R^2$", "Conditional $R^2$", "AIC", "BIC")
  lrt_terms <- combined$term_clean[str_starts(combined$term_clean, "LRT")]

  body <- c(
    subheader("Task Structure Effects"),
    paste0(map_chr(task_terms, row_fmt), " \\\\"),
    paste0(map_chr(design_terms, row_fmt), " \\\\"),
    subheader("Model Fixed Effects"),
    paste0(map_chr(model_terms, row_fmt), " \\\\"),
    subheader("Cross-Model Variance Components"),
    paste0(map_chr(variance_terms, row_fmt), " \\\\"),
    subheader("Random Effects"),
    paste0(map_chr(random_effect_terms, row_fmt), " \\\\"),
    "\\midrule",
    paste0(map_chr(model_stats_terms, row_fmt), " \\\\"),
    paste0(map_chr(lrt_terms, row_fmt), " \\\\")
  )

  paste(body, collapse = "\n")
}

combined_pooled_table_randomslopes_to_standalone_tex <- function(combined, model_level_labels, reference_label = NULL) {

  body_rows <- combined_pooled_table_randomslopes_to_latex(combined, model_level_labels)

  header <- paste(
    c("Term", "OL Task", "OL Models", "OL Random Slopes", "HS Task", "HS Models", "HS Random Slopes"),
    collapse = " & "
  )

  n_tweet <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "$N$\\textsubscript{tweet\\_id}"])
  obs     <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "Observations"])

  note <- c(
    "\\smallskip",
    paste0(
      "\\footnotesize \\textit{Note}: All models: $N$\\textsubscript{tweet\\_id} = ", n_tweet,
      "; Observations = ", obs, ". *** $p<.001$, ** $p<.01$, * $p<.05$. ",
      "All specifications include an intercept, with Separate Labeling as the reference task-structure category. ",
      "Task omits the per-model fixed effect (pooled average, ignoring model identity); ",
      "Random Slopes adds the same per-model fixed effect as Models plus uncorrelated random slopes for task structure, batch, and confidence across models. ",
      "LRT tests Random Slopes against Models (same fixed effects, REML)."
    )
  )

  if (!is.null(reference_label)) {
    note <- c(
      note,
      paste0(
        "\\footnotesize \"Model:\" rows (Models and Random Slopes columns) are estimated relative to ",
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
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    paste0(header, " \\\\"),
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

# 2-spec (NoModel/Main) version for combined_paper_table when family = "binomial",
# since the interaction model is skipped entirely for logit.
combined_pooled_table_logit_to_latex <- function(combined, model_level_labels) {

  library(tidyverse)

  cols <- c(
    "OL_NoModel_estimate_fmt", "OL_Main_estimate_fmt",
    "HS_NoModel_estimate_fmt", "HS_Main_estimate_fmt"
  )

  row_fmt <- function(term) {
    i <- match(term, combined$term_clean)
    vals <- unlist(combined[i, cols])
    vals <- ifelse(is.na(vals), "--", vals)
    paste(c(term, vals), collapse = " & ")
  }

  subheader <- function(label) paste0("\\multicolumn{5}{l}{\\textbf{", label, "}} \\\\")

  task_terms <- c(
    "Separate Labeling",
    "Joint: OL first (vs. Separate Labeling)",
    "Joint: HS first (vs. Separate Labeling)"
  )
  design_terms <- c("With Confidence (vs. Without)", "Batch Prompt (vs. Indiv. Prompts)")
  model_terms  <- model_level_labels
  random_effect_terms <- c("$\\tau_{00}$\\textsubscript{tweet\\_id}")
  model_stats_terms   <- c("ICC", "Marginal $R^2$", "Conditional $R^2$", "AIC", "BIC")

  body <- c(
    subheader("Task Structure Effects"),
    paste0(map_chr(task_terms, row_fmt), " \\\\"),
    paste0(map_chr(design_terms, row_fmt), " \\\\"),
    subheader("Model Fixed Effects"),
    paste0(map_chr(model_terms, row_fmt), " \\\\"),
    subheader("Random Effects"),
    paste0(map_chr(random_effect_terms, row_fmt), " \\\\"),
    "\\midrule",
    paste0(map_chr(model_stats_terms, row_fmt), " \\\\")
  )

  paste(body, collapse = "\n")
}

combined_pooled_table_logit_to_standalone_tex <- function(combined, model_level_labels, reference_label = NULL) {

  body_rows <- combined_pooled_table_logit_to_latex(combined, model_level_labels)

  header <- paste(
    c("Term", "OL Task", "OL Models", "HS Task", "HS Models"),
    collapse = " & "
  )

  n_tweet <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "$N$\\textsubscript{tweet\\_id}"])
  obs     <- trimws(combined$OL_Main_estimate_fmt[combined$term_clean == "Observations"])

  note <- c(
    "\\smallskip",
    paste0(
      "\\footnotesize \\textit{Note}: All models: $N$\\textsubscript{tweet\\_id} = ", n_tweet,
      "; Observations = ", obs, ". *** $p<.001$, ** $p<.01$, * $p<.05$. ",
      "All specifications include an intercept, with Separate Labeling as the reference task-structure category. ",
      "Task omits the per-model fixed effect (pooled average, ignoring model identity)."
    )
  )

  if (!is.null(reference_label)) {
    note <- c(
      note,
      paste0(
        "\\footnotesize \"Model:\" rows are estimated relative to ",
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
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    paste0(header, " \\\\"),
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
