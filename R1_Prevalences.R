
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # wd = source loc
################################################################################
### Load the Kern Original Data
################################################################################
folder_path <- "Tweets_CK"
train_file  <- "full_train_s.csv"
test_file   <- "full_test_s.csv"

train_df <- read_csv(file.path(folder_path, train_file))
test_df  <- read_csv(file.path(folder_path, test_file))


# reorder test_df columns to match train_df
test_df <- test_df %>% select(all_of(names(train_df)))

# add the 'original_split' column
train_df <- train_df %>% mutate(original_split = "train")
test_df  <- test_df  %>% mutate(original_split = "test")

# combine
tweets_full <- bind_rows(train_df, test_df)

# consistent naming
tweets_full <- tweets_full %>%
  rename(
    tweet_id           = `tweet.id`,
    batch_tweet        = `batch.tweet`,
    hate_speech        = `hate.speech`,
    offensive_language = `offensive.language`
  )


################################################################################
### Reproduce the Kern Table
################################################################################
summary <- tweets_full %>%
  group_by(version) %>%
  summarise(
    Annotations = n(),                     # count of tweets
    Annotators  = n_distinct(id),          # number of unique annotators
    OL          = mean(offensive_language, na.rm = T),
    HS          = mean(hate_speech, na.rm = T)
  ) %>%
  mutate(
    across(c(OL, HS), ~ round(.x, 3))      # round to 3 decimals
  )

total_annotations <- sum(summary$Annotations)
total_annotators  <- sum(summary$Annotators)
weighted_OL       <- sum(summary$OL * summary$Annotations) / total_annotations
weighted_HS       <- sum(summary$HS * summary$Annotations) / total_annotations

overall <- tibble(
  version     = "overall",
  Annotations = total_annotations,
  Annotators  = total_annotators,
  OL          = round(weighted_OL, 3),
  HS          = round(weighted_HS, 3)
)

final_summary <- bind_rows(summary, overall)
print(final_summary)


################################################################################
### Load and RBind the GPT Labelled Tweets
################################################################################

# zero shot
date_to_import_z <- "2025_04_10"
files_z <- list.files(
  path = "Data_Collection/",
  pattern = paste0("^", date_to_import_z),
  full.names = TRUE
)

combined_df_z <- files_z %>%
  map_df(read_csv, .id = "source") %>%     
  arrange(batch_id, tweet_in_batch) %>%
  mutate(shot = "zero-shot") %>%
  # insert 'shot' as 5th column:
  select(1:4, shot, everything())

combined_df_z$R1_score <- as.character(combined_df_z$R1_score)
combined_df_z$R2_score <- as.character(combined_df_z$R2_score)
combined_df_z$R3_score <- as.character(combined_df_z$R3_score)

# few shot
date_to_import_f <- "2025_07_01"
files_f <- list.files(
  path = "Data_Collection/OL_NH/",
  pattern = paste0("^", date_to_import_f),
  full.names = TRUE
)

combined_df_f <- files_f %>%
  map_df(read_csv, .id = "source") %>%
  arrange(batch_id, tweet_in_batch) %>%
  mutate(shot = "few-shot") %>%
  select(1:4, shot, everything())


combined_df <- bind_rows(combined_df_z, combined_df_f)

n_tweets        <- combined_df %>% pull(tweet_id) %>% unique() %>% length()
n_tweet_batches <- combined_df %>% pull(batch_id)   %>% unique() %>% length()
n_conditions    <- combined_df %>% pull(condition)  %>% unique() %>% length()
cat("Unique Tweets: ", n_tweets,        "\n")
cat("No. Batches:   ", n_tweet_batches, "\n")
cat("Conditions:    ", n_conditions,    "\n")



################################################################################
### Preprocess GPT Labels
################################################################################
shared_cols <- c(
  "batch_id", "tweet_in_batch", "tweet_id", 
  "tweet", "condition", "shot"
)

# wide to long
df_long <- combined_df %>%
  # keep only shared cols + all R*- columns
  select(batch_id, tweet_in_batch, tweet_id, tweet, condition, shot, starts_with("R")) %>%
  # split names like "R1_label" into responder = "R1" and .value = "label"
  pivot_longer(
    cols      = starts_with("R"),
    names_to  = c("responder", ".value"),
    names_sep = "_"
  )

# OL / HS label cols
library(stringr)
df_long <- df_long %>%
  mutate(
    OL = as.integer(str_detect(label, "(?<!N)OL")),
    HS = as.integer(str_detect(label, "(?<!N)HS"))
  )



################################################################################
### Compute and Compare Overall Prevalences
################################################################################

tasks        <- c("A", "B", "C.OL", "C.HS")
modes        <- c("", "_conf", "_batch", "_batch_conf")
custom_order <- as.vector(outer(tasks, modes, paste0))
shot_order   <- c("zero-shot", "few-shot")

df_long <- df_long %>%
  mutate(
    condition = factor(condition, levels = custom_order),
    shot      = factor(shot,      levels = shot_order)
  )

summary_tbl <- df_long %>%
  group_by(condition, shot) %>%
  summarize(
    OL = round(mean(OL, na.rm = TRUE), 2),
    HS = round(mean(HS, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(condition, shot)

summary_tbl



################################################################################
### Plot Prevalences
################################################################################
overall_OL <- final_summary %>% 
  filter(version == "overall") %>% 
  pull(OL)
overall_HS <- final_summary %>% 
  filter(version == "overall") %>% 
  pull(HS)

shot_order  <- c("zero-shot", "few-shot")
group_order <- c("base", "conf", "batch", "batch_conf")

# custom_order for the x-axis:
tasks        <- c("A", "B", "C.OL", "C.HS")
modes        <- c("", "_conf", "_batch", "_batch_conf")
custom_order <- as.vector(outer(tasks, modes, paste0))

# build long format df for plotting
df_plot <- df_long %>%
  # derive base_condition
  mutate(
    base_condition = word(condition, 1, sep = "_"),
    # derive group_type
    group_type = case_when(
      !str_detect(condition, "_")            ~ "base",
      str_detect(condition, "_batch_conf$")  ~ "batch_conf",
      str_detect(condition, "_conf$")        ~ "conf",
      str_detect(condition, "_batch$")       ~ "batch",
      TRUE                                   ~ "other"
    )
  ) %>%
  # stack OL/HS into Measure/Value
  pivot_longer(
    cols      = c(OL, HS),
    names_to  = "Measure",
    values_to = "Value"
  ) %>%
  # enforce factor ordering for plotting
  mutate(
    condition  = factor(condition,  levels = custom_order),
    group_type = factor(group_type, levels = group_order),
    shot       = factor(shot, levels = shot_order,
                        labels = c("Proportion\n(zero-shot)",
                                   "Proportion\n(few-shot)")),
    Measure    = factor(Measure, levels = c("OL","HS"))
  )


df_prop <- df_long %>%
  # derive base_condition + group_type as before
  mutate(
    base_condition = word(condition, 1, sep = "_"),
    group_type = case_when(
      !str_detect(condition, "_")           ~ "base",
      str_detect(condition, "_batch_conf$") ~ "batch_conf",
      str_detect(condition, "_conf$")       ~ "conf",
      str_detect(condition, "_batch$")      ~ "batch",
      TRUE                                  ~ "other"
    )
  ) %>%
  # stack OL/HS into Measure/Value
  pivot_longer(
    cols      = c(OL, HS),
    names_to  = "Measure",
    values_to = "Value"
  ) %>%
  # compute the mean(Value) = proportion of 1’s per group
  group_by(condition, group_type, shot, Measure) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # enforce the same factor‐orders for plotting
  mutate(
    condition  = factor(condition,  levels = custom_order),
    group_type = factor(group_type, levels = group_order),
    shot       = factor(shot, levels = shot_order,
                        labels = c("Proportion\n(zero-shot)",
                                   "Proportion\n(few-shot)")),
    Measure    = factor(Measure, levels = c("OL","HS"))
  )

# grab the “overall” reference‐line values
overall_OL <- final_summary %>% filter(version == "overall") %>% pull(OL)
overall_HS <- final_summary %>% filter(version == "overall") %>% pull(HS)

# ------------------------------------------------------------------
# 2) now plot proportions in the same 2×4 faceted style
# ------------------------------------------------------------------
## ---- proportions plot with matching legend style --------------------------
ggplot(df_prop, aes(x = condition, y = Value, fill = Measure)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  
  ## reference lines — map colour & linetype so they appear in the legend
  geom_hline(aes(yintercept = overall_OL,
                 colour     = "OL reference",
                 linetype   = "OL reference"),
             size = 0.8) +
  geom_hline(aes(yintercept = overall_HS,
                 colour     = "HS reference",
                 linetype   = "HS reference"),
             size = 0.8) +
  
  ## facet layout differs from the first plot
  facet_grid(
    shot ~ group_type,
    scales = "free_x",
    space  = "free_x",
    switch = "y"
  ) +
  
  ## bars
  scale_fill_manual(
    name   = "Measure",
    values = c(OL = "#1f77b4", HS = "#ff7f0e")
  ) +
  
  ## reference-line legend (colour & linetype share labels)
  scale_colour_manual(
    name   = "Reference\n(Kern et al. (2023))",
    values = c("OL reference" = "#1f77b4",
               "HS reference" = "#ff7f0e")
  ) +
  scale_linetype_manual(
    name   = "Reference\n(Kern et al. (2023))",
    values = c("OL reference" = "dashed",
               "HS reference" = "dashed")
  ) +
  
  ## keep the Measure legend above the Reference legend
  guides(
    fill      = guide_legend(order = 1),
    colour    = guide_legend(order = 2),
    linetype  = guide_legend(order = 2)
  ) +
  
  ## y-axis: 0–1 with 0·25 ticks (same as first plot)
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  labs(
    x   = NULL,
    y   = NULL          # you kept it blank here
  ) +
  
  ## theme tweaks copied from the finished first plot
  theme_bw(base_size = 14) +
  theme(
    axis.text.x          = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.placement      = "outside",
    strip.background     = element_blank(),
    
    legend.position      = "right",
    legend.box           = "vertical",
    legend.text          = element_text(size = 13),
    legend.title         = element_text(size = 13, face = "bold"),
    legend.background    = element_rect(fill = "transparent", colour = "transparent"),
    
    panel.grid.major.x   = element_blank(),
    panel.grid.minor     = element_blank()
  )


# save
ggsave("Plots/R/hs_ol_descr_x_zerofew.png", last_plot(),
       dpi = 300,
       width = 24, height = 10, units = "in")


#################
### Same plot but only for few shot conditions
#################

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ------------------------------------------------------------------
# 1) build a proportions table *only* for few-shot
# ------------------------------------------------------------------
df_prop_few <- df_long %>%
  # derive base_condition + group_type as before
  mutate(
    base_condition = word(condition, 1, sep = "_"),
    group_type = case_when(
      !str_detect(condition, "_")           ~ "base",
      str_detect(condition, "_batch_conf$") ~ "batch_conf",
      str_detect(condition, "_conf$")       ~ "conf",
      str_detect(condition, "_batch$")      ~ "batch",
      TRUE                                  ~ "other"
    )
  ) %>%
  # pivot OL/HS into Measure/Value
  pivot_longer(
    cols      = c(OL, HS),
    names_to  = "Measure",
    values_to = "Value"
  ) %>%
  # compute the mean(Value) = proportion of 1’s per cell
  group_by(condition, group_type, shot, Measure) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # keep only few-shot
  filter(shot == "few-shot") %>%
  # enforce your factor orders
  mutate(
    condition  = factor(condition,  levels = custom_order),
    group_type = factor(group_type, levels = group_order),
    Measure    = factor(Measure,    levels = c("OL","HS"))
  )

# grab overall reference‐line values
overall_OL <- final_summary %>% filter(version == "overall") %>% pull(OL)
overall_HS <- final_summary %>% filter(version == "overall") %>% pull(HS)

# ------------------------------------------------------------------
# 2) plot a 1×4 grid of just the few‐shot panels
# ------------------------------------------------------------------
ggplot(df_prop_few, aes(x = condition, y = Value, fill = Measure)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = overall_OL, linetype = "dashed", color = "#1f77b4") +
  geom_hline(yintercept = overall_HS, linetype = "dashed", color = "#ff7f0e") +
  facet_grid(
    . ~ group_type,       # one row, columns = group_type
    scales = "free_x", 
    space  = "free_x"
  ) +
  scale_fill_manual(values = c(OL = "#1f77b4", HS = "#ff7f0e")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x    = NULL,
    y    = "Proportion\n(few-shot)",
    fill = "Measure"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x          = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.placement      = "outside",
    strip.background     = element_blank(),
    legend.position      = c(1, 1),
    legend.justification = c(1, 1),
    panel.grid.major.x   = element_blank(),
    panel.grid.minor     = element_blank()
  )


# save
# ggsave("Plots/hs_ol_descr_x_fewshot.png", last_plot(), 
#        dpi = 300,
#        width = 24, height = 10, units = "in")






################################################################################
### Calculate Rater Agreements 
################################################################################

library(DescTools)  
library(irr)       

tweet_level_df <- df_long %>%
  group_by(tweet_id, condition, shot, batch_id, tweet_in_batch) %>%
  summarise(
    label_count = n(),
    HS_mode     = DescTools::Mode(HS),
    HS_mean     = mean(HS, na.rm = TRUE),
    HS_std      = sd(HS,   na.rm = TRUE),
    OL_mode     = Mode(OL),
    OL_mean     = mean(OL, na.rm = TRUE),
    OL_std      = sd(OL,   na.rm = TRUE),
    .groups = "drop"
  )


# 2) Aggregate modal ratings per tweet × condition × shot
df_agg <- df_long %>%
  group_by(tweet_id, condition, shot) %>%
  summarise(
    modal_HS = Mode(HS),
    modal_OL = Mode(OL),
    .groups = "drop"
  )

# 3) Prepare lists of conditions
conditions    <- sort(unique(df_long$condition))
HS_conditions <- conditions[!grepl("C\\.OL", conditions)]
OL_conditions <- conditions[!grepl("C\\.HS", conditions)]

# 4) Initialize empty agreement matrices
across_HS_zero <- matrix(
  NA_real_,
  nrow = length(HS_conditions),
  ncol = length(HS_conditions),
  dimnames = list(HS_conditions, HS_conditions)
)
across_HS_few <- across_HS_zero
across_OL_zero <- matrix(
  NA_real_,
  nrow = length(OL_conditions),
  ncol = length(OL_conditions),
  dimnames = list(OL_conditions, OL_conditions)
)
across_OL_few <- across_OL_zero

# 5) Fill in diagonals (Fleiss' kappa) and off-diagonals (Cohen's kappa)
#  Diagonals: Average Within Tweet Agreement within Conditions 
#             --> 3 Ratings per Tweet --> Fleiss Kappa
#  Off-Diagonals: Average agreement of Modal Ratings per Tweet across
#             --> 1 Modal Rating per Condition per Tweet --> Cohens Kappa
for (shot_type in c("zero-shot", "few-shot")) {
  for (cond_i in conditions) {
    # 5a) Fleiss' kappa for HS and OL in the same condition
    df_sub <- df_long %>% filter(shot == shot_type, condition == cond_i)
    # get the matrix for HS ratings
    mat_HS <- df_sub %>%
      select(tweet_id, responder, HS) %>%
      pivot_wider(names_from = responder, values_from = HS) %>%
      select(-tweet_id) %>%
      as.matrix()
    # same for OL
    mat_OL <- df_sub %>%
      select(tweet_id, responder, OL) %>%
      pivot_wider(names_from = responder, values_from = OL) %>%
      select(-tweet_id) %>%
      as.matrix()
    # calculate fleiss kappa value for the current condition
    kappa_fleiss_HS <- kappam.fleiss(mat_HS)$value
    kappa_fleiss_OL <- kappam.fleiss(mat_OL)$value
    
    # assign kappa value to the appropriate matrix
    # zero shot
    if (shot_type == "zero-shot") {
      if (cond_i %in% colnames(across_HS_zero)){
        across_HS_zero[cond_i, cond_i] <- kappa_fleiss_HS
      }
      if (cond_i %in% colnames(across_OL_zero)){
        across_OL_zero[cond_i, cond_i] <- kappa_fleiss_OL
      }
    } else {
    # few shot
      if (cond_i %in% colnames(across_HS_zero)){
        across_HS_few[cond_i, cond_i] <- kappa_fleiss_HS
      }
      if (cond_i %in% colnames(across_OL_zero)){
        across_OL_few[cond_i, cond_i] <- kappa_fleiss_OL
      }
    }
    
    # 5b) Cohen's kappa between different conditions' modal ratings
    for (cond_j in conditions) {
      if (cond_i != cond_j) {
        # HS between-conditions
        if (cond_i %in% HS_conditions && cond_j %in% HS_conditions) {
          # --> get the modal ratings per condition & tweet
          wide_HS <- df_agg %>%
            filter(shot == shot_type, condition %in% HS_conditions) %>%
            select(tweet_id, condition, modal_HS) %>%
            pivot_wider(names_from = condition, values_from = modal_HS)
          # --> select relevant cols
          pair <- wide_HS %>%
            select(all_of(c(cond_i, cond_j))) %>%
            drop_na()
          # calulcate cohens kappa
          kappa_pair <- kappa2(pair)$value 
          if (shot_type=="zero-shot") {
            across_HS_zero[cond_i, cond_j] <- kappa_pair
          } else {
            across_HS_few[cond_i, cond_j]  <- kappa_pair
          }
        }
        # OL between-conditions
        if (cond_i %in% OL_conditions && cond_j %in% OL_conditions) {
          wide_OL <- df_agg %>%
            filter(shot == shot_type, condition %in% OL_conditions) %>%
            select(tweet_id, condition, modal_OL) %>%
            pivot_wider(names_from = condition, values_from = modal_OL)
          pair_ol <- wide_OL %>%
            select(all_of(c(cond_i, cond_j))) %>%
            drop_na()
          kappa_pair_ol <-kappa2(pair_ol)$value 
          if (shot_type=="zero-shot") {
            across_OL_zero[cond_i, cond_j] <- kappa_pair_ol
          } else {
            across_OL_few[cond_i, cond_j]  <- kappa_pair_ol
          }
        }
      }
    }
  }
}

# Optionally convert to data-frames
across_HS_zero_df <- as.data.frame(across_HS_zero)
across_HS_few_df  <- as.data.frame(across_HS_few)
across_OL_zero_df <- as.data.frame(across_OL_zero)
across_OL_few_df  <- as.data.frame(across_OL_few)

# 6) Global vs. modal Cohen's kappa per condition x shot
# --> calculcate modes for the kern reference data set
global_modes <- tweets_full %>%
  group_by(tweet_id) %>%
  reframe(
    global_HS = Mode(hate_speech)[1], # select first if multiple modes
                                      # possible if a tweet was rated only 14 times
    global_OL = Mode(offensive_language)[1], 
    .groups = "drop"
  )

df_agg2 <- df_agg %>%
  left_join(global_modes, by = "tweet_id")

outcomes <- c("HS","OL")
shots    <- unique(df_agg2$shot)
conds    <- unique(df_agg2$condition)

kappa_results <- list()
for (outcome in outcomes) {
  gcol <- paste0("global_", outcome)
  mcol <- paste0("modal_",  outcome)
  kappa_results[[outcome]] <- map_dfr(shots, function(st) {
    tibble(
      condition = conds,
      kappa     = map_dbl(conds, function(cond) {
        tmp <- df_agg2 %>%
          filter(shot == st, condition == cond) %>%
          select(all_of(c(mcol, gcol))) %>%
          drop_na()
        if (nrow(tmp)>0) kappa2(tmp)$value else NA_real_
      }),
      shot = st
    )
  })
}
kappa_results

################################################################################
### Plot Agreements
################################################################################
library(tidyverse)   
library(stringr)
library(patchwork)   
library(RColorBrewer)
library(data.table)

# ── helpers ────────────────────────────────────────────────────────────────
coolwarm <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))

# prefixes / suffixes used for the ordering of conditions
prefixes  <- c("A", "B", "C")
suffixes  <- c("", "_conf", "_batch", "_batch_conf")
base_cond_order <- paste0(rep(prefixes, each = length(suffixes)), suffixes)

outcome = 'OL'
shot_code = 'few'

# build one panel (one outcome × one shot) and return long data-frame
build_panel_df <- function(outcome = c("OL", "HS"),
                           shot_code = c("zero", "few")) {
  
  outcome   <- match.arg(outcome)
  shot_code <- match.arg(shot_code)     # "zero" or "few"
  
  # 1) pull the right across-condition matrix ------------------------------
  mat <- get(paste0("across_", outcome, "_", shot_code))
  
  # 2) drop the mismatching C.<other> rows/cols -----------------------------
  other_outcome <- ifelse(outcome == "HS", "OL", "HS")
  no_cond       <- paste0("C.", other_outcome)
  keep_rows     <- !str_detect(rownames(mat), fixed(no_cond))
  keep_cols     <- !str_detect(colnames(mat), fixed(no_cond))
  mat           <- mat[keep_rows, keep_cols, drop = FALSE]
  
  # 3) strip the “.OL” / “.HS” endings -------------------------------------
  strip_pattern <- "\\.(?:OL|HS)"
  rownames(mat) <- str_remove_all(rownames(mat), strip_pattern)
  colnames(mat) <- str_remove_all(colnames(mat), strip_pattern)
  
  # 4) enforce the desired ordering ----------------------------------------
  cond_order <- base_cond_order[base_cond_order %in% rownames(mat)]
  mat        <- mat[cond_order, cond_order, drop = FALSE]
  mat_df <- as.data.frame(mat)
  
  # 5) add the white “gap” and the Global column ---------------------------
  mat_df$gap <- NaN
  
  kappa_results_select <- kappa_results[[outcome]] 
  kappa_results_select <- kappa_results_select %>% 
    filter(!grepl(other_outcome, condition))
  kappa_results_select$condition <-
    str_remove_all(kappa_results_select$condition, strip_pattern)
  kappa_results_select <- kappa_results_select %>%
    filter(shot == paste0(shot_code, "-shot"),
          condition %in% cond_order) %>%
    select(condition, kappa) %>%
    deframe() 
  kappa_select_df <- as.data.frame(kappa_results_select)
  colnames(kappa_select_df) <- 'reference_agreement'
  
  mat_df <- merge(mat_df,
                  kappa_select_df,
                  by.x = "row.names", by.y = "row.names",
                  sort = FALSE) # keep the correct order
  rownames(mat_df) <- mat_df$Row.names    
  mat_df$Row.names <- NULL 
  
  # 6) long format + upper-triangle mask -----------------------------------
  df <- as_tibble(mat_df, rownames = "row_cond") |>
    pivot_longer(-row_cond,
                 names_to  = "col_cond",
                 values_to = "value") |>
    mutate(outcome = factor(outcome, levels = c("OL", "HS")),
           shot    = factor(paste0(shot_code, "-shot"),
                            levels = c("zero-shot", "few-shot")))
  
  # compute indices only for the condition block (not for gap / Global)
  cond_index <- setNames(seq_along(cond_order), cond_order)
  df <- df |>
    mutate(row_idx = cond_index[row_cond],
           col_idx = cond_index[col_cond],
           # mask upper triangle of the condition block
           value   = ifelse(!is.na(row_idx) & !is.na(col_idx) &
                              col_idx > row_idx, NA_real_, value)) |>
    select(-row_idx, -col_idx)
  
  df
}

# ── build the full data-frame for the 4 panels ─────────────────────────────
library(purrr)
heat_df <- map_dfr(          # outer loop over outcomes
  c("OL", "HS"),
  function(o) {
    map_dfr(                 # inner loop over shot types
      c("zero", "few"),
      function(s) {
        build_panel_df(outcome = o, shot_code = s)
      }
    )
  }
)

# keep identical ordering across facets
all_rows <- unique(heat_df$row_cond)
all_cols <- unique(heat_df$col_cond)

heat_df <- heat_df |>
  mutate(row_cond = factor(row_cond, levels = all_rows),
         col_cond = factor(col_cond, levels = all_cols))

# ── ggplot heat-maps with facets ───────────────────────────────────────────
ggplot(heat_df,
            aes(x = col_cond, y = row_cond, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))),
            size = 3) +
  scale_y_discrete(limits = rev) + 
  scale_x_discrete(
    labels = function(x) ifelse(x == "gap", "", 
                                ifelse(x == 'reference_agreement',
                                       'Reference',x))) +
  scale_fill_gradientn(
    colours   = coolwarm(100),
    limits    = c(0, 1),
    na.value  = "white",
    name      = "Kappa\n(Cohen's /\nFleiss')"
  ) +
  facet_grid(outcome ~ shot, switch = "both") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    axis.title      = element_blank(),
    strip.text      = element_text(size = 14)
  ) +
  labs(title = "Agreement Across Conditions")


################################################################################
### Inspect some of the low Agreement Cases
################################################################################

## ── 1) LLM label data (few-shot only) ────────────────────────────────────────
llm_labels_reshaped <- df_long %>% 
  filter(shot == "few-shot") %>% 
  group_by(tweet_id, condition, .drop = FALSE) %>% 
  summarise(
    mean_OL_llm = mean(OL, na.rm = TRUE),
    mean_HS_llm = mean(HS, na.rm = TRUE),
    .groups     = "drop"
  )

## ── 2) Human (Kern) label data ───────────────────────────────────────────────
tweets_kern_reshaped <- tweets_full %>% 
  group_by(tweet_id, .drop = FALSE) %>% 
  summarise(
    mean_OL_human = mean(offensive_language, na.rm = TRUE),
    mean_HS_human = mean(hate_speech,        na.rm = TRUE),
    .groups       = "drop"
  )

## ── 3) tweet text (hashed) ───────────────────────────────────────────────────
tweets_text <- tweets_full %>% 
  distinct(tweet_id, tweet_hashed)

## ── 4) merge the three tables ────────────────────────────────────────────────
merged <- llm_labels_reshaped %>% 
  inner_join(tweets_kern_reshaped, by = "tweet_id") %>%   # LLM ↔︎ human
  left_join(tweets_text,         by = "tweet_id")         # add text

## ── 5) absolute differences & sample 10 rows ────────────────────────────────
merged <- merged %>% 
  mutate(
    mean_diff_OL = abs(mean_OL_human - mean_OL_llm),
    mean_diff_HS = abs(mean_HS_human - mean_HS_llm)
  )

set.seed(15)  # reproducible like random_state=13
sample_10 <- merged %>% 
  filter(
    mean_diff_OL > 0.5,
    !str_detect(condition, "^C\\.HS")      # drop bare C.HS controls
  ) %>% 
  slice_sample(n = 10)

sample_10


################################################################################
### Regression to test the impact of the different conditions
################################################################################

colnames(df_long)

df_long$batch <- ifelse(grepl('batch', df_long$condition), 1, 0)
df_long$conf <- ifelse(grepl('conf', df_long$condition), 1, 0)
df_long <- df_long %>% 
  mutate(
    task = case_when(
      grepl("C\\.OL", condition) ~ "C.OL",
      grepl("C\\.HS", condition) ~ "C.HS",
      grepl("A",     condition)  ~ "A",
      grepl("B",     condition)  ~ "B",
      TRUE                       ~ NA_character_     # fallback, if needed
    )
  )

# GLMM for OL
library(lme4)
colnames(df_long)
model_OL <- glmer(OL ~ 1 + task + batch + conf + (1 | tweet_id),
                  data = df_long[!grepl('C.HS', df_long$condition) , ],
                  family = binomial
                  , control = glmerControl(optimizer = "bobyqa")
                  )
summary(model_OL)
# lower prob for OL in task B and C 
#     --> in B: OL is asked second
# lower prob for B in batch
# higher prob when asking for confidence 

# GLMM for HS
model_HS <- glmer(HS ~ 1 + task + batch + conf + (1 | tweet_id),
                  data = df_long[!grepl('C.OL', df_long$condition) , ],
                  family = binomial
                  , control = glmerControl(optimizer = "bobyqa")
)

summary(model_HS)
# higher prob for HS in task B and C
#     --> in B: HS is asked first
# lower prob for B in batch
# higher prob when asking for confidence 









