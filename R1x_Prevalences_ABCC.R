
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # wd = source loc
rm(list = ls())
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
ggsave("Plots/R/hs_ol_descr_x_zerofew_ABCC.png", last_plot(),
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
# ggsave("Plots/hs_ol_descr_x_fewshot_ABCC.png", last_plot(), 
#        dpi = 300,
#        width = 24, height = 10, units = "in")










