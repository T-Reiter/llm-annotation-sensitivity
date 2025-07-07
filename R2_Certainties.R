library(tidyverse)

# read labels 
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # wd = source loc
csv_paths <- list.files("Data_Collection/OL_NH", 
                        pattern = "\\.csv$", full.names = TRUE)
csv_paths <- csv_paths[grepl('2025_07_01', csv_paths)]
dfs <- lapply(csv_paths, read_csv, show_col_types = FALSE)
all_data <- bind_rows(dfs)
length(unique(all_data$tweet_id))
length(unique(all_data$batch_id))
sum(duplicated(all_data))

# batch_data <- all_data %>% filter(grepl('batch', condition))
all_data$shot = 'few-shot'

## vector that defines the column set we’ll need again later -----------
all_data$cond_clean <- gsub("\\.(OL|HS)", "", all_data$condition)


################################################################################
### CERTAINTY METRIC 1: SELF-REPORTED CONFIDENCE SCORES (CONF CONDITIONS)
################################################################################

library(readr)

confidence_df <- all_data %>% filter(grepl('conf', condition))

confidence_df <- confidence_df %>%
  mutate(
    R1_OL = as.integer(str_detect(R1_label, "(?<!N)OL")),
    R2_OL = as.integer(str_detect(R2_label, "(?<!N)OL")),
    R3_OL = as.integer(str_detect(R3_label, "(?<!N)OL")),
    R1_HS = as.integer(str_detect(R1_label, "(?<!N)HS")),
    R2_HS = as.integer(str_detect(R2_label, "(?<!N)HS")),
    R3_HS = as.integer(str_detect(R3_label, "(?<!N)HS")),
  )


confidence_df$conf_R1 <- regmatches(confidence_df$R1_score, 
                              gregexpr("[[:digit:]]+", 
                                       confidence_df$R1_score)) 
confidence_df$conf_R2 <- regmatches(confidence_df$R2_score, 
                              gregexpr("[[:digit:]]+", 
                                       confidence_df$R2_score)) 
confidence_df$conf_R3 <- regmatches(confidence_df$R3_score, 
                              gregexpr("[[:digit:]]+", 
                                       confidence_df$R3_score)) 

# Correctly order the confidences
# takes some time... (not the most efficient approach but straightforward) 
for (row in 1:nrow(confidence_df)){
  if (grepl('A',confidence_df[row, 'condition'])){
    confidence_df[row, 'R1_OL_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[1]
    confidence_df[row, 'R1_HS_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[2]
    confidence_df[row, 'R2_OL_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[1]
    confidence_df[row, 'R2_HS_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[2]
    confidence_df[row, 'R3_OL_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[1]
    confidence_df[row, 'R3_HS_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[2]
  } else if (grepl('B',confidence_df[row, 'condition'])){
    confidence_df[row, 'R1_OL_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[2]
    confidence_df[row, 'R1_HS_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[1]
    confidence_df[row, 'R2_OL_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[2]
    confidence_df[row, 'R2_HS_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[1]
    confidence_df[row, 'R3_OL_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[2]
    confidence_df[row, 'R3_HS_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[1]
  } else if (grepl('C.OL',confidence_df[row, 'condition'])){
    confidence_df[row, 'R1_OL_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[1]
    confidence_df[row, 'R1_HS_confidence'] <- NA
    confidence_df[row, 'R2_OL_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[1]
    confidence_df[row, 'R2_HS_confidence'] <- NA
    confidence_df[row, 'R3_OL_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[1]
    confidence_df[row, 'R3_HS_confidence'] <- NA
  } else if (grepl('C.HS',confidence_df[row, 'condition'])){
    confidence_df[row, 'R1_OL_confidence'] <- NA
    confidence_df[row, 'R1_HS_confidence'] <- unlist(confidence_df[row, 'conf_R1'][[1]])[1]
    confidence_df[row, 'R2_OL_confidence'] <- NA
    confidence_df[row, 'R2_HS_confidence'] <- unlist(confidence_df[row, 'conf_R2'][[1]])[1]
    confidence_df[row, 'R3_OL_confidence'] <- NA
    confidence_df[row, 'R3_HS_confidence'] <- unlist(confidence_df[row, 'conf_R3'][[1]])[1]
  }
  
}


# Correctly match the confidences in A&B and melt the c conditions --------
library(dplyr)
library(stringr)
library(purrr)




key_cols <- c("batch_id", "tweet_in_batch", "tweet_id",
              "tweet", "condition", "shot",
              "R1_label", "R2_label", "R3_label",
              "R1_score", "R2_score", "R3_score",
              "cond_clean")

## ------------------------------------------------------------------ ##
## 2. derive OL / HS *confidence* columns --------------------------- ##
## ------------------------------------------------------------------ ##
# helper →  TRUE ⇢ OL is first element (index 1);  FALSE ⇢ OL is second
ol_first <- with(confidence_df,
                 str_detect(condition, "A")     |             # A
                   str_detect(condition, "C\\.OL"))             # C.OL


## ------------------------------------------------------------------ ##
## 3. build the A/B block (nothing special) ------------------------- ##
## ------------------------------------------------------------------ ##
confidence_df_A_B <- confidence_df %>%
  filter(str_detect(condition, "[AB]")) %>%                # keep A or B
  select(all_of(key_cols)) 

confidence_df_A_B <- merge(
  confidence_df_A_B,
  confidence_df[, c('tweet_id', 'cond_clean',
                    "R1_OL", "R2_OL", "R3_OL",
                    "R1_HS", "R2_HS", "R3_HS",
                    "conf_R1", "conf_R2", "conf_R3",
                    paste0("R", c(1,2,3), "_OL_confidence"),
                    paste0("R", c(1,2,3), "_HS_confidence"))],
  on = c('tweet_id', 'cond_clean'))

## ------------------------------------------------------------------ ##
## 4. build the C block (merge C.OL and C.HS) ----------------------- ##
## ------------------------------------------------------------------ ##
confidence_df_C.OL <- confidence_df %>%
  filter(grepl("C.OL", condition)) %>%                # keep A or B
  select(all_of(c(key_cols,
                  "R1_OL", "R2_OL", "R3_OL",
                  "conf_R1", "conf_R2", "conf_R3",
                  paste0("R", c(1,2,3), "_OL_confidence")))) 
colnames(confidence_df_C.OL)[which(colnames(confidence_df_C.OL) %in% 
                                     c("conf_R1", "conf_R2", "conf_R3"))] <- 
  c(paste0(c("conf_R1", "conf_R2", "conf_R3"),'_OL'))
  

confidence_df_C.HS <- confidence_df %>%
  filter(grepl("C.HS", condition)) %>%                # keep A or B
  select(all_of(c(key_cols,
                  "R1_HS", "R2_HS", "R3_HS",
                  "conf_R1", "conf_R2", "conf_R3",
                  paste0("R", c(1,2,3), "_HS_confidence")))) 
colnames(confidence_df_C.HS)[which(colnames(confidence_df_C.HS) %in% 
                                     c("conf_R1", "conf_R2", "conf_R3"))] <- 
  c(paste0(c("conf_R1", "conf_R2", "conf_R3"),'_HS'))


confidence_df_C <- merge(
  confidence_df_C.OL, 
  confidence_df_C.HS[,c('tweet_id', 'cond_clean',
                        "R1_HS", "R2_HS", "R3_HS",
                        "conf_R1_HS", "conf_R2_HS", "conf_R3_HS",
                        paste0("R", c(1,2,3), "_HS_confidence"))],
  on = c('tweet_id', 'cond_clean'))



## ------------------------------------------------------------------ ##
## 5. final combined data frame ------------------------------------- ##
## ------------------------------------------------------------------ ##
confidence_df_final <- bind_rows(confidence_df_A_B, confidence_df_C)
confidence_df_final$R1_OL_confidence <- as.numeric(confidence_df_final$R1_OL_confidence)
confidence_df_final$R2_OL_confidence <- as.numeric(confidence_df_final$R2_OL_confidence)
confidence_df_final$R3_OL_confidence <- as.numeric(confidence_df_final$R3_OL_confidence)
confidence_df_final$R1_HS_confidence <- as.numeric(confidence_df_final$R1_HS_confidence)
confidence_df_final$R2_HS_confidence <- as.numeric(confidence_df_final$R2_HS_confidence)
confidence_df_final$R3_HS_confidence <- as.numeric(confidence_df_final$R3_HS_confidence)

## quick sanity checks (rows similar to the Python prints) ------------
cat("NRows A and B conditions :", nrow(confidence_df_A_B), "\n")
cat("NRows C conditions       :", nrow(confidence_df_C),   "\n")
cat("NRows total              :", nrow(confidence_df_final), "\n")


# Plot the average certainties per condition 
confidence_df_final['mean_tweet_conf_OL'] <- 
  rowMeans(confidence_df_final[, paste0('R', c(1,2,3), '_OL_confidence')])
confidence_df_final['mean_tweet_conf_HS'] <- 
  rowMeans(confidence_df_final[, paste0('R', c(1,2,3), '_HS_confidence')])


mean_confidence_wide <- confidence_df_final %>%
  group_by(cond_clean) %>%
  summarise(
    mean_confidence_OL = mean(mean_tweet_conf_OL, na.rm = TRUE),
    mean_confidence_HS = mean(mean_tweet_conf_HS, na.rm = TRUE),
    .groups = "drop"
  )



library(ggplot2)

# Define color palette
palette <- c("OL" = "#1f77b4", "HS" = "#ff7f0e")

# Separate normal and batch conditions
normal_conditions <- mean_confidence_wide %>%
  filter(str_detect(cond_clean, "(?<!_batch)_conf$"))

batch_conditions <- mean_confidence_wide %>%
  filter(str_detect(cond_clean, "_batch_conf"))

# Sort conditions alphabetically
normal_conditions <- normal_conditions %>% arrange(cond_clean)
batch_conditions <- batch_conditions %>% arrange(cond_clean)

# Concatenate for ordered display
ordered_df <- bind_rows(normal_conditions, batch_conditions)

# Convert to long format for ggplot
long_df <- ordered_df %>%
  pivot_longer(
    cols = c(mean_confidence_OL, mean_confidence_HS),
    names_to = "group",
    values_to = "mean_confidence"
  ) %>%
  mutate(
    group = str_replace(group, "mean_confidence_", ""),
    group = factor(group, levels = c("OL", "HS")),
    cond_clean = factor(cond_clean, levels = ordered_df$cond_clean)
  )

# Plot
ggplot(long_df, aes(x = cond_clean, y = mean_confidence, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, ncol = 2, scales = "fixed") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "Mean Confidence",
    title = "Confidence by Condition and Group"
  ) +
  coord_cartesian(ylim = c(70, 100))





################################################################################
### CERTAINTY METRIC 2: (EMPIRICAL) ENTROPY 
################################################################################

key_cols <- c("batch_id", "tweet_in_batch", "tweet_id",
              "tweet", "condition", "shot",
              "cond_clean")


entropy_df <- all_data


entropy_df <- entropy_df %>%
  mutate(
    R1_OL = as.integer(str_detect(R1_label, "(?<!N)OL")),
    R2_OL = as.integer(str_detect(R2_label, "(?<!N)OL")),
    R3_OL = as.integer(str_detect(R3_label, "(?<!N)OL")),
    R1_HS = as.integer(str_detect(R1_label, "(?<!N)HS")),
    R2_HS = as.integer(str_detect(R2_label, "(?<!N)HS")),
    R3_HS = as.integer(str_detect(R3_label, "(?<!N)HS")),
  )



entropy_df_A_B <- entropy_df %>%
  filter(str_detect(condition, "[AB]")) %>%                # keep A or B
  select(all_of(key_cols)) 

entropy_df_A_B <- merge(
  entropy_df_A_B,
  entropy_df[, c('tweet_id', 'cond_clean',
                  "R1_OL", "R2_OL", "R3_OL",
                  "R1_HS", "R2_HS", "R3_HS")],
  on = c('tweet_id', 'cond_clean'))




## ------------------------------------------------------------------ ##
## 4. build the C block (merge C.OL and C.HS) ----------------------- ##
## ------------------------------------------------------------------ ##
entropy_df_C.OL <- entropy_df %>%
  filter(grepl("C.OL", condition)) %>%                # keep A or B
  select(all_of(c(key_cols,
                  "R1_OL", "R2_OL", "R3_OL")))

entropy_df_C.HS <- entropy_df %>%
  filter(grepl("C.HS", condition)) %>%                # keep A or B
  select(all_of(c(key_cols,
                  "R1_HS", "R2_HS", "R3_HS")))


entropy_df_C <- merge(
  entropy_df_C.OL, 
  entropy_df_C.HS[,c('tweet_id', 'cond_clean',
                        "R1_HS", "R2_HS", "R3_HS")],
  on = c('tweet_id', 'cond_clean'))



entropy_df_full <- rbind(entropy_df_A_B, entropy_df_C)


# Define a function to compute binary Shannon entropy
shannon_entropy <- function(p) {
  # Handle edge cases (p = 0 or 1) to avoid NaNs
  entropy <- -p * log2(p) - (1 - p) * log2(1 - p)
  entropy[is.nan(entropy)] <- 0  # 0 log 0 is defined as 0 in information theory
  return(entropy)
}

# Compute proportion of 1s for each outcome
entropy_df_full$p_OL <- rowMeans(entropy_df_full[, paste0('R', c(1,2,3), '_OL')])
entropy_df_full$p_HS <- rowMeans(entropy_df_full[, paste0('R', c(1,2,3), '_HS')])

# Apply entropy function
entropy_df_full$tweet_entropy_OL <- shannon_entropy(entropy_df_full$p_OL)
entropy_df_full$tweet_entropy_HS <- shannon_entropy(entropy_df_full$p_HS)

mean_entropy_wide <- entropy_df_full %>%
  group_by(cond_clean) %>%
  summarise(
    mean_entropy_OL = mean(tweet_entropy_OL, na.rm = TRUE),
    mean_entropy_HS = mean(tweet_entropy_HS, na.rm = TRUE),
    .groups = "drop"
  )


# Plot
library(ggplot2)
library(dplyr)
library(tidyr)

# Define color palette
palette <- c("OL" = "#1f77b4", "HS" = "#ff7f0e")

# Define condition order (same as in Python)
condition_order <- c(
  'A', 'A_conf', 'A_batch', 'A_batch_conf',
  'B', 'B_conf', 'B_batch', 'B_batch_conf',
  'C', 'C_conf', 'C_batch', 'C_batch_conf'
)

# Make sure cond_clean follows the correct order
mean_entropy_wide <- mean_entropy_wide %>%
  mutate(cond_clean = factor(cond_clean, levels = condition_order))

# Convert to long format for ggplot
mean_entropy_long <- mean_entropy_wide %>%
  pivot_longer(
    cols = c(mean_entropy_OL, mean_entropy_HS),
    names_to = "group",
    names_prefix = "mean_entropy_",
    values_to = "mean_entropy"
  ) %>%
  mutate(group = factor(group, levels = c("OL", "HS")))

# Plot
ggplot(mean_entropy_long, aes(x = cond_clean, y = mean_entropy, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, ncol = 2, scales = "fixed") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = "Mean entropy (bits)",
    title = "Mean Entropy by Condition and Group"
  )







################################################################################
### CERTAINTY METRIC 3: TOP2-TOKEN-LOGPROB DISPARITY
################################################################################







