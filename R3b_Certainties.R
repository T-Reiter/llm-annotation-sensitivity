library(tidyverse)

# read labels 
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # wd = source loc
csv_paths <- list.files("Data_Collection/OL_NH", 
                        pattern = "\\.csv$", full.names = TRUE)
csv_paths <- csv_paths[grepl('2025_07_01', csv_paths)]
dfs <- lapply(csv_paths, read_csv, show_col_types = FALSE)
all_data <- bind_rows(dfs)
all_data <- as.data.frame(all_data)
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
    # title = "Confidence by Condition and Group"
  ) +
  coord_cartesian(ylim = c(70, 100))


ggsave("Plots/R/certainty_selfreport.png", last_plot(),
       dpi = 300,
       width = 24, height = 10, units = "in")


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
    # title = "Mean Entropy by Condition and Group"
  )



ggsave("Plots/R/certainty_entropy.png", last_plot(),
       dpi = 300,
       width = 24, height = 10, units = "in")



################################################################################
### CERTAINTY METRIC 3: TOP2-TOKEN-LOGPROB DISPARITY
################################################################################

logprob_conditions <- c('A', 'B', 'C.OL', 'C.HS',
                        paste0(c('A', 'B', 'C.OL', 'C.HS'), '_conf')
                        )

logprob_df <- all_data %>% 
  filter(condition %in% logprob_conditions) %>%
  filter(shot == 'few-shot')


logprob_df <- logprob_df %>%
  mutate(
    R1_OL = as.integer(str_detect(R1_label, "(?<!N)OL")),
    R2_OL = as.integer(str_detect(R2_label, "(?<!N)OL")),
    R3_OL = as.integer(str_detect(R3_label, "(?<!N)OL")),
    R1_HS = as.integer(str_detect(R1_label, "(?<!N)HS")),
    R2_HS = as.integer(str_detect(R2_label, "(?<!N)HS")),
    R3_HS = as.integer(str_detect(R3_label, "(?<!N)HS")),
  )

library(dplyr)
library(purrr)

# ---- prerequisites ----
# logprob_df  # your original (wide) data frame
# Make sure the rater-specific columns exist, e.g. "R1_label", "R1_score", …

ID_VARS <- c("batch_id", "tweet_in_batch", "tweet_id",
             "tweet", "condition", "shot", 'cond_clean', 
             paste0('R', c(1,2,3), '_label'),
             paste0('R', c(1,2,3), '_score'),
             paste0('R1_Token', c(1,2,3,4,5)),
             paste0('R2_Token', c(1,2,3,4,5)),
             paste0('R3_Token', c(1,2,3,4,5)))

raters <- c("R1", "R2", "R3")

# ---- build long data frame ----
long_df <- map_dfr(raters, function(r) {
  logprob_df %>%
    select(all_of(ID_VARS)) %>%            # keep identifier columns
    mutate(
      Rater  = r,
      label  = .data[[paste0(r, "_label")]],
      score  = .data[[paste0(r, "_score")]],
      Token1 = .data[[paste0(r, "_Token1")]],
      Token2 = .data[[paste0(r, "_Token2")]],
      Token3 = .data[[paste0(r, "_Token3")]],
      Token4 = .data[[paste0(r, "_Token4")]],
      Token5 = .data[[paste0(r, "_Token5")]]
    )
})


long_df <- long_df %>%
  mutate(across(starts_with("Token"), ~ as.character(.))) %>%
  mutate(
    # ----- OL tokens -----
    OL_top5_logprobs = case_when(
      condition %in% c("A", "A_conf", "C.OL", "C.OL_conf") ~ Token1,
      condition == "B"                                      ~ Token3,
      condition == "B_conf"                                 ~ Token4,
      TRUE                                                  ~ NA
    ),
    
    # ----- HS tokens -----
    HS_top5_logprobs = case_when(
      condition == "A"              ~ Token3,
      condition == "A_conf"         ~ Token4,
      condition %in% c("B", "B_conf",
                       "C.HS", "C.HS_conf") ~ Token1,
      TRUE                                  ~ NA
    )
  )


library(stringr)

## 1️⃣  Regex pattern ------------------------------------------
# Same semantics as the Python LOGPAT
# --> turn logprob cell into ordered list of tokens!
LOGPAT <- regex(
  "^(?<tok>.*?)\\s+(?<logp>[-+]?(?:\\d*\\.\\d+|\\d+)(?:e[-+]?\\d+)?)\\s*$",
  ignore_case = TRUE
)

## 2️⃣  Helper --------------------------------------------------
to_list <- function(cell) {
  # Silently return empty list on NA / non-string inputs
  if (!is.character(cell) || is.na(cell)) return(list())
  
  lines <- str_split(cell, "\n", simplify = TRUE)
  
  map(lines, function(line) {
    m <- str_match(line, LOGPAT)
    if (!is.na(m[1, 1])) {
      # Named capture groups "tok" and "logp"
      list(token = m[1, "tok"],
           logp  = as.numeric(m[1, "logp"]))
    } else {
      NULL
    }
  }) %>% compact()            # drop NULLs
}

## 3️⃣  Apply row-wise ------------------------------------------
long_df <- long_df %>%
  mutate(
    OL_top5_list = map(OL_top5_logprobs, to_list),
    HS_top5_list = map(HS_top5_logprobs, to_list)
  )


library(dplyr)
library(purrr)

# helper -----------------------------------------------------------------
first_logp <- function(x) {
  # x is one element of OL_top5_list / HS_top5_list (itself a list)
  if (length(x) >= 1 && !is.null(x[[1]]$logp)) {
    x[[1]]$logp          # numeric value
  } else {
    NA_real_
  }
}

# main -------------------------------------------------------------------
long_df <- long_df %>%
  mutate(
    # 1. grab first log-prob (or NA) ----
    OL_first_token_logprob = map_dbl(OL_top5_list, first_logp),
    HS_first_token_logprob = map_dbl(HS_top5_list, first_logp),
    
    # 2. convert to probability ---------
    OL_first_token_prob    = exp(OL_first_token_logprob),
    HS_first_token_prob    = exp(HS_first_token_logprob)
  )

# optional: nicer printing
options(digits = 10)

# Plot Probs and Logprobs
library(ggplot2)
library(patchwork)   # install.packages("patchwork") if needed

# same colours as your Matplotlib palette
pal <- c(OL = "#1f77b4", HS = "#ff7f0e")

p1 <- ggplot(long_df, aes(OL_first_token_logprob)) +
  geom_histogram(bins = 50, fill = pal["OL"]) +
  labs(title = "OL – log p", x = "log-probability", y = "Count") +
  theme_minimal()

p2 <- ggplot(long_df, aes(HS_first_token_logprob)) +
  geom_histogram(bins = 50, fill = pal["HS"]) +
  labs(title = "HS – log p", x = "log-probability", y = "Count") +
  theme_minimal()

p3 <- ggplot(long_df, aes(OL_first_token_prob)) +
  geom_histogram(bins = 50, fill = pal["OL"]) +
  labs(title = "OL – p", x = "probability", y = "Count") +
  theme_minimal()

p4 <- ggplot(long_df, aes(HS_first_token_prob)) +
  geom_histogram(bins = 50, fill = pal["HS"]) +
  labs(title = "HS – p", x = "probability", y = "Count") +
  theme_minimal()

# 2×2 layout – same as plt.subplots(2, 2, …)
(p1 | p2) / (p3 | p4)



#### Calculate Top2-Token Disparity Ratings
#--- helpers ---------------------------------------------------------------
first_logp  <- function(x) {
  if (length(x) >= 1 && !is.null(x[[1]]$logp)) x[[1]]$logp else NA_real_
}
second_logp <- function(x) {
  if (length(x) >= 2 && !is.null(x[[2]]$logp)) x[[2]]$logp else NA_real_
}

#--- main ------------------------------------------------------------------
long_df <- long_df %>%
  mutate(
    # 1️⃣  first-listed log-probs
    OL_first_token_logprob = map_dbl(OL_top5_list, first_logp),
    HS_first_token_logprob = map_dbl(HS_top5_list, first_logp),
    
    # 2️⃣  second-listed log-probs
    OL_second_token_logprob = map_dbl(OL_top5_list, second_logp),
    HS_second_token_logprob = map_dbl(HS_top5_list, second_logp),
    
    # 3️⃣  probabilities (exp)
    OL_first_token_prob  = exp(OL_first_token_logprob),
    HS_first_token_prob  = exp(HS_first_token_logprob),
    OL_second_token_prob = exp(OL_second_token_logprob),
    HS_second_token_prob = exp(HS_second_token_logprob)
  )

# nicer printing (like pandas' display.float_format)
options(digits = 10)

# top 2 Token Disparity
long_df$top2_token_disparity_OL <- 
  long_df$OL_first_token_logprob - long_df$OL_second_token_logprob
long_df$top2_token_disparity_HS <- 
  long_df$HS_first_token_logprob - long_df$HS_second_token_logprob



# Combine the values for the C.OL and C.HS conditions 
# ── 1. base disparity_df --------------------------------------------------
disparity_df <- long_df %>%
  select(
    batch_id, tweet_in_batch, tweet_id, tweet,
    condition, cond_clean, Rater, shot, label,
    OL_top5_list, HS_top5_list,
    top2_token_disparity_OL, top2_token_disparity_HS
  ) 

key_cols <- c("batch_id", "tweet_in_batch", "tweet_id",
              "tweet", "condition", "shot", "Rater", "cond_clean")

# ── 2. conditions A & B ---------------------------------------------------
disparity_df_A_B <- disparity_df %>%
  filter(str_detect(condition, "[AB]"))   # matches 'A', 'B', 'A_conf', ...

# ── 3. C-conditions: start with unique tweet×cond×rater rows --------------
disparity_df_C <- disparity_df %>%
  filter(grepl('C', condition)) %>%
  select(all_of(key_cols)) %>%
  distinct(tweet_id, cond_clean, Rater, .keep_all = TRUE)

# ---- 3a. join OL ratings -------------------------------------------------
C_OL <- disparity_df %>%
  filter(str_detect(condition, "C\\.OL")) %>%
  select(tweet, tweet_id, cond_clean, Rater,
         OL_top5_list, top2_token_disparity_OL)

disparity_df_C <- disparity_df_C %>%
  left_join(C_OL, by = c("tweet","tweet_id", "cond_clean", "Rater"))

# ---- 3b. join HS ratings -------------------------------------------------
C_HS <- disparity_df %>%
  filter(str_detect(condition, "C\\.HS")) %>%
  select(tweet, tweet_id, cond_clean, Rater,
         HS_top5_list, top2_token_disparity_HS)

disparity_df_C <- disparity_df_C %>%
  left_join(C_HS, by = c("tweet","tweet_id", "cond_clean", "Rater"))

# ── 4. final combined data frame -----------------------------------------
disparity_df <- bind_rows(disparity_df_A_B, disparity_df_C)





#### Plot Top2 Token Disparities

# ── 1. means and SEMs ──────────────────────────────────────────────
stats <- disparity_df %>%                                   # base df from earlier step
  group_by(cond_clean) %>% 
  summarise(
    across(
      c(top2_token_disparity_OL, top2_token_disparity_HS),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sem  = ~ sd(.x,  na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ── 2. reshape for plotting and add 95 % CI ───────────────────────
plot_df <- stats %>%
  pivot_longer(
    -cond_clean,
    names_to   = c("Token", ".value"),   # produces columns: Token, mean, sem
    names_pattern = "(OL|HS)_(.*)"       # splits "OL_mean", "OL_sem", etc.
  ) %>%
  mutate(ci95 = 1.96 * sem)              # 95 % confidence interval

# ── 3. bar chart with error bars (ggplot2) ───────────────────────
# colour palette
pal <- c(OL = "#1f77b4", HS = "#ff7f0e")   # matches your Python palette


plot_df %>% 
  mutate(Token = factor(Token, levels = c("OL", "HS"))) %>%   # ← OL first
  ggplot(aes(x = cond_clean, y = mean, fill = Token)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - ci95, ymax = mean + ci95),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_y_continuous(breaks = seq(0, 16, 2), limits = c(0, 17)) +
  scale_fill_manual(values = pal) +
  labs(
    x = "Condition",
    y = "Average Top-2 Token Disparity",
    title = "Average Top-2 Token Disparities by Condition (95 % CI)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Plots/R/certainty_disparity.png", last_plot(),
       dpi = 300,
       width = 24, height = 10, units = "in")


################################################################################
### Agreement of the different uncertainty measures
################################################################################

# ── 1.  tweet-level confidence (row-wise means) ────────────────────────────
confidence_df_final <- confidence_df_final %>%
  mutate(
    mean_confidence_OL = rowMeans(
      select(., R1_OL_confidence, R2_OL_confidence, R3_OL_confidence),
      na.rm = TRUE
    ),
    mean_confidence_HS = rowMeans(
      select(., R1_HS_confidence, R2_HS_confidence, R3_HS_confidence),
      na.rm = TRUE
    )
  )

# ── 2.  tweet-level mean disparity across raters ───────────────────────────
disparity_df_means <- disparity_df %>% 
  group_by(cond_clean, tweet_id) %>% 
  summarise(
    mean_disparity_OL = mean(top2_token_disparity_OL, na.rm = TRUE),
    mean_disparity_HS = mean(top2_token_disparity_HS, na.rm = TRUE),
    .groups = "drop"
  )




### Plot correlations  ────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)     # install.packages("patchwork") if needed
library(tibble)        # rownames_to_column()
library(psych)         # corr.test()

# -------------------------------------------------------------------------
# Custom order for both axes
order_vars <- c("self_report", "entropy", "disparity")
rev_order  <- rev(order_vars)            # for y-axis (top → bottom)

# ───────────────────────────────────── OL ─────────────────────────────────
# Merge the three OL‐metric tables
merged_ol <- entropy_df_full %>% 
  select(tweet_id, cond_clean, entropy = tweet_entropy_OL) %>% 
  left_join(
    confidence_df_final %>% 
      select(tweet_id, cond_clean, self_report = mean_tweet_conf_OL),
    by = c("tweet_id", "cond_clean")
  ) %>% 
  left_join(
    disparity_df_means %>% 
      select(tweet_id, cond_clean, disparity = mean_disparity_OL),
    by = c("tweet_id", "cond_clean")
  )

# Correlation + p-values
ct_ol <- merged_ol %>% 
  select(all_of(order_vars)) %>% 
  psych::corr.test(use = "pairwise")

# Tidy OL matrix with significance stars
corr_ol_df <- ct_ol$r %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>% 
  left_join(
    ct_ol$p %>% 
      as.data.frame() %>% 
      rownames_to_column("Var1") %>% 
      pivot_longer(-Var1, names_to = "Var2", values_to = "p"),
    by = c("Var1", "Var2")
  ) %>% 
  mutate(
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    label = sprintf("%.2f%s", r, stars),
    Var2  = factor(Var2, levels = order_vars),   # x: left → right
    Var1  = factor(Var1, levels = rev_order)     # y: top → bottom
  )

# Heat-map for OL
p_ol <- ggplot(corr_ol_df, aes(Var2, Var1, fill = r)) +
  geom_tile() +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(
    limits   = c(-1, 1), midpoint = 0,
    low      = "#2166ac", mid = "white", high = "#b2182b",
    name     = "r"
  ) +
  labs(title = "Correlation: OL Metrics", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

p_ol

# ───────────────────────────────────── HS ─────────────────────────────────
# Merge the three HS‐metric tables
merged_hs <- entropy_df_full %>% 
  select(tweet_id, cond_clean, entropy = tweet_entropy_HS) %>% 
  left_join(
    confidence_df_final %>% 
      select(tweet_id, cond_clean, self_report = mean_tweet_conf_HS),
    by = c("tweet_id", "cond_clean")
  ) %>% 
  left_join(
    disparity_df_means %>% 
      select(tweet_id, cond_clean, disparity = mean_disparity_HS),
    by = c("tweet_id", "cond_clean")
  )

# Correlation + p-values
ct_hs <- merged_hs %>% 
  select(all_of(order_vars)) %>% 
  psych::corr.test(use = "pairwise")

# Tidy HS matrix with significance stars
corr_hs_df <- ct_hs$r %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>% 
  left_join(
    ct_hs$p %>% 
      as.data.frame() %>% 
      rownames_to_column("Var1") %>% 
      pivot_longer(-Var1, names_to = "Var2", values_to = "p"),
    by = c("Var1", "Var2")
  ) %>% 
  mutate(
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    label = sprintf("%.2f%s", r, stars),
    Var2  = factor(Var2, levels = order_vars),
    Var1  = factor(Var1, levels = rev_order)
  )

# Heat-map for HS
p_hs <- ggplot(corr_hs_df, aes(Var2, Var1, fill = r)) +
  geom_tile() +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(
    limits   = c(-1, 1), midpoint = 0,
    low      = "#2166ac", mid = "white", high = "#b2182b",
    name     = "r"
  ) +
  labs(title = "Correlation: HS Metrics", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

p_hs

# -------------------------------------------------------------------------
# Display the two plots side-by-side with a shared legend
(p_ol | p_hs) + plot_layout(guides = "collect") &
  theme(legend.position = "right")


ggsave("Plots/R/certainty_correlations.png", last_plot(),
       dpi = 300,
       width = 24, height = 10, units = "in")
