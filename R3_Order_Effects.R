library(readr)   
library(dplyr)   
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)
library(purrr)


# read labels 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # wd = source loc
csv_paths <- list.files("Data_Collection/OL_NH", 
                        pattern = "\\.csv$", full.names = TRUE)
dfs <- lapply(csv_paths, read_csv, show_col_types = FALSE)
all_data <- bind_rows(dfs)

# batch_data <- all_data %>% filter(grepl('batch', condition))
batch_data <- all_data
batch_data$shot = 'few-shot'


# --------- 1) Columns shared by every responder -------------------
shared_cols <- c("batch_id", "tweet_in_batch", "tweet_id",
                 "tweet", "condition", 'shot')

# --------- 2) Prepare an empty list to collect per-responder frames
df_list <- list()

# --------- 3) Loop over the responder IDs -------------------------
for (responder in c("R1", "R2", "R3")) {
  
  # copy the common columns
  df_temp <- batch_data[ , shared_cols, drop = FALSE]
  
  # add a column identifying the responder
  df_temp$responder <- responder
  
  # pull the responder-specific columns
  df_temp$label  <- batch_data[[paste0(responder, "_label")]]
  # df_temp$score  <- batch_data[[paste0(responder, "_score")]]
  # df_temp$Token1 <- batch_data[[paste0(responder, "_Token1")]]
  # df_temp$Token2 <- batch_data[[paste0(responder, "_Token2")]]
  # df_temp$Token3 <- batch_data[[paste0(responder, "_Token3")]]
  
  # store in the list
  df_list[[responder]] <- df_temp
}

# --------- 4) Concatenate everything into one long data frame -----
df_long <- do.call(rbind, df_list)
row.names(df_long) <- NULL   # tidy up row names

# ------------------------------------------------------------------
# 5) Indicator columns (same regex logic as Python)
# ------------------------------------------------------------------
df_long$OL <- ifelse(grepl("(?<!N)OL", df_long$label, perl = TRUE), 1L, 0L)
df_long$HS <- ifelse(grepl("(?<!N)HS", df_long$label, perl = TRUE), 1L, 0L)




################################################################################
### PLOT LINEAR EFFECTS (ALL CONDITIONS) (LMM)
################################################################################

df_melt <- df_long %>%
  pivot_longer(cols = c(OL, HS),
               names_to = "Measure",
               values_to = "Label") %>%
  select(batch_id, condition, tweet_in_batch, shot, Measure, Label) %>%
  ## 1b) drop C.OL↔HS and C.HS↔OL mismatches
  filter(!(str_detect(condition, "C\\.OL") & Measure == "HS") &
           !(str_detect(condition, "C\\.HS") & Measure == "OL")) %>%
  ## 1c) strip “.OL” / “.HS”
  mutate(condition = str_remove(condition, "\\.OL|\\.HS")) %>%
  ## 1d) derive facet columns
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = paste0(shot, "_", base_condition)
  ) %>%
  mutate(
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

row_order <- c(paste0("few-shot_" , LETTERS[1:3]))

df_melt <- df_melt %>%
  mutate(shot_base = factor(shot_base, levels = row_order))

# ──────────────────────────────────────────────────────────────
# 2.  PROPORTIONS FOR PLOT
# ──────────────────────────────────────────────────────────────
df_prop <- df_melt %>%
  group_by(shot, condition, tweet_in_batch, Measure) %>%
  summarise(Proportion = mean(Label, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = factor(paste0(shot, "_", base_condition),
                       levels = row_order),
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

# ──────────────────────────────────────────────────────────────
# 3.  FACETTED OLS LINES (ggplot2)
# ──────────────────────────────────────────────────────────────
palette <- c(OL = "#1f77b4", HS = "#ff7f0e")

g <- ggplot(df_prop,
            aes(tweet_in_batch, Proportion, 
                colour = Measure,
                fill = Measure)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "glm", formula = y ~ x, se = TRUE) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_fill_manual(values = palette, guide = "none") + 
  facet_grid(rows = vars(shot_base), cols = vars(group_type)) +
  labs(x = "Tweet Position in Batch",
       y = "Proportion") +
  theme_bw()

g

# ──────────────────────────────────────────────────────────────
# 4.  GLMMs (binomial logit)  — one per facet × Measure
#     Formula: Label ~ tweet_in_batch + (1 | batch_id)
# ──────────────────────────────────────────────────────────────
combos <- df_melt %>%
  distinct(shot_base, group_type, Measure)

pvals <- combos %>%
  mutate(p_value = pmap_dbl(list(shot_base, group_type, Measure),
                            function(sb, gt, meas) {
                              sub <- df_melt %>%
                                filter(shot_base == sb, group_type == gt, Measure == meas)
                              
                              # skip if not enough variation
                              if(n_distinct(sub$batch_id) < 2 ||
                                 length(unique(sub$Label)) < 2) return(1)
                              
                              fit <- tryCatch(
                                glmer(Label ~ tweet_in_batch + (1 | batch_id),
                                      data = sub, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa")),
                                error = function(e) NULL
                              )
                              if(is.null(fit)) return(1)
                              
                              p <- summary(fit)$coefficients["tweet_in_batch", "Pr(>|z|)"]
                              p
                            }))

# ──────────────────────────────────────────────────────────────
# 5.  SHADE FACETS WHERE EITHER OL OR HS IS SIGNIFICANT
# ──────────────────────────────────────────────────────────────
sig_panels <- pvals %>%
  group_by(shot_base, group_type) %>%
  summarise(sig = any(p_value < 0.05/24), .groups = "drop") %>%
  filter(sig)

if(nrow(sig_panels)) {
  g <- g +
    geom_rect(data = sig_panels,
              inherit.aes = FALSE,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = "transparent",
              color = "black",
              linewidth = 2) +
    theme(panel.spacing = unit(0.6, "lines"))
}
g

# ──────────────────────────────────────────────────────────────
# 6.  ADD ASTERISKS FOR EACH SIGNIFICANT MEASURE
# ──────────────────────────────────────────────────────────────
asterisks <- pvals %>%
  filter(p_value < 0.05/24) %>%
  mutate(y = c(0.90, 0.82)[match(Measure, c("OL","HS"))],
         label = "*")

g <- g +
  geom_text(data = asterisks,
            inherit.aes = FALSE,
            aes(x = Inf, y = y, label = label, 
                colour = Measure),
            hjust = 1.5, vjust = 0.5, size = 10,
            fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_fill_manual(values = palette, guide = "none") + 
  ggtitle("Effect of Position on OL/HS Proportions (asterisks = GLMM slope p < 0.05/24; random intercept = batch_id)")

g













################################################################################
### PLOT LINEAR EFFECTS (BATCH CONDITIONS ONLY) (LMM)
################################################################################
# ──────────────────────────────────────────────────────────────
# 1.  DATA PREPARATION   (melt, filter, derive facet keys)
# ──────────────────────────────────────────────────────────────
df_melt <- df_long %>% filter(grepl('batch', condition)) %>% 
  pivot_longer(cols = c(OL, HS),
               names_to = "Measure",
               values_to = "Label") %>%
  select(batch_id, condition, tweet_in_batch, shot, Measure, Label) %>%
  ## 1b) drop C.OL↔HS and C.HS↔OL mismatches
  filter(!(str_detect(condition, "C\\.OL") & Measure == "HS") &
           !(str_detect(condition, "C\\.HS") & Measure == "OL")) %>%
  ## 1c) strip “.OL” / “.HS”
  mutate(condition = str_remove(condition, "\\.OL|\\.HS")) %>%
  ## 1d) derive facet columns
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = paste0(shot, "_", base_condition)
  ) %>%
  mutate(
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

row_order <- c(paste0("few-shot_" , LETTERS[1:3]))

df_melt <- df_melt %>%
  mutate(shot_base = factor(shot_base, levels = row_order))

# ──────────────────────────────────────────────────────────────
# 2.  PROPORTIONS FOR PLOT
# ──────────────────────────────────────────────────────────────
df_prop <- df_melt %>%
  group_by(shot, condition, tweet_in_batch, Measure) %>%
  summarise(Proportion = mean(Label, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = factor(paste0(shot, "_", base_condition),
                       levels = row_order),
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

# ──────────────────────────────────────────────────────────────
# 3.  FACETTED OLS LINES (ggplot2)
# ──────────────────────────────────────────────────────────────
palette <- c(OL = "#1f77b4", HS = "#ff7f0e")

g <- ggplot(df_prop,
            aes(tweet_in_batch, Proportion, 
                colour = Measure,
                fill = Measure)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "glm", formula = y ~ x, se = TRUE) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_fill_manual(values = palette, guide = "none") + 
  facet_grid(rows = vars(shot_base), cols = vars(group_type)) +
  labs(x = "Tweet Position in Batch",
       y = "Proportion") +
  theme_bw()

g

# ──────────────────────────────────────────────────────────────
# 4.  GLMMs (binomial logit)  — one per facet × Measure
#     Formula: Label ~ tweet_in_batch + (1 | batch_id)
# ──────────────────────────────────────────────────────────────
combos <- df_melt %>%
  distinct(shot_base, group_type, Measure)

pvals <- combos %>%
  mutate(p_value = pmap_dbl(list(shot_base, group_type, Measure),
                            function(sb, gt, meas) {
                              sub <- df_melt %>%
                                filter(shot_base == sb, group_type == gt, Measure == meas)
                              
                              # skip if not enough variation
                              if(n_distinct(sub$batch_id) < 2 ||
                                 length(unique(sub$Label)) < 2) return(1)
                              
                              fit <- tryCatch(
                                glmer(Label ~ tweet_in_batch + (1 | batch_id),
                                      data = sub, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa")),
                                error = function(e) NULL
                              )
                              if(is.null(fit)) return(1)
                              
                              p <- summary(fit)$coefficients["tweet_in_batch", "Pr(>|z|)"]
                              p
                            }))

# ──────────────────────────────────────────────────────────────
# 5.  SHADE FACETS WHERE EITHER OL OR HS IS SIGNIFICANT
# ──────────────────────────────────────────────────────────────
sig_panels <- pvals %>%
  group_by(shot_base, group_type) %>%
  summarise(sig = any(p_value < 0.05/12), .groups = "drop") %>%
  filter(sig)

if(nrow(sig_panels)) {
  g <- g +
    geom_rect(data = sig_panels,
              inherit.aes = FALSE,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = "transparent",
              color = "black",
              linewidth = 2) +
    theme(panel.spacing = unit(0.6, "lines"))
}
g

# ──────────────────────────────────────────────────────────────
# 6.  ADD ASTERISKS FOR EACH SIGNIFICANT MEASURE
# ──────────────────────────────────────────────────────────────
asterisks <- pvals %>%
  filter(p_value < 0.05/12) %>%
  mutate(y = c(0.90, 0.82)[match(Measure, c("OL","HS"))],
         label = "*")

g <- g +
  geom_text(data = asterisks,
            inherit.aes = FALSE,
            aes(x = Inf, y = y, label = label, 
                colour = Measure),
            hjust = 1.5, vjust = 0.4, size = 10,
            fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_fill_manual(values = palette, guide = "none") + 
  ggtitle("Effect of Position on OL/HS Proportions (asterisks = GLMM slope p < 0.05/24; random intercept = batch_id)")

g





################################################################################
### PLOT QUADRATIC EFFECTS (BATCH CONDITIONS ONLY)
################################################################################
# ──────────────────────────────────────────────────────────────
# 1.  DATA PREPARATION   (melt, filter, derive facet keys)
# ──────────────────────────────────────────────────────────────
df_melt <- df_long %>% filter(grepl('batch', condition)) %>% 
  pivot_longer(cols = c(OL, HS),
               names_to = "Measure",
               values_to = "Label") %>%
  select(batch_id, condition, tweet_in_batch, shot, Measure, Label) %>%
  ## 1b) drop C.OL↔HS and C.HS↔OL mismatches
  filter(!(str_detect(condition, "C\\.OL") & Measure == "HS") &
           !(str_detect(condition, "C\\.HS") & Measure == "OL")) %>%
  ## 1c) strip “.OL” / “.HS”
  mutate(condition = str_remove(condition, "\\.OL|\\.HS")) %>%
  ## 1d) derive facet columns
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = paste0(shot, "_", base_condition)
  ) %>%
  mutate(
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

row_order <- c(paste0("few-shot_" , LETTERS[1:3]))

df_melt <- df_melt %>%
  mutate(shot_base = factor(shot_base, levels = row_order))

# ──────────────────────────────────────────────────────────────
# 2.  PROPORTIONS FOR PLOT
# ──────────────────────────────────────────────────────────────
df_prop <- df_melt %>%
  group_by(shot, condition, tweet_in_batch, Measure) %>%
  summarise(Proportion = mean(Label, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group_type = case_when(
      !str_detect(condition, "_")       ~ "base",
      str_ends(condition, "_batch_conf")~ "batch_conf",
      str_ends(condition, "_conf")      ~ "conf",
      str_ends(condition, "_batch")     ~ "batch",
      TRUE                              ~ "other"
    ),
    base_condition = str_split_fixed(condition, "_", 2)[,1],
    shot_base = factor(paste0(shot, "_", base_condition),
                       levels = row_order),
    group_type = factor(group_type,
                        levels = c("base","conf","batch","batch_conf"))
  )

# ──────────────────────────────────────────────────────────────
# 3.  FACETTED OLS LINES (ggplot2)
# ──────────────────────────────────────────────────────────────
palette <- c(OL = "#1f77b4", HS = "#ff7f0e")

g <- ggplot(df_prop,
            aes(tweet_in_batch, Proportion, 
                colour = Measure,
                fill = Measure)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_fill_manual(values = palette, guide = "none") + 
  facet_grid(rows = vars(shot_base), cols = vars(group_type)) +
  labs(x = "Tweet Position in Batch",
       y = "Proportion") +
  theme_bw()
g

# ──────────────────────────────────────────────────────────────
# 4.  GLMMs (binomial logit)  — one per facet × Measure
#     Formula: Label ~ tweet_in_batch + (1 | batch_id)
# ──────────────────────────────────────────────────────────────
combos <- df_melt %>%
  distinct(shot_base, group_type, Measure)

pvals <- combos %>%
  mutate(p_value = pmap_dbl(list(shot_base, group_type, Measure),
                            function(sb, gt, meas) {
                              sub <- df_melt %>%
                                filter(shot_base == sb, group_type == gt, Measure == meas)
                              
                              # skip if not enough variation
                              if(n_distinct(sub$batch_id) < 2 ||
                                 length(unique(sub$Label)) < 2) return(1)
                              
                              fit <- tryCatch(
                                glmer(Label ~ tweet_in_batch + I(tweet_in_batch^2) + (1 | batch_id),
                                      data = sub, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa")),
                                error = function(e) NULL
                              )
                              if(is.null(fit)) return(1)
                              
                              p  <- summary(fit)$coefficients["I(tweet_in_batch^2)", "Pr(>|z|)"]
                              p
                            }))

pvals

# ──────────────────────────────────────────────────────────────
# 5.  SHADE FACETS WHERE EITHER OL OR HS IS SIGNIFICANT
# ──────────────────────────────────────────────────────────────
sig_panels <- pvals %>%
  group_by(shot_base, group_type) %>%
  summarise(sig = any(p_value < 0.05 / 12), .groups = "drop") %>%
  filter(sig)

if(nrow(sig_panels)) {
  g <- g +
    geom_rect(data = sig_panels,
              inherit.aes = FALSE,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              fill = "transparent", 
              color = "black",
              linewidth = 2) +
    theme(panel.spacing = unit(0.6, "lines"))
}
g

# ──────────────────────────────────────────────────────────────
# 6.  ADD ASTERISKS FOR EACH SIGNIFICANT MEASURE
# ──────────────────────────────────────────────────────────────
asterisks <- pvals %>%
  filter(p_value < 0.05/12) %>%
  mutate(y = c(0.90, 0.82)[match(Measure, c("OL","HS"))],
         label = "*")

g <- g +
  geom_text(data = asterisks,
            inherit.aes = FALSE,
            aes(x = Inf, y = y, label = label, colour = Measure),
            hjust = 1.5, vjust = -0.1, size = 10,
            fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = palette, name = NULL) +
  ggtitle("Effect of Position on OL/HS Proportions (asterisks = GLMM slope p < 0.05/12; random intercept = batch_id)")

g

