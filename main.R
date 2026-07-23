################################################################################
### Setup
################################################################################

library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

source("run_analysis_new.R")
source("run_inference_analysis.R")

################################################################################
### Load Kern Data
################################################################################

folder_path <- "Tweets_CK"

train_df <- read_csv(file.path(folder_path, "full_train_s.csv"))
test_df  <- read_csv(file.path(folder_path, "full_test_s.csv"))

test_df <- test_df %>% select(all_of(names(train_df)))

tweets_full <- bind_rows(
  train_df %>% mutate(original_split = "train"),
  test_df  %>% mutate(original_split = "test")
) %>%
  rename(
    tweet_id = `tweet.id`,
    hate_speech = `hate.speech`,
    offensive_language = `offensive.language`
  )

summary <- tweets_full %>%
  summarise(
    OL = mean(offensive_language, na.rm = TRUE),
    HS = mean(hate_speech, na.rm = TRUE)
  )

final_summary <- tibble(
  version = "overall",
  OL = summary$OL,
  HS = summary$HS
)

################################################################################
### Load Models
################################################################################

load_model <- function(path, model_name) {
  files <- list.files(path, full.names = TRUE)
  
  map_df(files, read_csv) %>%
    arrange(batch_id, tweet_in_batch) %>%
    mutate(
      shot = "few-shot",
      model = model_name
    )
}

gpt4   <- load_model("Data_Collection/Models/GPT4o_mini", "gpt4o_mini")
gpt54_mini   <- load_model("Data_Collection/Models/GPT5.4_mini", "gpt54_mini")
gpt54  <- load_model("Data_Collection/Models/GPT5.4", "gpt5.4")
llama4 <- load_model("Data_Collection/Models/Llama4", "llama4")
mistral<- load_model("Data_Collection/Models/MistralLarge3", "mistral")

################################################################################
### Run Prevalence Analyses
################################################################################

source("run_analysis_new.R")

results_gpt4 <- run_full_analysis(
  combined_df = gpt4,
  model_name = "gpt4o_mini",
  tweets_full = tweets_full,
  final_summary = final_summary
)

results_gpt4 <- run_full_analysis(
  combined_df = gpt54_mini,
  model_name = "gpt54_mini",
  tweets_full = tweets_full,
  final_summary = final_summary
)

results_gpt54 <- run_full_analysis(
  combined_df = gpt54,
  model_name = "gpt5.4",
  tweets_full = tweets_full,
  final_summary = final_summary
)

results_llama4 <- run_full_analysis(
  combined_df = llama4,
  model_name = "llama4",
  tweets_full = tweets_full,
  final_summary = final_summary
)

results_mistral <- run_full_analysis(
  combined_df = mistral,
  model_name = "mistral",
  tweets_full = tweets_full,
  final_summary = final_summary
)


df_prevalence_all_models <- bind_rows(
  results_gpt4$prevalence_data,
  results_llama4$prevalence_data,
  results_mistral$prevalence_data,
  results_gpt54$prevalence_data
) %>%
  mutate(
    model = factor(
      model_name,
      levels = c("gpt4o_mini", "llama4", "mistral", "gpt5.4"),
      labels = c("GPT-4o mini", "Llama 4", "Mistral Large 3", "GPT-5.4")
    )
  )

combined_prevalence_plot <- ggplot(
  df_prevalence_all_models,
  aes(x = condition_label, y = Value_Pct, color = group_type)
) +
  
  geom_hline(
    data = ref_lines,
    aes(
      yintercept = intercept,
      linetype = "Reference"
    ),
    color = "gray50",
    linewidth = 0.35,
    show.legend = TRUE
  ) +
  
  geom_point(
    position = position_dodge(width = 0.55),
    size = 1.6
  ) +
  
  facet_grid(
    model ~ Measure,
    scales = "fixed",
    labeller = labeller(
      Measure = c(OL = "OL", HS = "HS")
    )
  ) +
  
  scale_color_manual(
    name = "Condition",
    values = c(
      "base"       = "#443983",
      "conf"       = "#31688e",
      "batch"      = "#35b779",
      "batch_conf" = "#90d743"
    ),
    labels = c(
      "base"       = "Base",
      "conf"       = "+ Conf.",
      "batch"      = "Batch",
      "batch_conf" = "Batch + Conf."
    )
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c("Reference" = "dashed")
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 100),
    expand = expansion(mult = c(0.03, 0.05)),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    x = "Task Structure",
    y = "Prevalence"
  ) +
  
  guides(
    color = guide_legend(order = 1, nrow = 2),
    linetype = guide_legend(order = 2)
  ) +
  
  theme_bw(base_size = 8) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 7.5, face = "bold"),
    
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 6.5),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.2, color = "gray85"),
    panel.border = element_rect(color = "gray75", linewidth = 0.3),
    
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 6.5, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 6.5),
    
    panel.spacing = unit(0.3, "lines"),
    
    plot.margin = margin(t = 2, r = 2, b = 12, l = 2)
  )

ggsave(
  "Plots/combined_prevalences_all_models_acl_halfpage.pdf",
  combined_prevalence_plot,
  device = cairo_pdf,
  width = 8.8,
  height = 14,
  units = "cm"
)
  


################################################################################
### RUN INDIVIDUAL LPM TABLES FOR APPENDIX
################################################################################

source("run_inference_analysis_v2.R")

results_gpt54mini_lpm <- run_inference_analysis(
  combined_df = gpt54_mini,
  model_name = "gpt5.4_mini"
)
results_gpt54mini_lpm$paper_table

results_gpt54_lpm <- run_inference_analysis(
  combined_df = gpt54,
  model_name = "gpt5.4"
)
results_gpt54_lpm$paper_table

results_llama4_lpm <- run_inference_analysis(
  combined_df = llama4,
  model_name = "llama4"
)
results_llama4_lpm$paper_table

results_mistral_lpm <- run_inference_analysis(
  combined_df = mistral,
  model_name = "mistral"
)
results_mistral_lpm$paper_table
)