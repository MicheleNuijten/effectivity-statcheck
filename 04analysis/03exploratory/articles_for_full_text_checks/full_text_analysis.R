rm(list = ls())

# load packages
library(tidyverse) 

# LOAD DATA --------------------------------------------------------------------

# data for articles with statistics
data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

# SELECT ARTICLES --------------------------------------------------------------

# make sure to draw the same sample each time
set.seed(06022023)

sample <- data_per_article %>% 
  filter(!is.na(period),
         nr_errors > 0) %>%
  group_by(journal, period) %>%
  sample_n(size = 10)

articles <- sample$source

sample_stats <- data %>%
  filter(source %in% articles) %>%
  # add extra columns for manual coding
  mutate(reason = NA,
         remarks = NA)

write.table(sample_stats, 
            "04analysis/03exploratory/articles_for_full_text_checks/manual_checks_errors.txt",
            row.names = FALSE,
            sep = ";")

# DESCRIPTIVES FROM SELECTED ARTICLES ------------------------------------------

coded_stats <- 
  read.csv("04analysis/03exploratory/articles_for_full_text_checks/manual_checks_errors.csv", 
           sep=";", header = TRUE)

coded_stats %>%
  filter(period == 1 & statcheck_journal == 1) %>%
  summarize(total_nhst = n(),
            total_errors = sum(error),
            perc_errors = round(total_errors / total_nhst * 100, 1),
            total_dec_errors = sum(decision_error),
            perc_dec_errors = round(total_dec_errors / total_nhst * 100, 1))

coded_stats %>%
  filter(period == 1 & statcheck_journal == 1 & error == TRUE) %>%
  count(journal, reason) %>%
  group_by(journal) %>%
  mutate(prop = prop.table(n())) %>%
  arrange(desc(journal))
