rm(list = ls())

# load packages
library(tidyverse) 

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

# EXCLUDE P = .000 -------------------------------------------------------------

data %>%
  filter(error == TRUE & reported_p == 0) %>%
  select(raw)

# INCONSISTENCY DETAILS --------------------------------------------------------

data %>%
  filter(journal == "PS" & period == 1 & decision_error == TRUE) %>%
  as_tibble %>%
  select(source, raw, computed_p, error, decision_error)

# AT LEAST ONE INCONSISTENCY ---------------------------------------------------

data_per_article %>%
  filter(!is.na(period)) %>%
  mutate(at_least_one_incons = ifelse(nr_errors > 0, 1, 0),
         at_least_one_dec_incons = ifelse(nr_dec_errors > 0, 1, 0)) %>%
  group_by(period, statcheck_journal) %>%
  summarize(perc_1_error = round(sum(at_least_one_incons) / n() * 100, 1),
            perc_1_dec_error = round(sum(at_least_one_dec_incons)/n()*100, 1))
  