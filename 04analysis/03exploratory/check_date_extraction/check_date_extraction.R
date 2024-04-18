rm(list = ls())

library(dplyr)    # data wrangling
library(magrittr) # pipe

set.seed(240414)

# LOAD DATA --------------------------------------------------------------------

# data for all articles, incl those without extracted statistics
data_per_article_all <- read.table("03data/2023-06-15data_per_article_with_missings.txt", header = TRUE)

# SELECT RANDOM SAMPLE TO CHECK DATES ------------------------------------------

# select random sample
sampled_rows <- sample(seq(nrow(data_per_article_all)), size = 50, replace = FALSE)

# create empty data frame to manually check dates
sampled_articles <- data_per_article_all[sampled_rows, ]

df_sample <- sampled_articles %>%
  select(source, journal, date_received, year_published)

write.table(df_sample, "04analysis/03exploratory/check_date_extraction_empty.txt",
            sep = "\t", row.names = FALSE)
