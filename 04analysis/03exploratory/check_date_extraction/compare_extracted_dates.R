rm(list = ls())

library(dplyr)
library(magrittr)

old <- read.table("03data/2023-06-15data_per_article_with_missings.txt",
                  header = TRUE)
new <- read.table("03data/2024-04-15data_per_article_with_missings.txt",
                  header = TRUE)

old <- old %>% select(source, journal, date_received, year_published)
new <- new %>% select(source, journal, date_received, year_published)

date_match <- old$date_received == new$date_received

merged <- left_join(old, new, by = "source")

no_match <- merged[!is.na(date_match) & !date_match, ]

table(no_match$journal.x)

table(new$journal)
