rm(list = ls())

library(tidyverse)

# load raw data (on statistical result level)
data <- read.table("03data/2024-04-15scraped_articles_v3.txt", header=TRUE, na.strings="NA")

# treat dates as such
data$date_received <- as.Date(data$date_received)

#-------------------------------------------------------------------------------

# add key variables

# time period: 
# for PS and comparison journal JEPG, period == 1 starts after July 2016
# for JESP and comparison journal JPSP, period == 1 starts after August 2017
data$period <- ifelse(((data$journal == "PS" | data$journal == "JEPG") & 
                        data$date_received <= as.Date("2016-07-01")) | 
                       (data$journal == "JESP" | data$journal == "JPSP") & 
                       data$date_received <= as.Date("2017-08-01"), 
                     yes = 0, no = 1)

# statcheck journal:
# PS and JESP introduced statcheck in their peer review process, the comparison
# journals JEPG and JPSP did not
data$statcheck_journal <- ifelse(data$journal == "PS" | data$journal == "JESP",
                                yes = 1, no = 0)

# save
write.table(data, paste0("03data/", Sys.Date(), "data_wrangled_with_missings.txt"), row.names = FALSE)

# remove articles without extracted stats
data_nomissing <- data %>%
  filter(!is.na(raw))

# save
write.table(data_nomissing, paste0("03data/", Sys.Date(), "data_wrangled_no_missings.txt"), row.names = FALSE)


#-------------------------------------------------------------------------------

# organize data on article level for analyses

data_per_article_w_missings <- 
  data %>%
  group_by(source) %>%
  summarize(journal = first(journal),
            statcheck_journal = first(statcheck_journal),
            year_published = first(year_published),
            date_received = first(date_received),
            period = first(period),
            nr_nhst = ifelse(is.na(first(raw)), 0, n()),
            nr_errors = sum(error, na.rm = TRUE),
            nr_dec_errors = sum(decision_error, na.rm = TRUE),
            perc_errors = round(nr_errors/nr_nhst*100, 2),
            perc_dec_errors = round(nr_dec_errors/nr_nhst*100, 2))

# save data frame
write.table(data_per_article_w_missings, 
            paste0("03data/", Sys.Date(), "data_per_article_with_missings.txt"), row.names = FALSE)


data_per_article <- 
  data_nomissing %>%
  group_by(source) %>%
  summarize(journal = first(journal),
            statcheck_journal = first(statcheck_journal),
            year_published = first(year_published),
            date_received = first(date_received),
            period = first(period),
            nr_nhst = n(),
            nr_errors = sum(error, na.rm = TRUE),
            nr_dec_errors = sum(decision_error, na.rm = TRUE),
            perc_errors = round(nr_errors/nr_nhst*100, 2),
            perc_dec_errors = round(nr_dec_errors/nr_nhst*100, 2))

# save data frame
write.table(data_per_article, 
            paste0("03data/", Sys.Date(), "data_per_article_with_stats.txt"), row.names = FALSE)
