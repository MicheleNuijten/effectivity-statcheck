rm(list = ls())

library(tidyverse)

# LOAD DATA --------------------------------------------------------------------

# data for all articles, incl those without extracted statistics
data_all <- read.table("03data/2023-01-06data_wrangled_with_missings.txt", header = TRUE)
data_per_article_all <- read.table("03data/2023-01-06data_per_article_with_missings.txt", header = TRUE)

# data for articles with statistics
data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

# load list of titles of JPSP articles 2003-2013 in the ASC subsection
asc_titles_df <- 
  as.data.frame(readxl::read_excel("01articles/overview_jpsp_asc_2003-2013.xlsx"))

# TABLE 2 WITHOUT JESP 2019 ----------------------------------------------------

# nr of available articles
(table2a <- data_per_article_all %>%
    filter(!is.na(period) & 
             !(journal == "JESP" & year_published == 2019)) %>%
    group_by(journal, period) %>%
    summarize(articles_with_date = n()))

# descriptives of scraped articles
(table2b <- data_per_article %>%
    filter(!is.na(period) & 
             !(journal == "JESP" & year_published == 2019)) %>%
    group_by(journal, period) %>%
    summarize(articles_with_nhst = n(),
              median_nhst_per_article = median(nr_nhst),
              mean_perc_errors = round(mean(perc_errors), 1),
              mean_perc_dec_errors = round(mean(perc_dec_errors), 1)))

# combine tables
(table2x <- full_join(table2a, table2b, by = c("journal", "period")) %>%
    mutate(perc_articles_with_nhst = round(articles_with_nhst/articles_with_date*100, 1)) %>%
    select(journal, period, articles_with_date, articles_with_nhst, perc_articles_with_nhst, 
           median_nhst_per_article:mean_perc_dec_errors))

# calculate totals per journal type

# nr of available articles
(table2c <- data_per_article_all %>%
    filter(!is.na(period) & 
             !(journal == "JESP" & year_published == 2019)) %>%
    group_by(statcheck_journal, period) %>%
    summarize(articles_with_date = n()) %>%
    mutate(journal = ifelse(statcheck_journal == 0, "control_total", 
                            "statcheck_total"))
)

# descriptives of scraped articles
(table2d <- data_per_article %>%
    filter(!is.na(period) & 
             !(journal == "JESP" & year_published == 2019)) %>%
    group_by(statcheck_journal, period) %>%
    summarize(articles_with_nhst = n(),
              median_nhst_per_article = median(nr_nhst),
              mean_perc_errors = round(mean(perc_errors), 1),
              mean_perc_dec_errors = round(mean(perc_dec_errors), 1)) %>%
    mutate(journal = ifelse(statcheck_journal == 0, "control_total", 
                            "statcheck_total")))

# combine tables
(table2total <- full_join(table2c, table2d, by = c("journal", "period")) %>%
    mutate(perc_articles_with_nhst = round(articles_with_nhst/articles_with_date*100, 1),
           journal = 
             factor(journal, levels = c("statcheck_total", "control_total"))) %>%
    arrange(journal) %>%
    select(journal, period, articles_with_date, articles_with_nhst, perc_articles_with_nhst, 
           median_nhst_per_article:mean_perc_dec_errors, -statcheck_journal.y))

# copy to clipboard
clipr::write_clip(table2total)

# combine with table above
(table2 <- full_join(table2x, table2total) %>%
    mutate(journal = 
             factor(journal, levels = c("PS", "JESP", "statcheck_total",
                                        "JEPG", "JPSP", "control_total"))) %>% 
    arrange(journal)
)

# copy to clipboard
clipr::write_clip(table2)

# LINE GRAPH OF MEANS WITHOUT JESP 2019 ----------------------------------------

line_data <- 
  table2 %>%
  mutate(is_total = factor(ifelse(journal %in% c("statcheck_total", "control_total"), 
                                  "yes", "no")),
         is_statcheck_journal = factor(ifelse(journal %in% c("JESP", "PS", "statcheck_total"),
                                              "yes", "no")),
         journal = gsub("_total", "\ntotal", journal),
         period = factor(period),
         pair = dplyr::case_when(
           grepl("total", journal) ~ "1. Total",
           journal %in% c("PS", "JEPG") ~ "2. General Psychology",
           journal %in% c("JESP", "JPSP") ~ "3. Social Psychology", 
           TRUE ~ NA_character_
         )) %>%
  select(-(articles_with_date:median_nhst_per_article))

## when using facet_wrap instead of cowplot:
# melt_line_data <-
#   line_data %>%
#   reshape2::melt(id.vars = c("journal", "period", "is_total", "is_statcheck_journal", "pair"),
#                  measure.vars = c("mean_perc_errors", "mean_perc_dec_errors"),
#                  variable.name = "error_type", value.name = "error_percentage")

line_error <- line_data %>%
  ggplot(aes(x = period, y = mean_perc_errors, group = journal,
             size = ifelse(is_total == "yes", 1.5, 1))) +
  geom_point() +
  geom_line(aes(lty = ifelse(is_statcheck_journal == "yes", "solid", "dashed"))) +
  geom_label(data = . %>% filter(period == 0), aes(label = journal), 
             hjust = 1.1, size = 3) +
  scale_linetype_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 12)) +
  scale_x_discrete(labels = c("before", "after")) +
  scale_size_identity() +
  labs(x = NULL, y = "Inconsistencies",
       title = "Mean % of (decision) inconsistencies per article with NHST results") +
  facet_grid(.~pair)

line_dec_error <- line_data %>%
  ggplot(aes(x = period, y = mean_perc_dec_errors, group = journal,
             size = ifelse(is_total == "yes", 1.5, 1))) +
  geom_point() +
  geom_line(aes(lty = ifelse(is_statcheck_journal == "yes", "solid", "dashed"))) +
  geom_label(data = . %>% filter(period == 0), aes(label = journal), 
             hjust = 1.1, size = 3) +
  scale_linetype_identity() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 2)) +
  scale_x_discrete(labels = c("before", "after")) +
  scale_size_identity() +
  labs(x = "Before/After statcheck implementation", y = "Decision Inconsistencies") +
  facet_grid(.~pair)

cowplot::plot_grid(line_error, line_dec_error, nrow = 2)
