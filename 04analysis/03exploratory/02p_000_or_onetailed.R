rm(list = ls())

# load packages
library(tidyverse)

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

# EXCLUDE P = .000 -------------------------------------------------------------

### EDIT:
# the strategy below is too lenient. better to rerun statcheck on these cases
# to see if the p-value could be rounded to .000


# count p=.000 as correct
data_p00_no_error <- data %>%
  mutate(error = ifelse(error == TRUE & reported_p == 0, FALSE, error))

# ORGANIZE DATA AT ARTICLE LEVEL -----------------------------------------------

# organize data on article level
data_per_article_p00_no_error <- 
  data_p00_no_error %>%
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

data_per_article_p00_no_error %>%
  filter(!is.na(period)) %>%
  group_by(journal, period) %>%
  summarize(articles_with_nhst = n(),
            median_nhst_per_article = median(nr_nhst),
            mean_perc_errors = round(mean(perc_errors), 1),
            mean_perc_dec_errors = round(mean(perc_dec_errors), 1))

# RECREATE TABLE 2 -------------------------------------------------------------

# descriptives of scraped articles
table2a <- data_per_article_p00_no_error %>%
    filter(!is.na(period)) %>%
    group_by(journal, period) %>%
    summarize(articles_with_nhst = n(),
              median_nhst_per_article = median(nr_nhst),
              mean_perc_errors = round(mean(perc_errors), 1),
              mean_perc_dec_errors = round(mean(perc_dec_errors), 1))

# calculate totals per journal type
table2b <- data_per_article_p00_no_error %>%
    filter(!is.na(period)) %>%
    group_by(statcheck_journal, period) %>%
    summarize(articles_with_nhst = n(),
              median_nhst_per_article = median(nr_nhst),
              mean_perc_errors = round(mean(perc_errors), 1),
              mean_perc_dec_errors = round(mean(perc_dec_errors), 1)) %>%
    mutate(journal = ifelse(statcheck_journal == 0, "control_total", 
                            "statcheck_total"))

# combine tables
(table2 <- full_join(table2a, table2b) %>%
    select(-statcheck_journal) %>%
  mutate(journal = 
           factor(journal, levels = c("PS", "JESP", "statcheck_total",
                                      "JEPG", "JPSP", "control_total"))) %>% 
    arrange(journal))

# save dataframe
write.table(table2, "04analysis/03exploratory/table2_p000.txt", row.names = FALSE)

# LINE GRAPH OF MEANS ----------------------------------------------------------

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
  select(-(articles_with_nhst:median_nhst_per_article))

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
       title = "Mean % of (decision) inconsistencies per article with NHST results",
       subtitle = "Cases with p </= .000 are counted as correct") +
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

ggsave("04analysis/03exploratory/line_graph_means_p000.png", 
       width = 10, height = 8)


# RUN LM -----------------------------------------------------------------------

lm_errors_p000 <- glmer(error ~ period*statcheck_journal + (1|source), 
                   data = data_p00_no_error, family = binomial(link = "logit"))

summary(lm_errors_p000)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -2.75363    0.03957 -69.583   <2e-16 ***
#   period                    0.04986    0.06737   0.740   0.4592    
# statcheck_journal         0.05009    0.04705   1.065   0.2871    
# period:statcheck_journal -0.20566    0.09036  -2.276   0.0229 *
