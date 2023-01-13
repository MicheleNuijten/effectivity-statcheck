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

# TABLE 1 ----------------------------------------------------------------------

# nr of downloads based on nr of files in directory

# function to list all articles in a directory while excluding additional 
# html files/folders
count_articles <- function(dir){
  
  subdirs <- list.dirs(dir, recursive = FALSE)
  nr_files <- rep(NA, length(subdirs))
  
  # if the journal is JPSP, exclude 2003-2013
  # these folders include too many articles
  # nr of downloads for these years needs to be assessed by looking at
  # asc titles
  
  if(grepl("jpsp", dir, ignore.case = TRUE)){
    incl_years <- paste0("(", paste(2014:2022, collapse = ")|("), ")")
    subdirs <- subdirs[grepl(incl_years, subdirs)]
  }
  
  for(subdir in seq_along(subdirs)){
    journal_year_dir <- subdirs[subdir]
    
    nr_files[subdir] <- length(list.files(journal_year_dir, pattern = "*.html?",
                                          recursive = FALSE))
  }
  
  nr_downloads <- sum(nr_files, na.rm = TRUE)
  
  return(nr_downloads)
  
}

# JEPG 
(nr_downloads_JEPG <- count_articles("./01articles/JEPG"))
# 1826

# JESP
(nr_downloads_JESP <- count_articles("./01articles/JESP"))
# 2557

# PS
(nr_downloads_PS <- count_articles("./01articles/PS"))
# 3851

# JPSP
# for JPSP it's slightly more complicated, because we only include articles from
# the section ASC, and in the previously downloaded sample (2003-2013) we included
# everything. To count the nr of downloads, use the list of included titles
nr_downloads_JPSP_period0 <- nrow(asc_titles_df)
nr_downloads_JPSP_period1 <- count_articles("./01articles/JPSP")

(nr_downloads_JPSP <- sum(nr_downloads_JPSP_period0, nr_downloads_JPSP_period1))
# 716

# combine in df
df_nr_downloads <- data.frame(journal = c("JEPG", "JESP", "JPSP", "PS"),
                              nr_downloads = c(nr_downloads_JEPG, nr_downloads_JESP,
                                               nr_downloads_JPSP, nr_downloads_PS))

# how many articles per journal have a date received?
df_date_received <- data_per_article_all %>%
  filter(!is.na(period)) %>%
  group_by(journal) %>%
  summarize(articles_with_date = n()) 

# combine dfs
df_nr_articles <- 
  full_join(df_nr_downloads, df_date_received, by = "journal") %>%
  mutate(perc_with_date = round(articles_with_date/nr_downloads*100, 1)) 

# create a factor variable with the desired level order
journal_factor <- factor(df_nr_articles$journal, 
                         levels = c("PS", "JESP", "JEPG", "JPSP"))

# reorder the rows based on the factor variable
(df_nr_articles <- df_nr_articles %>% arrange(journal_factor))

# summarize per journal type
(df_nr_articles_totals <- df_nr_articles %>%
    mutate(journal = factor(ifelse(df_nr_articles$journal %in% c("PS", "JESP"), 
                                   "statcheck_total", "control_total"))) %>%
    group_by(journal) %>%
    summarize(nr_downloads = sum(nr_downloads),
              articles_with_date = sum(articles_with_date),
              perc_with_date = round(articles_with_date/nr_downloads*100,1)))

# combine tables 
(df_nr_articles_all <- 
    full_join(df_nr_articles, df_nr_articles_totals) %>%
    mutate(journal = 
             factor(journal, levels = c("PS", "JESP", "statcheck_total",
                                        "JEPG", "JPSP", "control_total"))) %>% 
    arrange(journal))


# copy to clipboard
clipr::write_clip(df_nr_articles_all)

# TABLE 2 ----------------------------------------------------------------------

# nr of available articles
(table2a <- data_per_article_all %>%
   filter(!is.na(period)) %>%
   group_by(journal, period) %>%
   summarize(articles_with_date = n()))

# descriptives of scraped articles
(table2b <- data_per_article %>%
    filter(!is.na(period)) %>%
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
    filter(!is.na(period)) %>%
    group_by(statcheck_journal, period) %>%
    summarize(articles_with_date = n()) %>%
    mutate(journal = ifelse(statcheck_journal == 0, "control_total", 
                            "statcheck_total"))
)

# descriptives of scraped articles
(table2d <- data_per_article %>%
    filter(!is.na(period)) %>%
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

# save dataframe
write.table(table2, "04analysis/01descriptives/table2.txt", row.names = FALSE)

# VIOLIN PLOT ------------------------------------------------------------------

library(viridis)
library(cowplot)

# create summary dataframe
violin_data <- data_per_article %>%
  filter(!is.na(period)) %>%
  mutate(statcheck_journal = factor(statcheck_journal),
         period = factor(period)) %>%
  group_by(statcheck_journal, period)

melt_data <- 
  violin_data %>%
  reshape2::melt(id.vars = c("statcheck_journal", "period"), 
                 measure.vars = c("perc_errors", "perc_dec_errors"), 
                 variable.name = "error_type", value.name = "error_percentage")

# facet wrap violin plot
violinplot <- melt_data %>%  
  ggplot(aes(x = statcheck_journal, y = error_percentage, fill = period)) +
  geom_violin(position = position_dodge(width = 0.7), alpha=0.5) +
  geom_boxplot(position = position_dodge(width = 0.7), 
               width=0.1, color="grey", alpha=0.2) +
  facet_wrap(error_type ~ ., ncol = 2, 
             labeller = 
               labeller(error_type = 
                          c("perc_errors" = "Inconsistencies",
                            "perc_dec_errors" = "Decision Inconsistencies" ))) +
  scale_fill_viridis(discrete = TRUE,
                     labels = c("0" = "Before statcheck implementation",
                                "1" = "After statcheck implementation")) +
  labs(x = "", y = "", fill = "Time Period",
       title = 
         "Mean % of (decision) inconsistencies per article with NHST results",
       subtitle = "A.") + 
  scale_x_discrete(labels =
                     c("0" ="Control Journal", "1" = "Statcheck Journal")) +
  scale_y_continuous( labels = function(x) paste0(x, "%")) +
  theme(legend.position = "")

# truncated y-axes
violinplot_trunc <- melt_data %>%  
  ggplot(aes(x = statcheck_journal, y = error_percentage, fill = period)) +
  geom_violin(position = position_dodge(width = 0.7), alpha=0.5) +
  geom_boxplot(position = position_dodge(width = 0.7), 
               width=0.1, color="grey", alpha=0.2) +
  facet_wrap(error_type ~ ., ncol = 2, 
             labeller = 
               labeller(error_type = 
                          c("perc_errors" = "Inconsistencies",
                            "perc_dec_errors" = "Decision Inconsistencies" ))) +
  scale_fill_viridis(discrete = TRUE,
                     labels = c("0" = "Before statcheck implementation",
                                "1" = "After statcheck implementation")) +
  labs(x = "Type of Journal", y = "", fill = "Time Period",
       subtitle = "B. (truncated y-axis)") + 
  scale_x_discrete(labels = 
                     c("0" ="Control Journal", "1" = "Statcheck Journal")) +
  scale_y_continuous( labels = function(x) paste0(x, "%")) +
  coord_cartesian(y = c(0, 25)) +
  theme(legend.position = "bottom")

cowplot::plot_grid(violinplot, violinplot_trunc, nrow = 2)

ggsave("04analysis/01descriptives/violin_plots.png", 
       width = 6.5, height = 5.5, units = "in")

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

ggsave("04analysis/01descriptives/line_graph_means.png", 
       width = 10, height = 8)
