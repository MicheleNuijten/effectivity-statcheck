rm(list = ls())

# load packages
library(tidyverse) 

# LOAD DATA --------------------------------------------------------------------

# data for all articles, incl those without extracted statistics
data_all <- read.table("03data/2023-01-06data_wrangled_with_missings.txt", header = TRUE)
data_per_article_all <- read.table("03data/2023-01-06data_per_article_with_missings.txt", header = TRUE)

# data for articles with statistics
data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

# FUNCTION FOR PLOT ------------------------------------------------------------

# create general function for the type of plot we're making to save time and
# avoid errors when changing parameters

make_plot <- function(data, y, color = NULL, perc = TRUE, 
                      title = NULL, ylab = NULL){
  data %>%
    ggplot(aes(x = year_published, y = y, color = color)) +
    geom_line() +
    geom_point() +
    {if(perc) scale_y_continuous( labels = function(x) paste0(x, "%"))} +
    facet_wrap(~journal) +
    labs(title = title, 
         x = 'Publication Year',
         y = ylab)
  
}

# PERC W NHST OVER TIME --------------------------------------------------------

perc_nhst <- data_per_article_all %>%
  mutate(has_nhst = ifelse(nr_nhst > 0, 1, 0)) %>%
  group_by(journal, year_published) %>%
  summarize(perc_nhst = sum(has_nhst / n())* 100)

make_plot(data = perc_nhst, y = perc_nhst$perc_nhst,
          title = "% Articles in which statcheck detected NHST results")

# NR NHST OVER TIME ------------------------------------------------------------

mean_nr_nhst <- data_per_article %>%
  group_by(journal, year_published) %>%
  summarize(mean_nhst = mean(nr_nhst, na.rm = TRUE))

make_plot(data = mean_nr_nhst, y = mean_nr_nhst$mean_nhst, perc = FALSE,
          title = "Mean # NHST results in an article for article with NHST results")

# PERC (DEC) INCONS OVER TIME --------------------------------------------------

perc_errors <- data_per_article %>%
  group_by(journal, year_published) %>%
  summarize(mean_errors = mean(perc_errors),
            mean_dec_errors = mean(perc_dec_errors)) %>%
  pivot_longer(c(mean_errors, mean_dec_errors), names_to = "type_error", 
               values_to = "perc")

make_plot(data = perc_errors, y = perc_errors$perc, 
          color = perc_errors$type_error,
          title = "Mean % (decision) inconsistencies over time")

# PERC ARTICLES W INCONS OVER TIME ---------------------------------------------

articles_w_errors <- data_per_article %>%
  mutate(has_error = ifelse(nr_errors > 0, 1, 0),
         has_dec_error = ifelse(nr_dec_errors > 0, 1, 0)) %>%
  group_by(journal, year_published) %>%
  summarize(perc_art_error = sum(has_error)/n()*100,
            perc_art_dec_error = sum(has_dec_error)/n()*100) %>%
  pivot_longer(c(perc_art_error, perc_art_dec_error), names_to = "type_error",
               values_to = "perc")

make_plot(data = articles_w_errors, y = articles_w_errors$perc, 
          color = articles_w_errors$type_error,
          title = "% Articles with at least one (decision) inconsistency")

