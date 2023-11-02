rm(list = ls())

# load packages
library(tidyverse) 
library(ggpmisc) # for regression equations in plot
library(viridis) # for figure colors

# LOAD DATA --------------------------------------------------------------------

# data for articles with statistics
data <- read.table("03data/2023-06-15data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-06-15data_per_article_with_stats.txt", header = TRUE)

# reorder factor levels in journal to order panels in plots
journal_order <- c("PS", "JESP", "JEPG", "JPSP")
journal_labels <- c("PS (statcheck; general psychology)", "JESP (statcheck; social psychology)", 
                    "JEPG (control; general psychology)", "JPSP (control; social psychology)")

data$journal <- factor(data$journal, levels = journal_order, labels = journal_labels)
data_per_article$journal <- factor(data_per_article$journal, levels = journal_order, labels = journal_labels)

# FUNCTION FOR PLOT ------------------------------------------------------------

# create general function for the type of plot we're making to save time and
# avoid errors when changing parameters

make_plot <- function(data, y, color = NULL, shape = NULL, perc = TRUE, lm = TRUE,
                      label.y1 = .1, line = TRUE,
                      ymin = 0, ymax = 100, flag_intervention = TRUE,
                      title = NULL, ylab = NULL){
  
  # create data frame with intervention dates per journal to later be able
  # to add vertical lines at the time of the intervention
  date_intervention <- 
    data.frame(journal = factor(journal_order, labels = journal_labels),
               year_intervention = c(2016 + 7/12, # PS July 2016
                                     2017 + 8/12, # JESP August 2017
                                     2016 + 7/12, # PS July 2016
                                     2017 + 8/12)) # JESP August 2017
  
  data %>%
    ggplot(aes(x = year_published, y = y, color = color, shape = shape)) +
    {if (line) geom_line()} +
    geom_point() +
    {if(perc) scale_y_continuous(labels = function(x) paste0(x, "%"),
                                 limits = c(ymin, ymax))} +
    {if (lm) stat_poly_line()} +
    {if (lm) stat_poly_eq(use_label(c("eq", "R2")), label.y = label.y1)} +
    {if (flag_intervention) geom_vline(data = date_intervention, 
                                       aes(xintercept = year_intervention),
                                        linetype = 2)} +
    {if(!is.null(color)) scale_color_viridis(name = "",
                                             discrete = TRUE,
                                             begin = .6,
                                             end = 0,
                                             breaks = c("perc_art_error",
                                                        "perc_art_dec_error"),
                                             labels = c("Inconsistencies",
                                                        "Decision\nInconsistencies"))} +
    {if(!is.null(shape)) scale_shape_manual(name = "",
                                            labels = c("Inconsistencies",
                                                       "Decision\nInconsistencies"),
                                            values = c(19, 17))} +
    facet_wrap(~journal) +
    labs(title = title, 
         x = 'Publication Year',
         y = ylab) +
    theme_bw()
  
}

# PERC W NHST OVER TIME --------------------------------------------------------
# 
# perc_nhst_df <- data_per_article_all %>%
#   mutate(has_nhst = ifelse(nr_nhst > 0, 1, 0)) %>%
#   group_by(journal, year_published) %>%
#   summarize(perc_nhst = sum(has_nhst / n())* 100)
# 
# make_plot(data = perc_nhst_df, y = perc_nhst_df$perc_nhst,
#           title = "% Articles in which statcheck detected NHST results") 

# NR NHST OVER TIME ------------------------------------------------------------

mean_nr_nhst_df <- data_per_article %>%
  group_by(journal, year_published) %>%
  summarize(mean_nhst = mean(nr_nhst, na.rm = TRUE))

make_plot(data = mean_nr_nhst_df, y = mean_nr_nhst_df$mean_nhst, perc = FALSE,
          title = "Mean nr. of NHST results in an article for articles with NHST results",
          label.y1 = .95, ylab = "Nr. of NHST results")

ggsave("04analysis/03exploratory/fig3_nr_nhst_over_time.png", 
       width = 6.5, height = 4.5, units = "in")

# PERC (DEC) INCONS OVER TIME --------------------------------------------------
# 
# perc_errors_df <- data_per_article %>%
#   group_by(journal, year_published) %>%
#   summarize(mean_errors = mean(perc_errors),
#             mean_dec_errors = mean(perc_dec_errors)) %>%
#   pivot_longer(c(mean_errors, mean_dec_errors), names_to = "type_error",
#                values_to = "perc")
# 
# make_plot(data = perc_errors_df, y = perc_errors_df$perc,
#           color = perc_errors_df$type_error,
#           lm = FALSE,
#           title = "Mean % (decision) inconsistencies over time",
#           ymin = 0, ymax = 15)
# 
# ggsave("04analysis/03exploratory/fig4b_perc_errors_over_time.png", 
#        width = 6.5, height = 4.5, units = "in")

# PERC ARTICLES W INCONS OVER TIME ---------------------------------------------

articles_w_errors <- data_per_article %>%
  mutate(has_error = ifelse(nr_errors > 0, 1, 0),
         has_dec_error = ifelse(nr_dec_errors > 0, 1, 0)) %>%
  group_by(journal, year_published) %>%
  summarize(perc_art_error = sum(has_error)/n()*100,
            perc_art_dec_error = sum(has_dec_error)/n()*100) %>%
  pivot_longer(c(perc_art_error, perc_art_dec_error), names_to = "type_error",
               values_to = "perc") %>%
  mutate(type_error = factor(type_error, 
                             levels = c("perc_art_error", "perc_art_dec_error")))

make_plot(data = articles_w_errors, y = articles_w_errors$perc, 
          lm = FALSE,
          color = articles_w_errors$type_error,
          shape = articles_w_errors$type_error,
          title = "% Articles with at least one (decision) inconsistency",
          ymin = 0, ymax = 85)

ggsave("04analysis/03exploratory/fig4_perc_articles_errors_over_time.png", 
       width = 6.5, height = 4.5, units = "in")
