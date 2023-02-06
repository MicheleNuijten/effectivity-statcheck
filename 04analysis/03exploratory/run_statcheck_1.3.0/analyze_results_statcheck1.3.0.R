
# load data with statcheck 1.3.0 results
data1.3.0 <- read.table("04analysis/03exploratory/2023-01-05data_wrangled_no_missings_1.3.0.txt",
                        header = TRUE)
data_per_article_1.3.0 <- 
  read.table("04analysis/03exploratory/2023-01-05data_per_article_with_stats_1.3.0.txt",
             header = TRUE)
# descriptives
data_per_article_1.3.0 %>%
  filter(!is.na(period)) %>%
  group_by(journal, period) %>%
  summarize(articles_with_nhst = n(),
            median_nhst_per_article = median(nr_nhst),
            mean_perc_errors = round(mean(perc_errors), 1),
            mean_perc_dec_errors = round(mean(perc_dec_errors), 1))

# journal period articles_with_nhst median_nhst_per_article mean_perc_errors mean_perc_dec_errors
# <chr>    <int>              <int>                   <dbl>            <dbl>                <dbl>
# 1 JEPG         0                878                    19                7.4                  0.9
# 2 JEPG         1                652                    17.5              6.8                  0.8
# 3 JESP         0               1804                    15                8                    0.8
# 4 JESP         1                285                    26                5.1                  0.2
# 5 JPSP         0                566                    34                6.4                  0.6
# 6 JPSP         1                110                    38.5              3.9                  0.2
# 7 PS           0               2173                     8                9.3                  1.4
# 8 PS           1                547                    14                5.3                  2.8

# JESP did decrease in errors here and PS increased in decision errors (???)

# check out PS in more detail
data1.3.0 %>%
  filter(journal == "PS",
         period == 1,
         is.na(error),
         decision_error == TRUE) 
# some stats are read completely wrong which results in error = NA, but still
# a decision error

# see if we can remove these and then recalculate the error rates
data1.3.0_clean <- data1.3.0 %>%
  filter(!is.na(error))

data_per_article1.3.0_clean <- 
  data1.3.0_clean %>%
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

data_per_article1.3.0_clean %>%
  filter(!is.na(period)) %>%
  group_by(journal, period) %>%
  summarize(articles_with_nhst = n(),
            median_nhst_per_article = median(nr_nhst),
            mean_perc_errors = round(mean(perc_errors), 1),
            mean_perc_dec_errors = round(mean(perc_dec_errors), 1))

# decision errors in PS do go down now, but not that much
# check out PS in more detail again
data1.3.0_clean %>%
  filter(journal == "PS",
         period == 1,
         decision_error == TRUE) 

# still extraction errors causing more errors











