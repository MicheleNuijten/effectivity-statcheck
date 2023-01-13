rm(list = ls())

# load packages
devtools::install_github("MicheleNuijten/statcheck")
library(statcheck)

# source helper functions 
source("03data/00extract_pattern.R")
source("03data/00extract_dates.R")
source("03data/00html_to_txt.R")
source("03data/00scrape_html_dir.R")

# scrape articles per journal & year folder
article_dir <- "./01articles"

# journal folders
journal_dirs <- list.dirs(article_dir, recursive = FALSE)

# empty data frame for result
final_result <- data.frame(NULL)

for(jrnl in seq(journal_dirs)){
  
  # save variable journal
  journal_raw <- journal_dirs[jrnl]
  journal <- extract_pattern(journal_raw, "\\w{2,4}$")
  
  # year folders
  year_dirs <- list.dirs(journal_dirs[jrnl], recursive = FALSE)
  
  for(yr in seq(year_dirs)){
    
    # save variable year
    year_raw <- year_dirs[yr]
    year <- as.numeric(extract_pattern(year_raw, "\\d{4}$"))
    
    # scrape text 
    result <- scrape_html_dir(year_dirs[yr], subdir = FALSE, extension = TRUE)
    
    # add journal and year variable
    result_jrnl_yr <- cbind(result, journal = journal, year_published = year)
    
    # add results to main data frame
    if(nrow(result_jrnl_yr) > 0){
      final_result <- rbind(final_result, result_jrnl_yr)
    }
    
    # after appending the result_jrnl_yr dataframe to the main final_result dataframe,
    # the temporary dataframe result_jrnl_yr can be removed. 
    rm(result_jrnl_yr)
  }
  
}

write.table(final_result, paste0("03data/", Sys.Date(), "scraped_articles.txt"))

