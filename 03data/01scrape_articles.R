rm(list = ls())

# load packages
devtools::install_github("MicheleNuijten/statcheck@v1.4.0-beta.7")
library(statcheck)

# source helper functions 
source("03data/00extract_pattern.R")
source("03data/00extract_dates.R")
source("03data/00html_to_txt.R")
source("03data/00list_html_files.R")
source("03data/00scrape_html_files.R")
source("01articles/00clean_filenames.R")

# load list of titles of JPSP articles 2003-2013 in the ASC subsection
asc_titles_df <- 
  as.data.frame(readxl::read_excel("01articles/overview_jpsp_asc_2003-2013.xlsx"))

# specify which directory to search
article_dir <- "./01articles"

# list journal folders
journal_dirs <- list.dirs(article_dir, recursive = FALSE)

# empty data frame for result
final_result <- data.frame(NULL)

# scrape articles per journal & year folder
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
    
    # print statement to keep track of where we are
    cat(paste0("journal = ", journal, "; year = ", year, "\n"))
    
    # list html files in relevant directory
    html_files <- list_html_files(year_dirs[yr], subdir = FALSE, 
                                  extension = TRUE)
    
    #########################
    
    # for JPSP 2003-2013; only select articles from the subsection ASC
    # from these years we downloaded everything, so we have to match the titles
    # to a list of all ASC article titles to only scrape the relevant articles
    
    if(journal == "JPSP" & (year >=2003 & year <=2013)){
      # first remove general path; only keep file name
      filenames_base <- basename(html_files)
      
      # clean file names in the directory to match with title list
      filenames_clean <- clean_filenames(filenames_base, nchar = 25) 
      
      # select asc titles of relevant year
      asc_titles <- asc_titles_df[asc_titles_df$year == year, "title"]
      
      # clean asc titles to match with list of file names
      asc_titles_clean <- clean_filenames(asc_titles, nchar = 25)
      
      # select only jpsp articles to scrape if they occur in asc title list
      html_files <- html_files[which(filenames_clean %in% asc_titles_clean)]
    } 
  
    #########################
    
    result <- scrape_html_files(html_files, OneTailedTxt = TRUE)
    
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

