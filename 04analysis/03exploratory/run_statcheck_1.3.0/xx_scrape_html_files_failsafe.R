scrape_html_files <- function(files, ...) {
  
  # convert html to plain text -------------------------------------------------
  txts <- character(length(files))
  
  date_info <- data.frame(source = rep(NA, length(files)),
                          date_received = as.Date(rep(NA, length(files))))
  
  statcheck_result <- data.frame(matrix(NA, ncol = 15))
  colnames(statcheck_result) <- c("Source", "Statistic",  "df1", "df2", "Test.Comparison",
                                  "Value", "Reported.Comparison", "Reported.P.Value", "Computed", "Raw",
                                  "Error", "DecisionError", "OneTail", "OneTailedInTxt", "APAfactor")
  
  for (i in 1:length(files)) {
    txts[i] <-  html_to_txt(files[i])
    
    names(txts)[i] <- gsub(".html", "", basename(files[i]))
    names(txts)[i] <- gsub(".htm", "", names(txts)[i])
    
    # extract dates and other meta-info ------------------------------------------
    foo <- try(extract_dates(txts[i]))
    
    if(inherits(foo, "try-error")) {
      date_info[i, ] <- rep(NA, 2)
    } else {
      date_info[i, ] <- foo
    }
    
    # run statcheck --------------------------------------------------------------
    
    bar <- try(statcheck(txts[i], ...))
    
    if(!inherits(bar, "try-error") & !is.null(nrow(bar))){
      statcheck_result <- rbind(statcheck_result, bar)
    } 
  
  }
  # return complete dataframe --------------------------------------------------
  colnames(statcheck_result)[1] <- "source"
  
  result <- dplyr::full_join(statcheck_result, date_info, by = "source")
  
  return(result)
}
