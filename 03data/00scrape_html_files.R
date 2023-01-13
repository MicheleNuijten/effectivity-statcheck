scrape_html_files <- function(files, ...) {
  
  # convert html to plain text -------------------------------------------------
  txts <- character(length(files))
  message("Importing HTML files...")
  pb <- utils::txtProgressBar(max = length(files), style = 3)
  
  for (i in 1:length(files)) {
    txts[i] <-  html_to_txt(files[i])
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  names(txts) <- gsub(".html", "", basename(files))
  names(txts) <- gsub(".htm", "", names(txts))
  
  # extract dates and other meta-info ------------------------------------------
  
  date_info <- extract_dates(txts)
  
  # run statcheck --------------------------------------------------------------
  
  statcheck_result <- statcheck(txts, ...)
  
  # return complete dataframe --------------------------------------------------
  result <- dplyr::full_join(statcheck_result, date_info, by = "source")
  
  return(result)
}
