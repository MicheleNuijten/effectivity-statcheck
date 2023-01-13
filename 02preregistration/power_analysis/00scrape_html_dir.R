scrape_html_dir <- function(dir,
                         subdir = TRUE,
                         extension = TRUE,
                         ...) {
  if (missing(dir)) {
    dir <- tcltk::tk_choose.dir()
  }
  
  if (extension == TRUE) {
    pat = ".html|.htm"
  }
  
  if (extension == FALSE) {
    pat = ""
  }
  
  files <-
    list.files(dir,
               pattern = pat,
               full.names = TRUE,
               recursive = subdir)
  
  if (length(files) == 0) {
    stop("No HTML found")
  }
  
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
