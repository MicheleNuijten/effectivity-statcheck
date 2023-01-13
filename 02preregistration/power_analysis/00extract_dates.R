
extract_dates <- function(txts){
  
  # regex to extract date received/submitted
  # takes into account format in PS, JEPG, JPSP, and JESP
  rgx_received <- "(received|submitted):?\\s*\\d{0,2}\\s*\\w+\\s*\\d{0,2},?\\s*\\d{4}"
  rgx_date <- "\\d{0,2}\\s*\\w+\\s*\\d{0,2},?\\s*\\d{4}"
  
  if (is.null(names(txts))){
    names(txts) <-  seq_along(txts)
  }
  
  # create empty dataframe to fill with extracted dates
  date_df <- data.frame(source = names(txts),
                        date_received = lubridate::as_date(NA))
  
  for(i in seq(txts)){
    
    # extract (first) full date information
    raw_date <- extract_pattern(txts[i], rgx_received, ignore.case = TRUE)[1]
    
    # only keep actual date
    if(!is.null(raw_date)){
      txt_date <- extract_pattern(raw_date, rgx_date, ignore.case = TRUE)
      
      # convert to date object taking into account different date formats
      mdy <- suppressWarnings(lubridate::mdy(txt_date))
      
      if(!is.na(mdy)){
        # format is month-day-year
        date_df$date_received[i] <- mdy
      } else {
        # format is day-month-year
        date_df$date_received[i] <- lubridate::dmy(txt_date)
      }
    } else {
      date_df$date_received[i] <- NA
    }
    
  }
  
  return(date_df)
  
}
