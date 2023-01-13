list_html_files <- function(dir,
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
  
  return(files)
}