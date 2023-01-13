
# function to extract snippets of text from a string ---------------------------

extract_pattern <- function(txt, pattern, ignore.case = TRUE) {
  
  # extract the locations of the matches in the text:
  # gregexpr returns the position of every match in a string
  # if there are multiple matches in the text, gregexpr will flag them all
  # the output is in list format, but the relevant information is all in [[1]]
  string_loc <- gregexpr(pattern = pattern, 
                         text = txt, 
                         ignore.case = ignore.case,
                         perl = TRUE)[[1]] # perl is necessary for lookbehinds
  
  # if no match is found, return NULL
  if(string_loc[1] == -1){
    return(NULL)
  }
  
  # if a match is found:
  # extract the raw text of the regex match:
  # retrieve a 'substring' from the text that starts at string_loc: string_loc
  # is a vector of integers that indicate the location of the first characters of
  # the match. The end point of the substring is determined using the 
  # attribute 'match.length' of string_loc This is a vector of integers indicating
  # how long each matched string is. By adding the length of the string to the
  # position of the first character, and subtracting 1, you obtain the location
  # of the last character of the string.
  string <- substring(text = txt, 
                      first = string_loc, 
                      last = string_loc + attr(string_loc, "match.length") - 1)
  
  return(string)
}
