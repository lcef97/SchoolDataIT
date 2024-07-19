#' @keywords internal

str_detect.general <- function(string, pattern, negate = FALSE){
    if(length(pattern)==1){
      return(stringr::str_detect(string, pattern, negate = negate))
      } else if (length(pattern)>1){
        res <- c()
        for(i in (1:length(pattern))){
          newvector <- stringr::str_detect(string, pattern[i], negate = negate)
          res <- c(res,newvector)
          }
    return(res)
  }
}
