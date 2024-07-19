#' @keywords internal
#'
MeanOrMode <- function(c){
  if(is.numeric(c)) (mean(c, na.rm = TRUE))
  else if(!all(is.na(c))){
    names(which.max(table(c)))
  } else NA
}
