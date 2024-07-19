#' @keywords internal
#'
cutout.default <- function(data, startcol=10, pattern.out = c(),
                           init.cutout = c()){
  cutout <- init.cutout
  for (j in (startcol:ncol(data))){
    if (nrow( unique(data[which(! data[,j][[1]] %in% pattern.out),j]) ) > 2){
      cutout <- c(cutout, names(data)[j])
    }
  }
  return(cutout)
}
#

