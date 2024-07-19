#' @keywords internal
#'

filterCommonRows <- function(df1, df2, verbose = TRUE){
  nrow.old <- nrow(df1)
  df1 <- df1 %>% dplyr::filter(.data$School_code %in% df2$School_code)
  nrow.new <- nrow(df1)
  args <- as.list(match.call())[-1]
  args$df1 <- as.character(args$df1)
  args$df2 <- as.character(args$df2)
  if(verbose){cat("Deleted ", nrow.old - nrow.new, " rows of ",
                  args$df1, " not included in ", args$df2, " \n")}
  return(df1)
}
