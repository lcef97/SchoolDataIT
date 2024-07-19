#' @keywords internal
#'
Nuisance <- function(data, startcol = 10,
                     missing.pattern = c("NON DEFINITO", "-","INFORMAZIONE_ASSENTE", "NON_COMUNICATO"),
                     partial.pattern = "IN PARTE"){

  res <- as.data.frame( cbind(
    apply( data[,-c(1:(startcol - 1))],MARGIN = 2,
           FUN = function(x){
             x <- toupper(x)
             return (sum (x %in% toupper(missing.pattern)))
           } ),
    apply( data[,-c(1:(startcol - 1))],MARGIN = 2,
           FUN = function(x){
             x <- toupper(x)
             return (sum (x %in% toupper(partial.pattern)))
           } ) ) )
  names(res) = c("Non_definiti", "In_parte")
  return(res)
}


