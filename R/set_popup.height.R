#' @keywords internal
#'
set_popup_height <- function(x) {
  str1 <- '
   <style>
      .leaflet-popup-content {
        max-height: '

  str2 <- 'px;
        overflow-y: auto;
      }
    </style>
    '
  return(paste(str1, x, str2, sep=""))
}
