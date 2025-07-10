#' Add ISTAT municipality codes to the Ultra-broadband activation DB
#' downloaded with \code{\link{Get_BroadBand}}
#'
#'
#' @keywords internal

Util_BroadBand2mun <- function(data, input_School2mun = NULL, input_Registry = NULL,
                          input_AdmUnNames = NULL, verbose = FALSE,
                          autoAbort = FALSE){
  if(is.null(input_School2mun)){
    input_School2mun <- Get_School2mun (Year = 2025, verbose = verbose,
                               input_AdmUnNames = input_AdmUnNames,
                               input_Registry = input_Registry,
                               autoAbort = autoAbort)
  }
  broadband <- data
  df.R <- input_School2mun$Registry_from_registry %>%
    dplyr::select(.data$School_code, .data$Municipality_code)
  res <- broadband %>% dplyr::left_join(df.R, by = "School_code") %>%
    dplyr::filter(!is.na(.data$Municipality_code)) %>%
    dplyr::relocate(.data$Municipality_code, .before = .data$Municipality_description)
  return(res)
}
