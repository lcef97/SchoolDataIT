#' Map schools included in the ultra-broadband plan to their LAU codes.
#'
#' @description
#' Helper function to provide the ultra-broadband dataset obtained with \code{\link{Get_BroadBand}}
#' with the statistical codes of the relevant municipalities, obtained with \code{\link{Get_School2mun}},
#' in case the ultra-broadband dataset has been downloaded with argument \code{include_municipality_code = FALSE}.
#'
#'
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' obtained with the function \code{\link{Get_BroadBand}}
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param input_School2mun Object of class \code{list} obtained with \code{\link{Get_School2mun}},
#' including the mapping from school codes to municipality (and province) codes.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_Registry If \code{input_School2mun} is required, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' corresponding to the national school registry (preferably of the last year, i.e. 2024/2025) obtained with \code{\link{Get_Registry}}. NULL by default.
#' @param input_AdmUnNames If \code{input_School2mun} is required, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' corresponding to the statistical codes of administrative units (preferably referring to the period corresponding to school year 2024/2025)
#'  obtained with \code{\link{Get_AdmUnNames}}. NULL by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame},
#' identical to the output of \code{\link{Get_BroadBand}} with an additional column for LAU codes
#'
#' @source  Broadband dashboard: <https://bandaultralarga.italia.it/scuole-voucher/dashboard-scuole/> .
#' ISTAT LAU codes: <https://situas.istat.it/web/#/territorio>
#'
#'
#'
#' @details see \code{\link{Get_BroadBand}}
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
