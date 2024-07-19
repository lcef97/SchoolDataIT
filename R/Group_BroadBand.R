#' @keywords internal
#'

Group_BroadBand <- function(data = NULL, Date = Sys.Date(), verbose=TRUE,  autoAbort = FALSE){
  if(is.null(data)){
    data <- Get_BroadBand(Date = Date, verbose = verbose, autoAbort = autoAbort)
  }
  if(is.null(data)) return(NULL)
  data <- data %>% dplyr::mutate(
    Province_code = as.numeric(substr(.data$Municipality_code, 1, 3)))

  broadband.mun <- data %>% dplyr::filter(.data$Order != "NR") %>%
    dplyr::group_by(.data$Region_code, .data$Region_description, .data$Province_code,
                    .data$Municipality_code, .data$Order) %>%
    dplyr::summarise(nschools = dplyr::n(), Province_description = .data$Province_description[1L],
                     Municipality_description = .data$Municipality_description[1L],
                     BB_Activation_status = mean(.data$BB_Activation_status, na.rm = TRUE)) %>%
    dplyr::ungroup()

  broadband.prov <- data %>% dplyr::filter(.data$Order != "NR") %>%
    dplyr::group_by(.data$Region_code, .data$Region_description, .data$Province_code,  .data$Order) %>%
    dplyr::summarise(nschools = dplyr::n(), Province_description = .data$Province_description[1L],
                     BB_Activation_status = mean(.data$BB_Activation_status, na.rm = TRUE)) %>%
    dplyr::ungroup()

  res <- list(Municipality_data = broadband.mun, Province_data = broadband.prov)
  return(res)

}


