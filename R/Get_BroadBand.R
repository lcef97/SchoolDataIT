#' Download the data regarding the broad band connection activation in Italian schools
#'
#' @description
#' Retrieves the data regarding the activation date of the broad band connection in schools. It also indicates whether the connection was activated or not at a certain date.
#'
#'
#' @param Date Object of class \code{Date}. The date at which it is required to determine if the broad band connection has been activated or not. By default it is the current date.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. The variables \code{BB_Activation_date} and \code{BB_Activation_staus} indicate the activation date and activation status of the broadband connection at the selected date.
#'
#' @source \href{https://bandaultralarga.italia.it/scuole-voucher/dashboard-scuole/}{Broadband dashboard}
#'
#'
#' @details Ultra - Broadband is defined as everlasting internet connection with a
#' maximum speed of 1 gigabit per second, with a minimum guaranteed speed of
#' 100 megabits/second both on the uploading and downloading operations, until
#' the peering point is reached, as declared on the data provider's \href{https://bandaultralarga.italia.it/scuole-voucher/progetto-scuole/}{website}.
#' In the example the broadband availability at the beginning of school  year 2022/23 (1st september 2022) is shown.
#'
#' @examples
#'
#' \donttest{
#' Broadband_220901 <- Get_BroadBand(Date = as.Date("2022-09-01"), autoAbort = TRUE)
#'
#' Broadband_220901
#'
#' Broadband_220901[, c(9,6,13,14)]
#' }
#'
#'
#'
#'
#' @export


Get_BroadBand <- function(Date = Sys.Date(), verbose=TRUE,  show_col_types = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  starttime <- Sys.time()

  home.url <- "https://bandaultralarga.italia.it/scuole-voucher/dashboard-scuole/"
  homepage <- NULL
  attempt <- 0
  while(is.null(homepage) && attempt <= 10){
    homepage <- tryCatch({
      xml2::read_html(home.url)
    }, error = function(e){
      message("Cannot read the html; ", 10 - attempt,
              " attempts left. If the problem persists, please check if the provider's website is working
              or contact the package mantainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) return(NULL)

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  link <- links[grep("downloadCSVScuole", links, ignore.case = TRUE)]
  base.url <- dirname(home.url)

  file.url <- xml2::url_absolute(link, base.url)

  status <- 0
  while(status != 200){
    response <- tryCatch({
      httr::GET(file.url)
    }, error = function(e) {
      message("Error occurred during scraping, attempt repeated ... \n")
      NULL
    })
    status <- response$status_code
    if(is.null(response)){
      status <- 0
    }
    if(status != 200){
      message("Operation exited with status: ", status, "; operation repeated")
    }
  }

  if(rawToChar(response$content) == ""){
    message("It seems that Broadband data are not available.
            We apologise for the inconvenience")
    return(NULL)
  }

  if (httr::http_type(response) %in% c("application/csv", "text/csv", "application/octet-stream")) {
    if(verbose) cat("Broadband data correctly downloaded \n")
    broadband <- iconv(rawToChar(httr::content(response)),
                       from = "ISO-8859-1",  to = "ASCII//TRANSLIT") %>%
      readr::read_delim(delim = ";", show_col_types = show_col_types)
  } else {
    message(paste("Wrong file type:", httr::http_type(response)) )
    return(NULL)
  }

  broadband <- broadband %>%  dplyr::mutate(School_code = substr(
    .data$codice_univoco_infratel,
    gregexpr("-", .data$codice_univoco_infratel)[[1]][3]+1,
    gregexpr("-", .data$codice_univoco_infratel)[[1]][3]+10)) %>%
    #dplyr::filter(!grepl("[^A-Z]", substr(.data$School_code,1,4)) &
    #!grepl("X", substr(.data$School_code,1,4), ignore.case = TRUE) ) %>%
    dplyr::mutate(Municipality_code = sprintf("%06d", as.numeric(.data$procom))) %>%
    dplyr::rename(Province_description = .data$provincia, Municipality_description = .data$comune,
                  School_name = .data$nomescuola, Tipologia_indirizzo = .data$gradomacro,
                  Region_code = .data$id_regione, Region_description = .data$regione,
                  Latitude = .data$latitudine, Longitude = .data$longitudine,
                  Province_code = .data$id_provincia, Province_description = .data$provincia,
                  BB_Activation_date = .data$data_attivazione,
                  Infratel_code = .data$codice_univoco_infratel) %>%
    School.order() %>%
    dplyr::mutate(
      BB_Activation_date =  as.Date(.data$BB_Activation_date, format = "%d/%m/%Y")) %>%
    dplyr::mutate(
      Region_description = stringr::str_to_title(.data$Region_description),
      Province_description = stringr::str_to_title(.data$Province_description),
      Municipality_description = stringr::str_to_title(.data$Municipality_description))

  broadband$Province_description <- broadband$Province_description %>%
    stringr::str_replace_all("Massa Carrara", "Massa-Carrara") %>%
    stringr::str_replace_all("Forli-Cesena", "Forli'-Cesena")

  broadband$Municipality_description <- broadband$Municipality_description %>%
    stringr::str_replace_all("Forli", "Forli'") %>%
    stringr::str_replace_all("Doberdo", "Doberdo'") %>%
    stringr::str_replace_all("Baselga Di Pine", "Baselga di Pine'") %>%
    stringr::str_replace_all("Citta Di", "Citta' Di") %>%
    stringr::str_replace_all("Citta Della", "Citta' Della")

  broadband <- broadband %>%
    dplyr::select(.data$Region_code, .data$Region_description,
                  .data$Province_code, .data$Province_description,
                  .data$Municipality_code, .data$Municipality_description, .data$Latitude,
                  .data$Longitude, .data$School_code, .data$Infratel_code,
                  .data$School_name, .data$Order, .data$BB_Activation_date)

  Date <- as.Date(Date)

  while(!"Date" %in% class(Date)){
    message("Please, provide a date in format `yyyy-mm-dd` (do not insert quotes in the prompt)")
    Date.new <- readline(prompt = "")
    Date <- as.Date(Date.new)
  }

  broadband <- broadband %>%  dplyr::mutate(BB_Activation_status = ifelse(
    !is.na(.data$BB_Activation_date), as.numeric(as.Date(.data$BB_Activation_date) <= Date), 0))

  provs.man <- prov.names() %>% dplyr::select(-.data$Region_code, -.data$Region_description) %>%
    dplyr::mutate(Province_description = stringr::str_to_title(.data$Province_description))

  broadband$Province_code <-
    dplyr::left_join(dplyr::select(broadband, .data$Province_description),
                     provs.man, by = "Province_description") %>%
    dplyr::select(.data$Province_code) %>% unlist()

  broadband <- broadband %>% dplyr::mutate(
    dplyr::across(.data$Province_code, ~dplyr::case_when(
      toupper(.data$Municipality_description) == "SAN TEODORO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "ME", 83, 90),
      toupper(.data$Municipality_description) == "PEGLIO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "CO", 13, 41),
      toupper(.data$Municipality_description) == "CASTRO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "LE", 75, 16),
      toupper(.data$Municipality_description) == "VALVERDE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "PV", 18, 87),
      toupper(.data$Municipality_description) == "SAMONE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "TO", 1, 22),
      toupper(.data$Municipality_description) == "CALLIANO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "AT", 5, 22),
      TRUE ~ .data$Province_code)),
    dplyr::across(.data$Province_description, ~dplyr::case_when(
      toupper(.data$Municipality_description) == "SAN TEODORO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "ME", "Messina", "Sassari"),
      toupper(.data$Municipality_description) == "PEGLIO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "CO", "Como", "Pesaro E Urbino"),
      toupper(.data$Municipality_description) == "CASTRO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "LE", "Lecce", "Bergamo"),
      toupper(.data$Municipality_description) == "VALVERDE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "PV", "Pavia", "Catania"),
      toupper(.data$Municipality_description) == "SAMONE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "TO", "Torino", "Trento"),
      toupper(.data$Municipality_description) == "CALLIANO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "AT", "Asti", "Trento"),
      TRUE ~ .data$Province_description)),
    dplyr::across(.data$Municipality_code, ~dplyr::case_when(
      toupper(.data$Municipality_description) == "PESCARA" ~ "068028",
      toupper(.data$Municipality_description) == "SAN TEODORO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "ME", "083090", "090092"),
      toupper(.data$Municipality_description) == "PEGLIO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "CO", "013078", "041041"),
      toupper(.data$Municipality_description) == "CASTRO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "LE", "075096", "016065"),
      toupper(.data$Municipality_description) == "VALVERDE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "PV", "018070", "087052"),
      toupper(.data$Municipality_description) == "SAMONE" ~ ifelse(
        substr(.data$School_code, 1, 2) == "TO", "001235", "022165"),
      toupper(.data$Municipality_description) == "CALLIANO" ~ ifelse(
        substr(.data$School_code, 1, 2) == "AT", "005014", "022035"),
      TRUE ~ .data$Municipality_code)))

  endtime <- Sys.time()

  if(verbose){
    cat(paste(round(difftime(endtime, starttime, units="secs") ,2),
              "seconds required download and process broadband data \n") )}

  return(broadband)
}

