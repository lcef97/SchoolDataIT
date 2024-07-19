#' Download the registry of Italian public schools from the school registry section
#'
#' @description This function returns two main pieces of information regarding Italian schools, namely:
#' \itemize{
#'   \item The denomination of the region, province and municipality to which the school belongs.
#'   \item The mechanographical code to the reference institute of each school.
#' }
#' It is possible to access schools in all the national territory, including the autonomous provinces of Aosta, Trento and Bozen.
#'
#'
#'
#' @param Year Numeric or character. Reference school year (last available is 2024).
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}. \code{2023} by default.
#' @param show_col_types Logical. If \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param filename Character. A string included in the name of the file to download, identifying the schools included.
#' By default it is \code{c("SCUANAGRAFESTAT", "SCUANAAUTSTAT")}, i.e. the file names used for public school registries,
#' respectively across all the national territory except for the autonomous provinces of Aosta, Trento or Bozen, and only in the three
#' If instead the registry of the private schools is needed, please insert \code{"SCUANAGRAFEPAR"} and/or \code{"SCUANAAUTPAR"}.
#'
#' For the registry of private schools, either in all the national territory except for the aforementioned provinces, and for these provinces, please use \code{"SCUANAGRAFEPAR"} and \code{"SCUANAAUTPAR"} respectively. Please notice that data regarding private schools are not available for most functions in this package.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @details Schools different from primary, middle or high schools are classified as \code{"NR"}.
#'
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole}{Homepage}
#' @examples
#'
#' \donttest{
#'   Get_Registry(2024, filename = "SCUANAGRAFESTAT", autoAbort = TRUE)
#' }
#'
#'
#'
#'
#' @export



Get_Registry <- function(Year = 2023, filename = c("SCUANAGRAFESTAT", "SCUANAAUTSTAT"),
                         show_col_types = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  home.url <-"https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole"
  homepage <- NULL
  attempt <- 0
  while(is.null(homepage) && attempt <= 10){
    homepage <- tryCatch({
      xml2::read_html(home.url)
    }, error = function(e){
      message("Cannot read the html; ", 10 - attempt,
              " attempts left. If the problem persists, please contact the mantainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) return(NULL)
  name_pattern2 <- "([0-9]+)\\.(csv)$"
  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()

  pattern <- year.patternA(Year)

  while(! all(filename %in% c("SCUANAGRAFESTAT", "SCUANAGRAFEPAR", "SCUANAAUTSTAT", "SCUANAAUTPAR"))){
    notfound <- which(!filename %in% c(
      "SCUANAGRAFESTAT", "SCUANAGRAFEPAR", "SCUANAAUTSTAT", "SCUANAAUTPAR"))[1L]
    message(paste0("No file corresponds to the filename `", filename[notfound],"`"))
    cat("Please, choose one among the following (do not use quotes or hyphens in the prompt): \n" )
    cat("    - SCUANAGRAFESTAT for public schools in all regions except the provinces of Aosta, Trento and Bozen \n")
    cat("    - SCUANAGRAFEPAR for private schools in the aforementioned territory \n")
    cat("    - SCUANAAUTSTAT for public schools in the provinces of Aosta, Trento and Bozen \n")
    cat("    - SCUANAAUTPAR for private schools in the aforementioned provinces \n")
    filename[notfound] <- readline(prompt = "    ")
  }

  out <- list()

  for(obj in filename){

    for (string in links[grep(".csv", links)] ) {
      num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
      if ( num_numeric_digits >= nchar(pattern) ){
        first_nchar <- stringr::str_extract(string, paste0("[0-9]{", nchar(pattern), "}"))
        if (!is.na(first_nchar) & first_nchar == pattern & length(grep(obj, string)) > 0 ) {
          file_to_download <- string
          break
        }
      }
    }

    if (!"file_to_download" %in% ls()){
      message("Schools registry not available for this year. We apologise for the inconvenience.")
      return(NULL)
    }

    status <- 0
    while(status != 200){
      base.url <- dirname(home.url)
      file.url <- file.path(base.url, file_to_download)
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

    if (httr::http_type(response) %in% c("application/csv", "text/csv", "application/octet-stream")) {
      dat <- readr::read_csv(rawToChar(response$content), show_col_types = show_col_types) %>%
        dplyr::mutate(CAPSCUOLA = as.character(.data$CAPSCUOLA))
      names(dat) <- names(dat) %>% stringr::str_replace_all("ANNOSCOLASTICO", "Year") %>%
        stringr::str_replace_all("AREAGEOGRAFICA", "Area") %>%
        stringr::str_replace_all("REGIONE", "Region_description") %>%
        stringr::str_replace_all("PROVINCIA", "Province_description") %>%
        stringr::str_replace_all("CODICEISTITUTORIFERIMENTO", "Reference_institute_code") %>%
        stringr::str_replace_all("DENOMINAZIONEISTITUTORIFERIMENTO", "Reference_institute_name") %>%
        stringr::str_replace_all("CODICESCUOLA", "School_code") %>%
        stringr::str_replace_all("DENOMINAZIONESCUOLA", "School_name") %>%
        stringr::str_replace_all("INDIRIZZOSCUOLA", "School_address") %>%
        stringr::str_replace_all("CAPSCUOLA", "Postal_code") %>%
        stringr::str_replace_all("CODICECOMUNESCUOLA", "Cadastral_code") %>%
        stringr::str_replace_all("DESCRIZIONECOMUNE", "Municipality_description") %>%
        stringr::str_replace_all("DESCRIZIONECARATTERISTICASCUOLA", "School_feature_description") %>%
        stringr::str_replace_all("DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA", "Order_description") %>%
        stringr::str_replace_all("INDICAZIONESEDEDIRETTIVO", "Headquarters_indication") %>%
        stringr::str_replace_all("INDICAZIONESEDEOMNICOMPRENSIVO", "Comprehensive_institute_venue_information") %>%
        stringr::str_replace_all("INDIRIZZOEMAILSCUOLA", "email_address") %>%
        stringr::str_replace_all("INDIRIZZOPECSCUOLA", "Certified_email_address") %>%
        stringr::str_replace_all("SITOWEBSCUOLA", "Website") %>%
        stringr::str_replace_all("SEDESCOLASTICA", "Venue")
      out[[obj]] <- dat
    } else{
      message("Wrong file type:", httr::http_type(response))
    }
  }
  if(length(out) == 0L) return(NULL)

  res <- do.call(dplyr::bind_rows, out) %>%
    dplyr::relocate(.data$Postal_code, .after = "Municipality_description") %>%
    dplyr::relocate(.data$Reference_institute_name, .after = "Postal_code")%>%
    dplyr::relocate(.data$School_name, .after = "Reference_institute_name") %>%
    dplyr::relocate(.data$School_address, .after = "Municipality_description") %>%
    dplyr::mutate(Area = stringr::str_to_title(.data$Area),
                  Region_description = stringr::str_to_title(.data$Region_description),
                  Province_description = stringr::str_to_title(.data$Province_description),
                  Municipality_description = stringr::str_to_title(.data$Municipality_description),
                  Reference_institute_name = stringr::str_to_title(.data$Reference_institute_name),
                  School_name = stringr::str_to_title(.data$School_name),
                  School_address = stringr::str_to_title(.data$School_address),
                  School_feature_description = stringr::str_to_title(.data$School_feature_description),
                  Order_description = stringr::str_to_title(.data$Order_description))

  return(res)
}

