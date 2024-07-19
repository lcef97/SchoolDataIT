#' Associate a Municipality (LAU) code to each school
#'
#' @description  This function associates the relevant municipality codes to all the schools listed in the two main registries provided by the Italian Ministry of Education, University and Research, namely:
#'  \itemize{
#'    \item The registry of school buildings, here referred to as \code{Registry_from_buildings} (\code{\link{Get_DB_MIUR}})
#'    \item The official schools registry, here referred to as \code{Registry_from_registry} (see \code{\link{Get_Registry}})
#'  }
#'
#'
#'
#' @param Year Numeric or character value (last available is 2023).
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023.} \code{2023} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param input_AdmUnNames Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_AdmUnNames}}
#' The ISTAT file including all the administrative units codes for the year in scope.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_Registry Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_Registry}}
#' The school registry corresonding to the year in scope.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Edilizia+Scolastica&datasetId=DS0101EDIANAGRAFESTA2021}{Buildings registry (2021 onwards)};
#'  \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Edilizia+Scolastica&datasetId=DS0200EDIANAGRAFESTA}{Buindings registry(until 2019)};
#'   \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole}{Schools registry}
#'
#'
#' @return An object of class \code{list}, including 4 elements:
#' \itemize{
#'   \item \code{$Registry_from_buildings}: Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the schools listed in the buildings registry
#'   \item \code{$Registry_from_registry}: Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the schools listed in the schools registry
#'   \item \code{$Any}: Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}: schools listed anywhere
#'   \item \code{$Both}: Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}: schools listed in both the sections
#'
#' }
#'
#'
#'
#' @examples
#'
#' \donttest{
#' Get_School2mun(Year = 2023, autoAbort = TRUE)
#' }
#'
#'
#' @export


Get_School2mun <- function(Year = 2023, show_col_types = FALSE, verbose = TRUE,
                           input_AdmUnNames = NULL, input_Registry = NULL, autoAbort = FALSE) {

  start.zero <- Sys.time()

  pattern <- year.patternB(Year)
  YearMinus1 <- as.numeric(substr(year.patternA(Year),1,4))
  if(is.null(input_AdmUnNames)){
    if(verbose) cat("Mapping cadastral codes to municipality (LAU) codes: \n")
    input_AdmUnNames <- Get_AdmUnNames(
      Year = ifelse(any(pattern %in% c(year.patternB(2016), year.patternB(2018))), Year, YearMinus1),
      date = ifelse(any(pattern %in% c(year.patternB(2016), year.patternB(2018))), "01_01", "06_30"),
      autoAbort = autoAbort)
  }
  # Case: failure
  if(is.null(input_AdmUnNames)) return(NULL)

  temp.R1 <- input_AdmUnNames %>% dplyr::select(.data$Cadastral_code, .data$Municipality_code)
  temp.R2 <- input_AdmUnNames %>% dplyr::select(.data$Cadastral_code, .data$Province_initials, .data$Municipality_code)

  if(verbose){cat("Retrieving registry from the buildings section ... \n")}

  starttime <- Sys.time()

  if(!Check_connection(autoAbort)) return(NULL)

  pattern <- year.patternB(Year)
  home.url <-"https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Edilizia%20Scolastica"
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
  name_pattern <- "([0-9]+)\\.(csv)$"
  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>%
    unique()
  links <- grep("EDIANAGRAFESTA", links, value = TRUE)
  links <- grep(".csv", links, value = TRUE)

  for (string in links) {
    num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
    nchar_min <- min(nchar(pattern))
    nchar_max <- max(nchar(pattern))
    if (num_numeric_digits >= nchar_min ){
      first_nchar_min <- stringr::str_extract(string, paste0("[0-9]{", nchar_min, "}"))
      first_nchar_max <- stringr::str_extract(string, paste0("[0-9]{", nchar_max, "}"))
      if (!is.na(first_nchar_min) & !is.na(first_nchar_max) &
          any(pattern %in% c(first_nchar_min, first_nchar_max))) {
        file_to_download <-  string
      }
    }
  }

  base.url <- dirname(home.url)
  file.url <- file.path(base.url, file_to_download)

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

  if (httr::http_type(response) %in% c("application/csv", "text/csv", "application/octet-stream")) {
    input_Registry1 <- readr::read_csv(rawToChar(response$content), show_col_types = FALSE)
  } else {
    message(paste("Wrong file type:", httr::http_type(response)) )
    message("Failed to download and process:", file_to_download, "\n")
    return(NULL)
  }

  # This is for the province of Naples whose abbreviation is `NA`
  input_Registry1$SIGLAPROVINCIA <- stringr::str_replace_na(input_Registry1$SIGLAPROVINCIA, "NA")

  # This is for the municipality of Bladen/Plodn/Sappada which changed it province in 2018
  if(!any(pattern %in% year.patternB(2016))){
    input_Registry1 <-input_Registry1  %>%
      dplyr::mutate(dplyr::across(.data$SIGLAPROVINCIA, ~ dplyr::case_when(
        toupper(.data$DESCRIZIONECOMUNE) == "SAPPADA" ~ "UD",
        TRUE ~ .data$SIGLAPROVINCIA
    )))
  }

  tabrename <- tabrename.manual()
  for (j in (1:ncol(input_Registry1))){
    if (names(input_Registry1)[j] %in% tabrename$Input){
      names(input_Registry1)[j] <- tabrename[which(tabrename$Input == names(input_Registry1)[j]),4]
    }
  }

  endtime <- Sys.time()
  if(verbose) {
    cat(round(difftime(endtime, starttime, units="secs") ,2), "seconds needed for the download \n" )
  }

  if(any(pattern %in% c("201516", "2016", "201718", "201819"))){
    Registry1 <- input_Registry1 %>%
      dplyr::rename(Cadastral_code = .data$Municipality_code) %>%
      dplyr::select(.data$School_code, .data$Province_initials,
                    .data$Cadastral_code, .data$Municipality_description) %>%
      dplyr::left_join(temp.R1, by = "Cadastral_code") %>%
      dplyr::select(-.data$Cadastral_code) %>%
      fixMun.manual(Year)
  } else {
    Registry1 <- input_Registry1 %>%
      dplyr::select(.data$School_code, .data$Province_initials, .data$Municipality_code, .data$Municipality_description)
  }
  Registry1 <- Registry1 %>% dplyr::mutate(Province_code = as.numeric(substr(.data$Municipality_code, 1, 3))) %>%
    dplyr::select(.data$School_code, .data$Province_code, .data$Province_initials,
                  .data$Municipality_code, .data$Municipality_description) %>%
    dplyr::mutate(Municipality_description = stringr::str_to_title(.data$Municipality_description)) %>% unique()

  if(is.null(input_Registry)){
    if(verbose) cat("Retrieving registry from registry section ... \n ")
    starttime <- Sys.time()
    input_Registry <- Get_Registry(Year = Year, autoAbort = autoAbort)
    endtime <- Sys.time()
    if(verbose){
      cat(round(difftime(endtime, starttime, units="secs") ,2), "seconds needed for the download \n"  )
    }
  }
  if(is.null(input_Registry)) return(NULL)

  Registry2 <- input_Registry %>% dplyr::select(
    .data$School_code,.data$Cadastral_code, .data$Municipality_description)%>%
    dplyr::left_join(temp.R2, by = "Cadastral_code") %>%
    dplyr::select(-.data$Cadastral_code) %>%
    dplyr::mutate(Province_code = as.numeric(substr(.data$Municipality_code, 1, 3))) %>%
    dplyr::select(.data$School_code, .data$Province_code, .data$Province_initials,
                  .data$Municipality_code, .data$Municipality_description) %>%
    dplyr::mutate(Municipality_description = stringr::str_to_title(.data$Municipality_description)) %>%
    unique() %>% fixMun.manual(Year)

  res <- list()
  res[["Registry_from_buildings"]] <- Registry1
  res[["Registry_from_registry"]] <- Registry2
  res[["Any"]] <- rbind(Registry2, Registry1) %>% unique()
  res[["Both"]] <- Registry2 %>% dplyr::filter(.data$School_code %in% Registry1$School_code)

  return(res)
}


