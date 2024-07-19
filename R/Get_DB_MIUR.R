#' Download the database of Italian public schools buildings
#'
#' @description  This function downloads the School Buildings Open Database provided by the Italian Ministry of Education, University and Research.
#'
#'
#' It is one of the main sources of information regarding the infrastructure system of public schools in Italy.
#' For a given year, all available data are downloaded (except for the structural units section, which has a different level of detail) and gathered into a unique dataframe.
#'
#' @param Year Numeric or character value. Reference school year (last available is 2023).
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}. \code{2022} by default (other databases are not currently available for 2023).
#' @param input_Registry Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The school registry corresonding to the year in scope, obtained as output of the  function \code{\link{Get_Registry}}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default.
#' @param input_AdmUnNames Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The ISTAT file including all the codes and all the names of the administrative units for the year in scope, obtained as output of the function \code{\link{Get_AdmUnNames}}.
#' Only  necessary for school years 2015/16, 2017/18 and 2018/19.
#' If \code{NULL} and required, it will be downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#'
#'
#' @source  \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Edilizia+Scolastica}{Homepage}
#'
#' @details
#' This function downloads the raw data; missing observations are not edited; all variables are characters. To edit the output of this function and convert the relevant variables to numeric or Boolean, please \code{\link{Util_DB_MIUR_num}}.
#' Schools different from primary, middle or high schools are classified as \code{"NR"}. In the example, the data for school year 2022/23 are retrieved.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. All variables are characters.
#'
#' @examples
#'
#' \donttest{
#'   input_DB23_MIUR <- Get_DB_MIUR(2023, autoAbort = TRUE)
#'
#'   input_DB23_MIUR[-c(1,4,6,9)]
#'
#' }
#'
#'
#' @export


Get_DB_MIUR <- function(Year = 2023, verbose = TRUE, input_Registry = NULL,
                        input_AdmUnNames = NULL, show_col_types = FALSE, autoAbort = FALSE){

  start.zero <- Sys.time()

  if(!Check_connection(autoAbort)) return(NULL)

  # Link retrieving
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
  pattern <- year.patternB(Year)
  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- links[which(!is.na(links))]
  if (!any(str_detect.general(links, pattern))){
    message("No data available for this year. We apologise for the inconvenience")
    return(NULL)
  }

  files_to_download <- c()
  for (string in links[grep(".csv", links)] ) {
    num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
    nchar_min <- min(nchar(pattern))
    nchar_max <- max(nchar(pattern))
    if (num_numeric_digits >= nchar_min ){
      first_nchar_min <- stringr::str_extract(string, paste0("[0-9]{", nchar_min, "}"))
      first_nchar_max <- stringr::str_extract(string, paste0("[0-9]{", nchar_max, "}"))
      if (!is.na(first_nchar_min) & !is.na(first_nchar_max) & any(pattern %in% c(first_nchar_min, first_nchar_max)) &
          ! string %in% files_to_download) {
        files_to_download <- append(files_to_download, string)
      }
    }
  }

  # Scraping
  base.url <- dirname(home.url)
  input_MIUR <- list()
  starttime <- Sys.time()
  for (link in files_to_download) {
    file.url <- file.path(base.url, link)
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
      if(verbose){
        dat <- readr::read_csv(rawToChar(response$content), show_col_types = FALSE)
        cat("CSV file downloaded:", link, " ... ")
      } else {
        suppressMessages(dat <- readr::read_csv(rawToChar(response$content)))
      }
      input_MIUR[[link]] <- dat
      input_MIUR[[link]] <- input_MIUR[[link]] %>% dplyr::select(-.data$ANNOSCOLASTICO)
      input_MIUR[[link]] <- input_MIUR[[link]][!duplicated(input_MIUR[[link]]),]
      #input_MIUR[[link]] <- input_MIUR[[link]] %>% tidyr::unite(ID, .data$CODICESCUOLA, .data$CODICEEDIFICIO)

    } else {
      if(verbose){
        message(paste("Wrong file type:", httr::http_type(response)) )
        message("Failed to download and process:", link)
      }
    }
    endtime <- Sys.time()
    if(verbose){
      cat(paste(round(difftime(endtime, starttime, units="secs"),
                      2),"seconds required to join it \n ") )
    }
    starttime <- Sys.time()
  }
  if(length(input_MIUR) == 0L) return(NULL)

  # Joining tables
  mapping_MIUR <- input_MIUR[[grep("ANAGRAFE", names(input_MIUR))]]
  DB_MIUR.R <- mapping_MIUR %>% dplyr::select(
    .data$CODICESCUOLA, .data$CODICEEDIFICIO, .data$CODICECOMUNE, .data$DESCRIZIONECOMUNE,
    .data$SIGLAPROVINCIA, .data$CAP)


  # This is for the municipality of Bladen/Plodn/Sappada which changed it province in 2018
  if(!any(pattern %in% year.patternB(2016))){
    DB_MIUR.R <- DB_MIUR.R %>% dplyr::mutate(dplyr::across(.data$SIGLAPROVINCIA, ~ dplyr::case_when(
      toupper(.data$DESCRIZIONECOMUNE) == "SAPPADA" ~ "UD",
      TRUE ~ .data$SIGLAPROVINCIA
    )))
  }

  for ( i in c(1:length(input_MIUR))) {
    if(length(grep("ANAGRAFE", names(input_MIUR)[i]))==0){
      if (nrow(input_MIUR[[i]]) != nrow(DB_MIUR.R) & verbose == TRUE){
        warning(paste("Expected", nrow(DB_MIUR.R), "rows but in",
                      names(input_MIUR)[i], "there are:", nrow(input_MIUR[[i]])))
      }
      DB_MIUR.R <- dplyr::left_join(DB_MIUR.R, input_MIUR[[i]], by = c("CODICESCUOLA", "CODICEEDIFICIO"))
    }
  }

  names(DB_MIUR.R) <- names(DB_MIUR.R) %>% stringr::str_remove_all(".y") %>% stringr::str_remove_all(".x")
  DB_MIUR.R <- DB_MIUR.R[,!duplicated(colnames(DB_MIUR.R))]

  # This is for the province of Naples which happens to have "NA" as abbreviation
  DB_MIUR.R$SIGLAPROVINCIA <- stringr::str_replace_na(DB_MIUR.R$SIGLAPROVINCIA, "NA")

  #DB_MIUR.R <- DB_MIUR.R %>%tidyr::separate(col = .data$ID, into=c("CODICESCUOLA", "CODICEEDIFICIO"), sep="_")

  tabrename <- tabrename.manual()
  for (j in (1:ncol(DB_MIUR.R))){
    if (names(DB_MIUR.R)[j] %in% tabrename$Input){
      names(DB_MIUR.R)[j] <- tabrename[which(tabrename$Input == names(DB_MIUR.R)[j]),4]
    }
  }

  #This is for old data where the municipality is identified through the cadastral code
  if (any(pattern %in% c(year.patternB(2016), year.patternB(2018), year.patternB(2019)))) {

    YearMinus1 <- as.numeric(substr(year.patternA(Year),1,4))
    if(is.null(input_AdmUnNames)) {
      cat("Mapping cadastral codes to municipality (LAU) codes:")
      input_AdmUnNames <- Get_AdmUnNames(
        Year = ifelse(any(pattern %in% c(year.patternB(2016), year.patternB(2018))), Year, YearMinus1),
        date = ifelse(any(pattern %in% c(year.patternB(2016), year.patternB(2018))), "01_01", "06_30"),
        autoAbort = autoAbort)
    }
    CodMun.R <- input_AdmUnNames %>% dplyr::select(.data$Cadastral_code, .data$Municipality_code)

    DB_MIUR.R <- DB_MIUR.R %>% dplyr::rename(Cadastral_code = .data$Municipality_code) %>%
      dplyr::left_join(CodMun.R, by = "Cadastral_code") %>%
      dplyr::relocate(.data$Municipality_code, .after = "Building_code") %>%
      dplyr::select(-.data$Cadastral_code) %>%
      fixMun.manual(Year)
  }

  if (is.null(input_Registry)) input_Registry <- Get_Registry(Year = Year, autoAbort = autoAbort)

  left <- input_Registry[,c(1,6,5)] %>% dplyr::filter(.data$School_code %in% DB_MIUR.R$School_code)

  DB_MIUR <- dplyr::left_join(left, DB_MIUR.R, by = "School_code") %>%
    School.order() %>%
    dplyr::mutate(Municipality_description = stringr::str_to_title(.data$Municipality_description))

  enditme <- Sys.time()
  if(verbose){
    cat(paste("Total running time needed to import school buildings data:",
              round(difftime(endtime, start.zero, units="secs"), 2), "seconds \n"  ))
  }

  return(DB_MIUR)
}


