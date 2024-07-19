#' Download the number of teachers in Italian schools by province
#'
#' @description  This functions downloads the number of teachers by province from the open website of the Italian Ministry of Education, University and Research.
#'
#' @param Year Numeric or character value. Reference school year for the school registry data (last available is 2023).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022.} \code{2023} by default
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the 'verbose' argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param filename Character. Which data to retrieve among the province counts of teachers/school personnel.
#' By default it is \code{c("DOCTIT", "DOCSUP")}, which are the file names used so far for the number of tenured and temporary teachers respectively.
#' Other file names are the following:
#'
#' \code{"ATATIT"} for the number of tenured non-teaching personnel
#'
#'
#' \code{"ATASUP"} for the number of temporary non-teaching personnel
#'
#' @details Please notice that by default, the function returns the count of the number of tenured and temporary teachers.
#' If either the count of non-teaching personnel or the count of a single category of teaching personnel is needed, please adapt
#' the \code{filename} argument accordingly.
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Personale+Scuola}{Homepage}
#'
#' @examples
#'
#'
#' nteachers23 <- Get_nteachers_prov(2023, filename = "DOCTIT", autoAbort = TRUE)
#' nteachers23[, c(3,4,5)]
#'
#'
#' @export
#'
#'
Get_nteachers_prov <- function(Year = 2023, verbose = TRUE, show_col_types = FALSE,
                               filename = c("DOCTIT", "DOCSUP"), autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)
  options(dplyr.summarise.inform = FALSE)

  start.zero <- Sys.time()

  home.url <- "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Personale%20Scuola"
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
  pattern <- ifelse(year.patternA(Year)=="201516", "1516", year.patternA(Year) )

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- links[which(!is.na(links))]
  if (!any(stringr::str_detect(links, pattern))){
    warning("No data available for this year. We apologise for the inconvenience")
    return(NULL)
  }

  files_to_download <- c()
  for (string in links[grep(".csv", links)] ){
    num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
    if (num_numeric_digits >= nchar(pattern)){
      chrpart <- gsub("csv", "", gsub("[^[:alpha:]]", "", string))
      numpart <- gsub("[[:alpha:]]", "", string)
      if (!is.na(numpart) & grepl(pattern, numpart) & !string %in% files_to_download &
          any(stringr::str_detect(chrpart, filename))){
        files_to_download <- append(files_to_download, string)
        if(verbose) cat("Found ", string, " as element ",length(files_to_download), "\n")
      }
    }
  }

  base.url <- dirname(home.url)

  input <- list()

  starttime <- Sys.time()
  for (link in files_to_download) {

    status <- 0
    while(status != 200){
      file.url <- file.path(base.url, link)
      response <- tryCatch({
        httr::GET(file.url)
      }, error = function(e) {
        message("Error occurred during scraping, attempt repeated ... \n")
        NULL
      })
      status <- response$status_code
      if(status != 200){
        message("Operation exited with status: ", status, "; operation repeated \n")
      }
      if(is.null(response)){
        status <- 0
      }
      if(status != 200){
        message("Operation exited with status: ", status, "; operation repeated")
      }
    }

    if (httr::http_type(response) %in% c("application/csv", "text/csv", "application/octet-stream")) {
      dat <- readr::read_csv(rawToChar(response$content), show_col_types = show_col_types)
      if(verbose) cat("CSV file downloaded:", link, " ... ")
      element.name <- substr(link,1, regexpr("[0-9]", link)-1)
      input[[element.name]] <- dat
      input[[element.name]] <- input[[element.name]][!duplicated(input[[element.name]]),]

    } else {
      message(paste("Wrong file type:", httr::http_type(response)) )
      cat("Failed to download and process:", link, "\n")
    }
    endtime <- Sys.time()
    if(verbose){
      cat(round(difftime(endtime, starttime, units="secs") ,2), " seconds required \n")
    }
    starttime <- Sys.time()
  }
  if(length(input) == 0L) return(NULL)


  if("DOCTIT" %in% names(input)){
    input$DOCTIT <- input$DOCTIT %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_teachers = sum(.data$DOCENTITITOLARIFEMMINE) + sum(.data$DOCENTITITOLARIMASCHI)) %>% dplyr::ungroup()
  }

  if("DOCSUP" %in% names(input)){
    input$DOCSUP <- input$DOCSUP %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_teachers = sum(.data$DOCENTISUPPLENTIFEMMINE) + sum(.data$DOCENTISUPPLENTIMASCHI)) %>% dplyr::ungroup()
  }

  if("ATATIT" %in% names(input)){
    input$ATATIT <- input$ATATIT %>%
      dplyr::group_by(.data$PROVINCIA) %>% dplyr::summarise(
        Tot_ATA = sum(.data$ATATITOLARIFEMMINE) + sum(.data$ATATITOLARIMASCHI)) %>% dplyr::ungroup()
  }

  if("ATASUP" %in% names(input)){
    input$ATASUP <- input$ATASUP %>%
      dplyr::group_by(.data$PROVINCIA) %>% dplyr::summarise(
        Tot_ATA = sum(.data$ATASUPPLENTIFEMMINE) + sum(.data$ATASUPPLENTIMASCHI)) %>% dplyr::ungroup()
  }

  if(length(input) == 0L) return(NULL)

  nteachers <- input[c("DOCTIT", "DOCSUP")]
  nteachers <- Filter(Negate(is.null), nteachers)
  nATA <- input[c("ATATIT", "ATASUP")]
  nATA <- Filter(Negate(is.null), nATA)

  nteachers.Tot <- do.call(rbind, nteachers)

  nATA.Tot <- do.call(rbind, nATA)

  if(!is.null(nteachers.Tot)){
    nteachers.Tot <- nteachers.Tot %>% dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>%
      dplyr::summarise(Tot_teachers = sum(.data$Tot_teachers)) %>% dplyr::ungroup()
  }
  if(!is.null(nATA.Tot)){
    nATA.Tot <- nATA.Tot %>% dplyr::group_by(.data$PROVINCIA) %>%
      dplyr::summarise(Tot_ATA = sum(.data$Tot_ATA)) %>% dplyr::ungroup()
  }

  if(!is.null(nteachers.Tot)){
    npers.Tot <- nteachers.Tot
    if(!is.null(nATA.Tot)){
      npers.Tot <- dplyr::left_join(npers.Tot, nATA.Tot, by = "PROVINCIA")
    }
  } else{
    npers.Tot <- nATA.Tot %>% dplyr::mutate(Order = NA) %>%
      dplyr::relocate(.data$Order, .after = .data$PROVINCIA)
  }

  npers.Tot <- npers.Tot %>% rename_by_idx(c(1,2), c("Province_description", "Order")) %>%
    dplyr::filter(is.na(.data$Order) | .data$Order != "SCUOLA INFANZIA")

  npers.Tot$Order <- npers.Tot$Order %>%
    stringr::str_replace_all("SCUOLA PRIMARIA", "Primary") %>%
    stringr::str_replace_all("SCUOLA SECONDARIA I GRADO", "Middle") %>%
    stringr::str_replace_all("SCUOLA SECONDARIA II GRADO", "High")


  names(npers.Tot) <- stringr::str_to_title(names(npers.Tot))

  npers.Tot$Province_description <- stringr::str_to_title(npers.Tot$Province_description) %>%
    stringr::str_replace_all("Monza E Brianza", "Monza E Della Brianza") %>%
    stringr::str_replace_all("Barletta-Adria-Trani", "Barletta-Andria-Trani") %>%
    stringr::str_replace_all("Medio-Campitano", "Medio Campidano") %>%
    stringr::str_replace_all("Pesaro-Urbino", "Pesaro E Urbino")
  npers.Tot$Province_description[which(npers.Tot$Province_description=="Massa")] <- "Massa-Carrara"

  provnames <- prov.names() %>% dplyr::select(-.data$Region_code, -.data$Region_description) %>%
    dplyr::mutate(dplyr::across(.data$Province_description, ~ stringr::str_to_title(.)))


  npers.Tot <- npers.Tot %>% dplyr::left_join(provnames, by = "Province_description") %>%
      dplyr::relocate(.data$Province_code, .before = 1) %>%
      dplyr::relocate(.data$Province_initials, .after = "Province_code")


  return(npers.Tot)

}

