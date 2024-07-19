#' Download students' number data
#'
#' @description   This functions downloads the data regarding the number of students, from the open website of the Italian Ministry of Education, University and Research
#'
#'
#'
#' @param Year Numeric or character. Reference school year  (last available is 2023).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022}. \code{2023} by default
#' @param filename Character. A string included in the name of the file to download.
#' By default it is \code{c("ALUCORSOETASTA", "ALUCORSOINDCLASTA")}, which are the file names used so far for the number of students by age and the number of studentsin public schools  by age and class.
#'
#'
#'
#'
#' Other file names are the following. The output is not currently supported by the remainder of the functions involving the number of students.
#'
#'
#' \code{"ALUITASTRACITSTA"} for the number of Italian and foreign students in public schools
#'
#'
#' \code{"ALUSECGRADOINDSTA"} for the number of students of public schools by high school address
#'
#'
#' \code{"ALUTEMPOSCUOLASTA"} for the number of students of public schools by school running time
#'
#' \code{"ALUCORSOETAPAR"}, \code{"ALUCORSOINDCLAPAR"}, \code{"ALUITASTRACITPAR"}, \code{"ALUSECGRADOINDPAR"}, \code{"ALUTEMPOSCUOLAPAR"} for the data of the previous file but referring to private schools.
#'
#'
#'
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Studenti}{Homepage}
#'
#'
#' @examples
#'
#' \donttest{
#'   Get_nstud(2023, filename = "ALUCORSOINDCLASTA", autoAbort = TRUE)
#' }
#'
#'
#'
#' @return By default, a list of two \code{tbl_df}, \code{tbl} and \code{data.frame} objects:
#'
#' \itemize{
#'   \item \code{$ALUCORSOETASTA}: The number of students by school, school grade and age. It provides a higher number of school than the other element
#'   \item \code{$ALUCORSOINDCLASTA}: The number of students and classes by school and school grade. This is a long-format dataframe.
#'
#' }
#'
#' @export

Get_nstud <- function(Year = 2023, filename = c("ALUCORSOETASTA", "ALUCORSOINDCLASTA"),
                      verbose = TRUE, show_col_types = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  start.zero <- Sys.time()

  home.url <- "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Studenti"
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
  pattern <- year.patternA(Year)

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- links[which(!is.na(links))]
  if (!any(stringr::str_detect(links, pattern))){
    message("No data available for this year. We apologise for the inconvenience")
    return(NULL)
  }


  files_to_download <- c()
  for (string in links[grep(".csv", links)] ) {
    num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
    if (num_numeric_digits >= nchar(pattern) ){
      first_nchar <- stringr::str_extract(string, paste0("[0-9]{", nchar(pattern), "}"))
      if (!is.na(first_nchar) & pattern == first_nchar & ! string %in% files_to_download &
          #substr(string, nchar(string)-num_numeric_digits-6, nchar(string)-num_numeric_digits-4) == "STA" &
          substr(string,1,3) == "ALU" &
          substr(string, 1, nchar(string)-num_numeric_digits-4 ) %in% filename){
        files_to_download <- append(files_to_download, string)
      }
    }
  }

  input.nstud0 <- list()

  starttime <- Sys.time()
  for (link in files_to_download) {

    status <- 0
    while(status != 200){
      base.url <- dirname(home.url)
      file.url <- file.path(base.url, link)
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
      dat <- readr::read_csv(rawToChar(response$content), show_col_types = show_col_types)
      if(verbose) cat("CSV file downloaded: ", link, "...")
      element.name <- substr(link,1, regexpr("[0-9]", link)-1)
      input.nstud0[[element.name]] <- dat
      input.nstud0[[element.name]] <- input.nstud0[[element.name]][!duplicated(input.nstud0[[element.name]]),]
      endtime <- Sys.time()
      if(verbose) {cat(round(difftime(endtime, starttime, units="secs") ,2),
                       "seconds required \n ")}
      starttime <- Sys.time()


    } else {
      message(paste("Wrong file type:", httr::http_type(response)) )
      message("Failed to download and process:", link, "\n")
    }
  }

  if(length(input.nstud0) == 0L) return(NULL)

  input.nstud0 <- lapply(input.nstud0, function(x){
    if ("ANNOCORSOCLASSE" %in% names(x)){
      x <- x %>% dplyr::rename(ANNOCORSO = .data$ANNOCORSOCLASSE)
    } else return(x)
  })

  input.nstud0  <- lapply(input.nstud0, function(x){
    if("ANNOCORSO" %in% names(x)){
      x <- x %>% dplyr::filter(.data$ANNOCORSO <= 5) %>%
        dplyr::mutate(ANNOCORSO = .data$ANNOCORSO + dplyr::case_when(
          .data$ORDINESCUOLA == "SCUOLA PRIMARIA" ~ 0,
          .data$ORDINESCUOLA == "SCUOLA SECONDARIA I GRADO" ~ 5 ,
          .data$ORDINESCUOLA == "SCUOLA SECONDARIA II GRADO" ~ 8 ) )
    }
    x <- x %>% dplyr::mutate(ORDINESCUOLA = dplyr::case_when(
      .data$ORDINESCUOLA == "SCUOLA PRIMARIA" ~ "Primary",
      .data$ORDINESCUOLA == "SCUOLA SECONDARIA I GRADO" ~ "Middle",
      .data$ORDINESCUOLA == "SCUOLA SECONDARIA II GRADO" ~ "High"))

    return(x)
  })

  tabrename.nstud <- tabrename.nstud()

  input.nstud0 <- lapply(input.nstud0, function(x){
    for (j in (1:ncol(x))){
      if (names(x)[j] %in% tabrename.nstud$Input){
        names(x)[j] <- tabrename.nstud[which(tabrename.nstud$Input == names(x)[j]), 2]
      }
    }
    return(x)
  })

  endtime <- Sys.time()
  if(verbose) cat(paste(difftime(endtime, start.zero, units="secs"), " seconds needed to retrieve students number data \n" ) )

  return(input.nstud0)
}






