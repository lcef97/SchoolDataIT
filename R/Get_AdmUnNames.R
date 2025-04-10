#' Download the names and codes of Italian LAU and NUTS-3 administrative units
#'
#' @description This function downloads a file provided by the Italian National Institute of Statistics including all the codes of administrative units in Italy. As of today, it is the easiest way to map directly cadastral codes to municipality codes.
#'
#' @param Date Character. The date at which administrative unit codes are sought for. Important: must be in the format: "yyyy-mm-dd".
#'  Current date by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, including: NUTS-3 code, NUTS-3 abbreviation,
#' LAU code, LAU name (description) and cadastral code. All variables are characters except for the NUTS-3 code.
#'
#' @examples
#' \donttest{
#'   Get_AdmUnNames("2025-01-01", autoAbort = TRUE)
#'
#' }
#'
#'
#'
#'
#' @source <https://situas.istat.it/web/#/territorio>
#'
#' @export



Get_AdmUnNames <- function(Date = Sys.Date(), autoAbort = FALSE){


  if(!Check_connection(autoAbort = autoAbort)) return(NULL)

  pattern0 <- c("https://situas.istat.it/ShibO2Module/api/Report/Spool/", "/61?&pdoctype=CSV")
  #date <- stringr::str_replace(date, "_", "-")
  #while(!date %in% c("01-01", "06-30", "09-01")) {
  # message("Please, choose either '01-01', '06-30' or '09-01' as date")
  #date <- readline(prompt = "  > ")
  #}
  while(!as.Date(Date) <= Sys.Date()){
    message("Please, choose a date prior to ", Sys.Date())
    Date <- readline(prompt = "  > ")
  }
  #Year <- 2000 + as.numeric(year.patternA(Year))%%100
  url<- paste0(pattern0[1], Date, pattern0[2])
  json_body <- "{}"


  status <- 0
  attempt <- 0
  while(status != 200 && attempt <= 10){

    response <- tryCatch({httr::POST(url,
                                     body = json_body,
                                     encode = "raw",
                                     httr::add_headers(
                                       `Content-Type` = "application/json-patch+json",
                                       `User-Agent` = "R (httr package)",
                                       `Origin` = "https://situas.istat.it",
                                       `Referer` = "https://situas.istat.it/web/"))
    }, error = function(e) return(NULL))

    status <- response$status_code
    if(is.null(response)){
      status <- 0
    }
    if(status != 200){
      attempt <- attempt + 1
      message("Operation exited with status: ", status, "; operation repeated (",
              10 - attempt, " attempts left)")
    }
    if(attempt >= 10) {
      message("Maximum attempts reached. Abort. We apologise for the inconvenience")
      return(NULL)
    }
  }

  DB <- NULL
  attempt <- 0
  while(is.null(DB) && attempt <= 10){
    DB <- tryCatch({
      readr::read_delim(rawToChar(httr::content(response, type = "raw")), delim = ";", show_col_types = FALSE)
    }, error = function(e){
      message("Cannot read the administrative unit names file; ", 10 - attempt,
              " attempts left. If the problem persists, please contact the maintainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(DB)) return(NULL)

  res <- DB %>%
    dplyr::select(.data$`Codice Provincia (Storico)`, .data$`Sigla automobilistica`,
                  .data$`Codice Comune (alfanumerico)`, .data$`Comune`,
                  .data$`Codice catasto`)
  names(res) <- c("Province_code", "Province_initials","Municipality_code",
                  "Municipality_description", "Cadastral_code")

  res$Province_code <- as.numeric(res$Province_code)
  res$Province_initials[which(res$Province_code == 63)] <- "NA"

  return(res)
}
