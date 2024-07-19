#' Download the Invalsi census survey data
#'
#' @description  Downloads the full database of the Invalsi scores, detailed either at the municipality or province level.
#' The format is intermediate between long and short, since the numeric variables are:
#' \itemize{
#'   \item \code{Average_percentage_score} Average direct score (percentage of sufficient tests)
#'   \item \code{Std_dev_percentage_score} Standard deviation of the direct score
#'   \item \code{WLE_average_score} Average WLE score. The WLE score is calculated through the Rasch's psychometric model and is suitable for middle and high schools in that it is cleaned from the effect of cheating  (which would affect both the average score and the score variability). By construction it has a mean around 200 points.
#'   \item \code{Std_dev_WLE_score} Standard deviation of the WLE score. By construction it ranges around 40 points at the school level.
#'   \item \code{Students_coverage} Students coverage percentage
#' }
#'
#'
#'
#' @param level Character. The level of aggregation of Invalsi census data. Either \code{"NUTS-3"}, \code{"Province"}, \code{"LAU"}, \code{"Municipality"}. \code{"LAU"} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#'
#' @source  \href{https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-comunali-di-popolazione-comune-del-plesso/}{Municipality data};
#'                       \href{https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-provinciali-di-popolazione/}{Province data}
#'
#'
#'
#'
#' @examples
#' \donttest{
#' Get_Invalsi_IS(level = "NUTS-3", autoAbort = TRUE)
#' }
#'
#'
#' @export

Get_Invalsi_IS <- function(level = "LAU", verbose = TRUE, show_col_types = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  starttime <- Sys.time()
  if (level %in% c("Municipality", "LAU")){
    nCol <- 10
    if (verbose) cat("Retrieving Invalsi census data for municipalities: \n")
    url.invalsi <- "https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-comunali-di-popolazione-comune-del-plesso/"
    name_pattern <- "report_comuni_plessi-2.csv"
  } else if (level %in%c("Province", "NUTS-3") ){
    nCol <- 11
    if (verbose) cat("Retrieving Invalsi census data for provinces")
    url.invalsi <- "https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-provinciali-di-popolazione/"
    name_pattern <- "matrice_medie_provinciali-3.csv"
  }

  homepage <- NULL
  while(is.null(homepage)){
    homepage <- tryCatch({
      xml2::read_html(url.invalsi)
    }, error = function(e){
      NULL
    })
  }

  Invalsi_IS <- NULL
  status <- 0
  attempt <- 0
  while(status != 200 && attempt <= 10){
    link <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    link <- grep(name_pattern, link, value = TRUE) %>% unique()
    response <- tryCatch({
      httr::GET(link)
    }, error = function(e) {
      message("Error occurred during scraping, attempt repeated ... \n")
      NULL
    })
    if(is.null(response)){
      status <- 0
    }else{
      status <- response$status_code
      if(status != 200) {
        if(!autoAbort){
          message("Error occurred; connection exited with status ", status, " ; ", 10 - attempt, " attempts left \n",
                  "To abort the operation, press `A`; to hold on press any key \n")
          holdOn <- readline(prompt = "    ")
          if(holdOn == "A") {
            message("You chose to abort the operation. We apologise for the inconvenience.")
            return(NULL)
          }
        } else return(NULL)
      }
    }
    attempt <- attempt + 1
  }
  if(status == 200){
    if(verbose) cat("Encoding raw content in UTF-8 \n")
    content.UTF8 <- iconv(rawToChar(response$content), from = "ISO-8859-1", to = "UTF-8")
    Invalsi_IS <- readr::read_delim(content.UTF8, delim = ";",
                                    show_col_types = show_col_types, locale = readr::locale(decimal_mark = ","))
  } else {
    message("Maximum (10) attempts exceeded. We apologise for the inconvenience.")
    return(NULL)
  }

  Invalsi_IS[,c((nCol-4):nCol)] <- Invalsi_IS[,c((nCol-4):nCol)] %>%
    apply(MARGIN = 2, FUN = function(x){as.numeric(gsub(",", ".", x))} )
  names(Invalsi_IS) <- names(Invalsi_IS) %>%
    stringr::str_replace_all("Codice_provincia", "Province_code") %>%
    stringr::str_replace_all("Nome_provincia", "Province_description") %>%
    stringr::str_replace_all("Sigla_provincia", "Province_initials") %>%
    stringr::str_replace_all("id_comune", "Municipality_code") %>%
    stringr::str_replace_all("Comune", "Municipality_description") %>%
    stringr::str_replace_all("Grado", "Grade") %>%
    stringr::str_replace_all("Materia", "Subject") %>%
    stringr::str_replace_all("Anno", "Year") %>%
    stringr::str_replace_all("Punteggio_wle_medio", "WLE_average_score") %>%
    stringr::str_replace_all("punteggio_medio_wle", "WLE_average_score") %>%
    stringr::str_replace_all("deviazione_standard_wle", "Std_dev_WLE_score") %>%
    stringr::str_replace_all("Dev_Std_Punteggio_wle", "Std_dev_WLE_score") %>%
    stringr::str_replace_all("perc_copertura_stu", "Students_coverage") %>%
    stringr::str_replace_all("Punteggio_percentuale_medio", "Average_percentage_score") %>%
    stringr::str_replace_all("Punteggio_medio", "Average_percentage_score") %>%
    stringr::str_replace_all("punteggio_medio", "Average_percentage_score") %>%
    stringr::str_replace_all("deviazione_standard", "Std_dev_percentage_score") %>%
    stringr::str_replace_all("Deviazione_standard", "Std_dev_percentage_score") %>%
    stringr::str_replace_all("Dev_Std_Punteggio_Percentuale", "Std_dev_percentage_score")

  Invalsi_IS$Subject <- Invalsi_IS$Subject %>%
    stringr::str_replace_all("Matematica", "Mathematics") %>%
    stringr::str_replace_all("Italiano", "Italian") %>%
    stringr::str_replace_all("Inglese", "English")

  if(level %in% c ("LAU","Municipality")){
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::mutate(Municipality_code = sprintf("%06d", .data$Municipality_code)) %>%
      dplyr::mutate(Municipality_description = stringr::str_to_title(.data$Municipality_description))
  } else {
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::mutate(Province_description = stringr::str_to_title(.data$Province_description))
  }

  endtime <- Sys.time()
  if(verbose) {
    cat(paste("Total running time to retrieve Invalsi", level, "data:",
              paste(round(difftime(endtime, starttime, units="secs") ,2)), "seconds \n"))
  }
  return(Invalsi_IS)
}
