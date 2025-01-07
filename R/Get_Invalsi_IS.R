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
#' @param multiple_out Logical. If the massive DB is downloaded, wheter keeping
#' both LAU-level and NUTS-3-level dataframes as outputs (thus overriding the \code{level} argument) or not.
#' \code{FALSE} by default.
#'
#' @return Unless \code{multiple_out == TRUE}, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' Otherwise, a list including objects of the aforementioned classes
#'
#' @source  <https://serviziostatistico.invalsi.it/en/archivio-dati/?_sft_invalsi_ss_data_collective=open-data>
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


Get_Invalsi_IS <- function(level = "LAU", verbose = TRUE, show_col_types = FALSE,
                           multiple_out = FALSE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  starttime <- Sys.time()
  if (toupper(level) %in% c("MUNICIPALITY", "LAU", "NUTS-4")){
    nCol <- 10
    if (verbose) cat("Retrieving Invalsi census data for municipalities: \n")
    url.invalsi <- "https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-comunali-di-popolazione-comune-del-plesso/"
    name_pattern <- "report_comuni_plessi"
  } else if (toupper(level) %in%c("PROVINCE", "NUTS-3") ){
    nCol <- 11
    if (verbose) cat("Retrieving Invalsi census data for provinces")
    url.invalsi <- "https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-provinciali-di-popolazione/"
    name_pattern <- "matrice_medie_provinciali"
  }

  homepage <- NULL
  massive_DB <- FALSE
  homepage <- tryCatch({
    xml2::read_html(url.invalsi)
  }, error = function(e){
    NULL
  })
  if(is.null(homepage)){
    cat("Level - specific Invalsi data not found. Let us try with the massive DB ...\n")
    url.invalsi <- "https://serviziostatistico.invalsi.it/en/invalsi_ss_data/punteggi-e-percentuale-di-studenti-nei-livelli-di-competenza-per-ripartizioni-territoriali-e-caratteristiche-di-contesto/"
    homepage <- tryCatch({
      xml2::read_html(url.invalsi)
    }, error = function(e){
      NULL
    })
    if(!is.null(homepage)){
      massive_DB <- TRUE
      name_pattern <- "report_generale"
    } else {
      message("Invalsi webpage not found. We apologise for the inconvenience. If the problem persists, please contact the maintainer.")
      return(NULL)
    }
  }

  Invalsi_IS <- NULL
  status <- 0
  attempt <- 0
  while(status != 200 && attempt <= 10){
    links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    if(!massive_DB){
      link <- unique(links[grepl(paste0(
        "(?=.*", name_pattern, ")(?=.*\\.csv$)(?!.*", "livelli", ")"), links, perl = TRUE)])
    } else {
      link <- unique(links[grepl(paste0(
        "^(?!.*tracciato)(?=.*", name_pattern, ")(?=.*\\.csv)"), links, perl = TRUE)])

    }
    response <- tryCatch({
      httr::GET(link, httr::progress())
    }, error = function(e) {
      message("Error occurred during scraping, attempt repeated ... \n")
      NULL
    })
    status <- response$status_code
    if(is.null(response)){
      status <- 0
    }
    if(status != 200) {
      attempt <- attempt + 1
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
  if(status == 200){
    if(verbose) cat("Encoding raw content in UTF-8 \n")
    content.UTF8 <- iconv(rawToChar(response$content), from = "ISO-8859-1", to = "UTF-8")
    Invalsi_IS <- readr::read_delim(content.UTF8, delim = ";",
                                    show_col_types = show_col_types, locale = readr::locale(decimal_mark = ","))
  } else {
    message("Maximum (10) attempts exceeded. We apologise for the inconvenience.")
    return(NULL)
  }
  if(!massive_DB){
    Invalsi_IS[,c((nCol-4):nCol)] <- Invalsi_IS[,c((nCol-4):nCol)] %>%
      apply(MARGIN = 2, FUN = function(x){as.numeric(gsub(",", ".", x))} )
  }

  names(Invalsi_IS) <- names(Invalsi_IS) %>%
    stringr::str_replace_all("Aggregato_territoriale","Territorial_aggregate") %>%
    stringr::str_replace_all("aggregazione","Misc_level") %>%
    stringr::str_replace_all("Denominazione", "Description") %>%
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
    stringr::str_replace_all("Punteggio_medio$", "Average_percentage_score") %>%
    stringr::str_replace_all("punteggio_medio$", "Average_percentage_score") %>%
    stringr::str_replace_all("deviazione_standard$", "Std_dev_percentage_score") %>%
    stringr::str_replace_all("Deviazione_standard$", "Std_dev_percentage_score") %>%
    stringr::str_replace_all("Dev_Std_Punteggio_Percentuale", "Std_dev_percentage_score") %>%
    stringr::str_replace_all("media_ESCS", "ESCS_average") %>%
    stringr::str_replace_all("Dev_Std_ESCS", "ESCS_Std_dev") %>%
    stringr::str_replace_all("Primo_Livello", "First_level") %>%
    stringr::str_replace_all("Secondo_Livello", "Second_level") %>%
    stringr::str_replace_all("Terzo_Livello", "Third_level") %>%
    stringr::str_replace_all("Quarto_Livello", "Fourth_level") %>%
    stringr::str_replace_all("Quinto_Livello", "Fifth_level") %>%
    stringr::str_replace_all("Perc_traguardi", "Targets_percentage") %>%
    stringr::str_replace_all("Percentuale_partecipazione", "Students_coverage") %>%
    stringr::str_replace_all("^id$", "ID")

  Invalsi_IS$Subject <- Invalsi_IS$Subject %>%
    stringr::str_replace_all("Matematica", "Mathematics") %>%
    stringr::str_replace_all("Italiano", "Italian") %>%
    stringr::str_replace_all("Inglese", "English")

  if(!massive_DB){
    if (toupper(level) %in% c("MUNICIPALITY", "LAU", "NUTS-4")){
      Invalsi_IS <- Invalsi_IS %>%
        dplyr::mutate(Municipality_code = sprintf("%06d", .data$Municipality_code)) %>%
        dplyr::mutate(Municipality_description = stringr::str_to_title(.data$Municipality_description))
    } else {
      Invalsi_IS <- Invalsi_IS %>%
        dplyr::mutate(Province_description = stringr::str_to_title(.data$Province_description))
    }
  } else{
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::mutate(Description = stringr::str_to_title(.data$Description))

    # This is a loss in information next package versions shall overcome:
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::filter(.data$Misc_level == "Totale") %>%
      dplyr::select(-.data$Misc_level)

    dd_mun <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Comune plesso") %>%
      dplyr::rename(Municipality_code = .data$ID) %>%
      dplyr::rename(Municipality_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)%>%
      dplyr::mutate(Municipality_code = sprintf("%06d", .data$Municipality_code))
    dd_prov <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Provincia") %>%
      dplyr::rename(Province_code = .data$ID) %>%
      dplyr::rename(Province_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)
    dd_reg <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Regione") %>%
      dplyr::rename(Region_code = .data$ID) %>%
      dplyr::rename(Region_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)
    dd_LWS <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Sistemi locali del lavoro (SLL)") %>%
      dplyr::rename(LWS_code = .data$ID) %>%
      dplyr::rename(LWS_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)
    dd_InnerAreas2014 <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Aree interne SNAI 2014") %>%
      dplyr::rename(Inner_area_code = .data$ID) %>%
      dplyr::rename(Inner_area_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate) # Not sure this is relevant
    dd_InnerAreas2021 <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Aree interne SNAI 2021") %>%
      dplyr::rename(Inner_area_code = .data$ID) %>%
      dplyr::rename(Inner_area_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)
    dd_macroarea <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Area geografica") %>%
      dplyr::rename(Macroarea_code = .data$ID) %>%
      dplyr::rename(Macroarea_description = .data$Description) %>%
      dplyr::select(-.data$Territorial_aggregate)

    if(multiple_out){
      Invalsi_IS <- list(Municipality_data = dd_mun, Province_data = dd_prov,
                         Region_data = dd_reg, LWS_data = dd_LWS, Inner_Areas_2014_data = dd_InnerAreas2014,
                         Inner_Areas_2021_data = dd_InnerAreas2021, Macroarea_data = dd_macroarea)
    } else{
      if (toupper(level) %in% c("MUNICIPALITY", "MUN", "LAU", "NUTS-4")){
        Invalsi_IS <- dd_mun
      } else if(toupper(level) %in% c("PROV", "PROVINCE", "NUTS-3")){
        Invalsi_IS <- dd_prov
      } else if(toupper(level) %in% c("REG", "REGION", "NUTS-2")){
        Invalsi_IS <- dd_reg
      } else if(toupper(level) %in% c("INNERAREAS", "INNER_AREAS", "INNERAREAS2020", "INNERAREAS20",
                                      "INNER_AREAS2020", "INNER_AREAS20", "INNERAREAS_2020", "INNERAREAS_20",
                                      "INNER_AREAS_2020", "INNER_AREAS_20", "INNERAREAS2021", "INNERAREAS21",
                                      "INNER_AREAS2021", "INNER_AREAS21", "INNERAREAS_2021", "INNERAREAS_21",
                                      "INNER_AREAS_2021", "INNER_AREAS_21")){
        Invalsi_IS <- dd_InnerAreas2021
      } else if(toupper(level) %in% c("INNERAREAS_OLD", "INNER_AREAS_OLD",  "INNERAREAS2014", "INNERAREAS14",
                                      "INNER_AREAS2014", "INNER_AREAS14", "INNERAREAS_2014", "INNERAREAS_14",
                                      "INNER_AREAS_2014", "INNER_AREAS_14")){
        Invalsi_IS <- dd_InnerAreas2014
      } else if(toupper(level) %in% c("LWS", "LOCAL WORKING SYSTEM", "LOCAL LABOUR SYSTEM", "LOCAL LABOR SYSTEM",
                                      "LLS", "SISTEMA LOCALE DEL LAVORO")){
        Invalsi_IS <- dd_LWS
      } else if(toupper(level) %in% c("MACROAREA", "NUTS-1", "NCS", "NORTHCENTERSOUTH", "NORTHCENTRESOUTH")){
        Invalsi_IS <- dd_macroarea
      } else{
        message("Level not recognised: `", level, "`; municipality data returned by default")
        Invalsi_IS <- dd_mun
      }
    }
  }

  endtime <- Sys.time()
  if(verbose) {
    cat(paste("Total running time to retrieve Invalsi", level, "data:",
              paste(round(difftime(endtime, starttime, units="secs") ,2)), "seconds \n"))
  }
  return(Invalsi_IS)
}
