#' Download the Invalsi census survey data
#'
#' @description  Downloads the full database of the Invalsi scores, detailed either at the municipality or province level.
#'
#'
#'
#' @param multiple_out Logical. Wheter keeping
#' multiple dataframes as outputs (thus overriding the \code{level} argument) or not.
#' We advise to keep this option active in order not to lose information. \code{TRUE} by default.
#' @param level Character. The level of aggregation of Invalsi census data, such as \code{"NUTS-3"}, \code{"Province"} for provinces; or \code{"LAU"}, \code{"Municipality"} for muncipalities.
#' Only useful when \code{multiple_out == FALSE}. \code{"LAU"} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' \code{TRUE} by default.
#' @param category Logical. Whether to focus on a specific category of students participating to the census survey. Warning: experimental. \code{FALSE} by default.
#'
#' @return If \code{multiple_out == FALSE}, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' Otherwise, a list including objects of the aforementioned classes
#'
#' @source  <https://serviziostatistico.invalsi.it/en/archivio-dati/?_sft_invalsi_ss_data_collective=open-data>
#'
#' @details
#' Numeric variables provided are:
#' \itemize{
#'   \item \code{Average_percentage_score} Average direct score (percentage of sufficient tests)
#'   \item \code{Std_dev_percentage_score} Standard deviation of the direct score
#'   \item \code{WLE_average_score} Average WLE score. The WLE score is calculated through the Rasch's psychometric model and is suitable for middle and high schools in that it is cleaned from the effect of cheating  (which would affect both the average score and the score variability). By construction it has a mean around 200 points.
#'   \item \code{Std_dev_WLE_score} Standard deviation of the WLE score. By construction it ranges around 40 points at the school level.
#'   \item \code{Students_coverage} Students coverage percentage
#' }
#' Additional numeric variables, not always available for all observational units, are:
#' \itemize{
#'   \item Mean and SD of ESCS indicator
#'   \item \code{First-Fifth_Level}: Distribution of the proficiency level of students
#'   \item \code{Targets_percentage}: Percentage of students reaching targets
#' }
#' Numeric codes \code{888} and \code{999} denote not applicable and not available fields respectively.
#'
#' If \code{multiple_out == TRUE}, provides the following datasets:
#' \itemize{
#'   \item \code{Municipality_data}: LAU-level data
#'   \item \code{Province_data}: NUTS-3-level data
#'   \item \code{Region_data}: NUTS-2-level data
#'   \item \code{LLS_data}: data at the level of local labour systems
#'   (Sistemi Locali del Lavoro; see \href{https://www.istat.it/statistiche-per-temi/focus/informazioni-territoriali-e-cartografiche/statistiche-sul-territorio/sistemi-locali-del-lavoro-e-distretti-industriali/}{ISTAT webpage} for details)
#'   \item \code{Inner_Areas_2021_data} aggregated data for inner areas according to the 2020 taxonomy
#'   \item \code{Inner_Areas_2014_data} aggregated data for inner areas according to the former 2014 taxonomy
#'   \item \code{Macroarea_data} data aggregated for North-West, North-East, Center, South and Islands
#' }
#'
#' @examples
#' \donttest{
#' Data_Invalsi <- Get_Invalsi_IS(level = "NUTS-3", autoAbort = TRUE, verbose = FALSE)
#' Data_Invalsi$Province_data
#' }
#'
#'
#' @export


Get_Invalsi_IS <- function(level = "LAU", verbose = TRUE, show_col_types = FALSE,
                           multiple_out = TRUE, autoAbort = FALSE, category = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  starttime <- Sys.time()
  if (verbose) cat("Retrieving Invalsi census data ... \n")
  url.invalsi <- "https://serviziostatistico.invalsi.it/en/invalsi_ss_data/punteggi-e-percentuale-di-studenti-nei-livelli-di-competenza-per-ripartizioni-territoriali-e-caratteristiche-di-contesto/"

  homepage <- tryCatch({
    xml2::read_html(url.invalsi)
  }, error = function(e){
    NULL
  })
  if(!is.null(homepage)){
    name_pattern <- "report_generale"
  } else {
    message("It seems INVALSI changed address. \n Please contact the maintainer and ask him to fix.")
    return(NULL)
  }

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  # Warning: this works for the current website architecture - may
  # change in the future
  links <- links[grepl(paste0(name_pattern), links)]
  link <- unique(links[grepl("zip", links)])
  if(length(link)==0L){
    message("It seems INVALSI changed address. \n Please contact the maintainer and ask him to fix.")
    return(NULL)
  }
  base.url <- dirname(url.invalsi)
  file.url <- xml2::url_absolute(link, base.url)
  temp1 <- tempfile(fileext=".zip")
  timeout.default <- options("timeout")$timeout
  on.exit(options(timeout = timeout.default))
  options(timeout = max(timeout.default, 180))

  attempt <- 0
  x <- NULL
  while(is.null(x)){
    x <- tryCatch({
      utils::download.file(url = file.url, destfile = temp1, mode = "wb", quiet = TRUE)},
      error = function(e) {
        NULL
      })
    if(is.null(x)){
      attempt <- attempt + 1
      message("Error in file downloading, ", 10-attempt, " attempts left.\n")
    }
    if(attempt >= 10){
      unlink(temp1, recursive = TRUE, force = TRUE)
      message("Unable to download data; max attempts reached.")
      message("Abort. Please contact the maintiner and ask him to fix.\n")
      return(NULL)
    }
  }

  options(timeout = timeout.default)

  attempt <- 0
  Invalsi_IS <- NULL
  while(is.null(Invalsi_IS)){
    zip.content <- utils::unzip(temp1, list = TRUE)
    content.csv <- zip.content[which(grepl("\\.csv$", zip.content$Name, ignore.case = TRUE)),]
    if(nrow(content.csv) > 0L){
      csv.max <- which.max(zip.content$Length)
      filename <- zip.content$Name[csv.max]
      con <- unz(temp1, filename)
      Invalsi_IS <- tryCatch({
        readr::read_delim( con, delim = ";", na = c("", " ", "N.A."),
                           locale = readr::locale(decimal_mark = ",",
                                                  encoding = "ISO-8859-1"),
                           show_col_types = show_col_types )},
        error = function(e){
          message("For some reason, R cannot open the .csv file... \n")
          return(NULL)
        })
    }
    if(is.null(Invalsi_IS)) {
      message("Retry operation. ", 10-attempt, " attempts left... \n")
      attempt <- attempt + 1
    }
    if(attempt >= 10) {
      message("Impossible to read Invalsi file; max attempts reached. Abort. \n",
      "Please contact the maintainer and ask him to fix. \n")
      return(NULL)
    }
  }

  names(Invalsi_IS) <- names(Invalsi_IS) %>%
    stringr::str_replace_all("Aggregato_territoriale","Territorial_aggregate") %>%
    stringr::str_replace_all("aggregazione","Level") %>%
    stringr::str_replace_all("Denominazione", "Description") %>%
    stringr::str_replace_all("Codice_provincia", "Province_code") %>%
    stringr::str_replace_all("Nome_provincia", "Province_description") %>%
    stringr::str_replace_all("Sigla_provincia", "Province_initials") %>%
    stringr::str_replace_all("id", "ID") %>%
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
    stringr::str_replace_all("Percentuale_partecipazione", "Students_coverage")

  Invalsi_IS$Subject <- Invalsi_IS$Subject %>%
    stringr::str_replace_all("Matematica", "Mathematics") %>%
    stringr::str_replace_all("Italiano", "Italian") %>%
    stringr::str_replace_all("Inglese", "English")




  Invalsi_IS <- Invalsi_IS %>%
    dplyr::mutate(Description = stringr::str_to_title(.data$Description))

  # This is a loss in information next package versions shall overcome:
  if(!category){
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::filter(.data$Level == "Totale") %>%
      dplyr::select(-.data$Level)
  } else{
    categories <- unique(Invalsi_IS$Level)
    categories_mat <- apply(cbind(c(1:length(categories)), categories), 1,
                            function(X) paste0(X, collapse = ": "))
    cat("Please, choose one or more numbers \n corresponding to the following categories;
         \n use the semicolon (;) as separator \n",
         paste(categories_mat, collapse = "\n"))
    nn_categories <- as.numeric(unlist(strsplit(readline(prompt = "  > "), split = ";")))
    categories_in <- categories[nn_categories]
    Invalsi_IS <- Invalsi_IS %>%
      dplyr::filter(.data$Level %in% categories_in) %>%
      dplyr::rename( Category = .data$Level)
    }


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
  dd_LLS <- Invalsi_IS %>% dplyr::filter(.data$Territorial_aggregate == "Sistemi locali del lavoro (SLL)") %>%
    dplyr::rename(LLS_code = .data$ID) %>%
    dplyr::rename(LLS_description = .data$Description) %>%
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
    dplyr::select(-.data$Territorial_aggregate) %>%
    dplyr::mutate(dplyr::across(
      .data$Macroarea_description, ~ stringr::str_replace_all(.x, c(
         "Nord Ovest" = "North-West",
        "Nord Est" = "North-East",
        "Centro" = "Center",
        "Sud E Isole" = "South and Islands",
        "Sud" = "South"))))

  if(multiple_out){
    Invalsi_IS <- list(Municipality_data = dd_mun, Province_data = dd_prov,
                       Region_data = dd_reg, LLS_data = dd_LLS,
                       Inner_Areas_2014_data = dd_InnerAreas2014,
                       Inner_Areas_2021_data = dd_InnerAreas2021,
                       Macroarea_data = dd_macroarea)
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
      } else if(toupper(level) %in% c("LWS", "LOCAL WORKING SYSTEM", "LOCAL WORK SYSTEM", "LOCAL LABOUR SYSTEM",
                                      "LOCAL LABOR SYSTEM",
                                      "LLS", "SISTEMA LOCALE DEL LAVORO")){
        Invalsi_IS <- dd_LLS
      } else if(toupper(level) %in% c("MACROAREA", "NUTS-1", "NCS", "NORTHCENTERSOUTH", "NORTHCENTRESOUTH")){
        Invalsi_IS <- dd_macroarea
      } else{
        message("Level not recognised: `", level, "`; municipality data returned by default. Sorry. \n")
        Invalsi_IS <- dd_mun
      }
  }



  unlink(temp1, recursive = TRUE, force = TRUE)


  endtime <- Sys.time()
  if(verbose) {
    cat(paste("Total running time to retrieve Invalsi", level, "data:",
              paste(round(difftime(endtime, starttime, units="secs") ,2)), "seconds \n"))
  }
  return(Invalsi_IS)
}
