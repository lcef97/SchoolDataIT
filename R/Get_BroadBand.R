#' Download the data regarding the broad band connection activation in Italian schools
#'
#' @description
#' Retrieves the data regarding the activation date of the ultra-broadband connection in schools
#' and indicates whether the connection was activated or not at a certain date.
#'
#'
#' @param Date Object of class \code{Date}. The date at which it is required
#' to determine if the broad band connection has been activated or not.
#' By default, the first day of last month, which we assume to be the last update of the dataset.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param include_municipality_code Logical. Whether to include municipality codes.
#'  \code{TRUE} by default.
#' @param input_School2mun Object of class \code{list} obtained with \code{\link{Get_School2mun}}.
#' If \code{include_municipality_code == TRUE}, the mapping from school codes to municipality (and province) codes.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_Registry If \code{input_School2mun} is required, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' corresponding to the national school registry (preferably of the last year, i.e. 2024/2025) obtained with \code{\link{Get_Registry}}. NULL by default.
#' @param input_AdmUnNames If \code{input_School2mun} is required, an object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' corresponding to the statistical codes of administrative units (preferably referring to the period corresponding to school year 2024/2025)
#'  obtained with \code{\link{Get_AdmUnNames}}. NULL by default.
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'  The variables \code{BB_Activation_date} and \code{BB_Activation_staus} indicate
#'  the activation date and activation status of the broadband connection at the selected date.
#'
#' @source  Broadband dashboard: <https://bandaultralarga.italia.it/scuole-voucher/dashboard-scuole/>
#'
#'
#' @details Ultra - Broadband is defined as everlasting internet connection with a
#' maximum speed of 1 gigabit per second, with a minimum guaranteed speed of
#' 100 megabits/second both on the uploading and downloading operations, until
#' the peering point is reached, as declared on the data provider's website:
#' <https://bandaultralarga.italia.it/scuole-voucher/progetto-scuole/>.
#' In the example the broadband availability at the beginning of school  year 2022/23 (1st september 2022) is shown.
#'
#' @examples
#'
#' \donttest{
#'
#'   Broadband_220901 <- Get_BroadBand(Date = as.Date("2022-09-01"), autoAbort = TRUE)
#'
#'   Broadband_220901
#'
#'   Broadband_220901[, c(9,6,13,14)]
#'
#' }
#'
#'
#'
#'
#' @export


Get_BroadBand <- function(Date = as.Date(format(as.Date(format(Sys.Date(), "%Y-%m-01"))-1, "%Y-%m-01")),
                          include_municipality_code = TRUE,
                          input_School2mun = NULL,
                          input_Registry = NULL,
                          input_AdmUnNames = NULL,
                          verbose=TRUE, autoAbort = FALSE){

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
              or contact the package maintainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) return(NULL)

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  link <- links[grep("Report", links, ignore.case = TRUE)]
  base.url <- dirname(home.url)

  file.url <- xml2::url_absolute(link, base.url)

  status <- 0
  attempt <- 0
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
      attempt <- attempt + 1
      message("Operation exited with status: ", status, "; operation repeated (",
              10 - attempt, " attempts left)")
    }
    if(attempt >= 10) {
      message("Maximum attempts reached. Abort. We apologise for the inconvenience")
      return(NULL)
    }
  }
  if(is.null(response$content)){
    message("It seems that Broadband data are not available.
            We apologise for the inconvenience")
    return(NULL)
  }

  temp <- tempdir()
  if(!dir.exists(temp)){
    dir.create(temp)
  }

  excel <- paste0(temp, "/tempdata.xlsx")
  writeBin(as.vector(response$content), excel)

  if(! "readxl" %in% rownames(utils::installed.packages())){
    if(verbose){
      cat("Parsing broadband data from the xml file ...\n")
    }
    utils::unzip(excel, exdir = temp)
    templist <- list.files(temp, full.names = TRUE)
    xl.link <- templist[which(substr(templist, nchar(templist)-1, nchar(templist))=="xl")]
    ws.link <- grep("worksheets", list.files(xl.link, full.names = TRUE) , value = TRUE)
    #ws.link <- xl.link %>% list.files(full.names = TRUE) %>% grep("worksheets", ., value = TRUE)
    xml.link <-  grep(".xml", list.files(ws.link, full.names = TRUE), value = TRUE)

    worksheet1 <- xml2::read_xml(xml.link[1])
    descr.d1 <- xml2::xml_ns(worksheet1)[["d1"]]

    rows <- xml2::xml_find_all(worksheet1, ".//d1:sheetData/d1:row", namespaces = c(d1 = descr.d1))

    data_list <- lapply(rows, function(X) {
      cells <- xml2::xml_find_all(X, "./d1:c", ns = c(d1 = descr.d1))

      row.ls <- sapply(cells, function(Y) {
        attr <- xml2::xml_attr(Y, "t")
        val_node <- xml2::xml_find_first(Y, ".//d1:v", ns = c(d1 = descr.d1))

        if (!is.na(attr) && attr == "inlineStr") {
          return(xml2::xml_text(
            xml2::xml_find_first(Y, ".//d1:t", ns = c(d1 = descr.d1))))
        } else if (!is.na(val_node)) {
          return(xml2::xml_text(val_node))
        } else return(NA)
      })
      row.value <- as.vector(unlist(row.ls))

      cell_refs <- xml2::xml_attr(cells, "r")
      row.idx <- as.numeric(regmatches(cell_refs[1], gregexpr("[0-9]+", cell_refs[1] )))
      if(!is.na(row.idx)){
        if(verbose && row.idx%%5000 == 0){
          cat("Parsed the first ", row.idx, "rows \n")
        }
      }

      if(length(row.value)>0){
        if(!is.na(row.value[1])){
          return(row.value)
        } else return (NA)
      }
    })

    dd <- as.data.frame(do.call(rbind, data_list[-1]))
    names(dd) <- data_list[[1]]
    dd <- dd %>% dplyr::filter(!is.na(.data$Comune)) %>%
      dplyr::group_by(.data$`Codice univoco Infratel`) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Latitudine = as.numeric(.data$Latitudine)) %>%
      dplyr::mutate(Longitudine = as.numeric(.data$Longitudine))

  } else {
    dd <- readxl::read_xlsx(excel, col_types = "text")
  }

  while(!"Date" %in% class(Date)){
    message("Please, provide a date in format `yyyy-mm-dd` (do not insert quotes in the prompt)")
    Date.new <- readline(prompt = "")
    Date <- as.Date(Date.new)
  }

  broadband <- dd %>% dplyr::filter(substr(.data$`Codice Sede`,1,2) != "XX") %>%
    dplyr::select(.data$Regione, .data$Provincia, .data$Comune, .data$`Codice Sede`,
                  .data$`Codice univoco Infratel`, .data$Denominazione,
                  .data$Latitudine, .data$Longitudine,
                  .data$`Date prevista attivazione`, .data$`Data effettiva attivazione`)%>%
    dplyr::mutate(`Date prevista attivazione` =
                    as.numeric(.data$`Date prevista attivazione` )- 25569) %>%
    dplyr::mutate(`Data effettiva attivazione` =
                    as.numeric(.data$`Data effettiva attivazione`) - 25569) %>%
    dplyr::mutate(
      BB_Activation_date =as.Date(
        dplyr::coalesce(.data$`Data effettiva attivazione`,
                        .data$`Date prevista attivazione`))) %>%
    dplyr::mutate(Provincia = toupper(.data$Provincia)) %>%
    dplyr::select(-.data$`Data effettiva attivazione`,
                  -.data$`Date prevista attivazione`) %>%
    dplyr::rename(Region_description = .data$Regione) %>%
    dplyr::rename(Province_description = .data$Provincia) %>%
    dplyr::rename(Municipality_description = .data$Comune) %>%
    dplyr::rename(School_code = .data$`Codice Sede`) %>%
    dplyr::rename(School_name = .data$Denominazione) %>%
    dplyr::rename(Infratel_code = .data$`Codice univoco Infratel`) %>%
    dplyr::rename(Latitude = .data$Latitudine) %>%
    dplyr::rename(Longitude = .data$Longitudine) %>%
    School.order() %>%
    dplyr::mutate(BB_Activation_status = as.numeric(.data$BB_Activation_date <= Date)) %>%
    dplyr::mutate(Province_description = ifelse(
      grepl("-CESENA$", .data$Province_description),
      "FORLI'-CESENA", .data$Province_description)) %>%
    dplyr::left_join(prov.names()[,-c(3, 4)], by = "Province_description") %>%
    dplyr::relocate(.data$Region_code, .before = 1) %>%
    dplyr::relocate(.data$Province_code, .after = .data$Region_description)

  if(dir.exists(temp)){
    temp.contents <- list.files(temp, full.names = T)
    for(i in (1:length(temp.contents))) {
      unlink(temp.contents[i], recursive = TRUE)
    }
  }

  if(include_municipality_code){
    broadband <- Util_BroadBand2mun(broadband, input_School2mun = input_School2mun,
                                    input_Registry = input_Registry, input_AdmUnNames = input_AdmUnNames,
                                    autoAbort = autoAbort, verbose = FALSE)
  }

  endtime <- Sys.time()
  if (verbose && !is.null(broadband)) {
    cat(paste(round(difftime(endtime, starttime, units="secs") ,2),
              "seconds required to download ultra-broadband activation data \n") )
  }
  return(broadband)
}
