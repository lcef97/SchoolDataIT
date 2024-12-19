#' Download the classification of peripheral municipalities
#'
#' @description
#' Retrieves the classification of Italian municipalities into six categories; classes D, E, and F are the so-called internal/inner areas; classes A, B and C are the central areas.
#'
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param verbose Logical. Whether to keep track of computational time. \code{TRUE} by default.
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @details
#' Classes are defined according to these criteria; see the methodological note (in Italian) for more detail:
#'
#' \itemize{
#'   \item A - Standalone pole municipalities, the highest degree of centrality;
#'   they are characterised by a thorough and self-sufficient combined endowment of school, health and transport infrastructure,
#'    i.e. there are at least a lyceum and a technical high school; a railway station of medium dimensions and a hospital provided with an emergency ward.
#'   \item B - Intermunicipality poles; the endowment of such infrastructures is complete if a small set of contiguous municipalities is considered
#' }
#' The remaining classes are defined in terms of the national distribution of the road distances from a municipality to the closest pole:
#' \itemize{
#'   \item C - Belt municipalities, travel time below the median (< 27'42'') .
#'   \item D - Intermediate municipalities, travel time between the median and the third quartile (27'42'' - 40'54'').
#'   \item E - Peripheral municipalities, travel time between the third quartile and 97.5th percentile (40'54'' - 1h 6' 54'').
#'   \item F - Ultra-peripheral municipalities, travel time over the 97.5th percentile (>1h 6' 54'').
#' }
#' For more information regarding the dataset, it is possible to check the ISTAT methodological note (in Italian) available at <https://www.istat.it/it/files//2022/07/FOCUS-AREE-INTERNE-2021.pdf>
#'
#'
#' @examples
#'
#' \donttest{
#' InnerAreas <- Get_InnerAreas(autoAbort = TRUE)
#' InnerAreas[, c(1,9,13)]
#' }
#'
#' @source  <https://www.istat.it/notizia/la-geografia-delle-aree-interne-nel-2020-vasti-territori-tra-potenzialita-e-debolezze/>
#'
#'
#'
#' @export


Get_InnerAreas <- function(verbose = TRUE, autoAbort = FALSE){

  # This version does not require readxl to reduce the number of dependencies.
  # For a faster version requiring it, please ask the maintainer.
  if(!Check_connection(autoAbort)) return(NULL)

  starttime <- Sys.time()

  home.ISTAT <-"https://www.istat.it/notizia/la-geografia-delle-aree-interne-nel-2020-vasti-territori-tra-potenzialita-e-debolezze/"
  homepage <- NULL
  attempt <- 0
  while(is.null(homepage) && attempt <= 10){
    homepage <- tryCatch({
      xml2::read_html(home.ISTAT)
    }, error = function(e){
      message("Cannot read the html; ", 10 - attempt,
              " attempts left. If the problem persists, please contact the maintainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) {
    message("Maximum attempts reached. Abort. We apologise for the inconvenience")
    return(NULL)
  }

  name_pattern <- "Elenco_Comuni_Classi_di_Aree_Interne"
  link <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  link <- grep(name_pattern, link, value = TRUE)

  temp <- tempdir()
  if(!dir.exists(temp)){
    dir.create(temp)
  }

  status <- 0
  attempt <- 0
  while(status != 200 && attempt <= 10){
    base.url <- dirname(home.ISTAT)
    file.url <- xml2::url_absolute(link, base.url)
    response <- httr::GET(file.url, httr::write_disk(tempfile(fileext = ".xlsx")),  httr::config(timeout = 60))
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

  excel <- response$request$output$path

  utils::unzip(excel, exdir = temp)

  sheet_path <- file.path(temp, "xl", "worksheets", "sheet1.xml")
  shared_strings_path <- file.path(temp, "xl", "sharedStrings.xml")

  strings0 <- xml2::read_xml(shared_strings_path)

  ns <- xml2::xml_ns(strings0)

  shared_strings <- strings0 %>%
    xml2::xml_find_all(".//d1:si/d1:t", ns = ns) %>%
    xml2::xml_text()

  sheet <- xml2::read_xml(sheet_path)
  rows <- xml2::xml_find_all(sheet, "//d1:row", ns = xml2::xml_ns(sheet))

  lst <- lapply(rows, function(X){
    cells <- xml2::xml_find_all(X, "d1:c", ns = ns)
    strings <- as.vector(unlist(lapply(cells, function(Y) {
      id <- xml2::xml_find_first(Y, "d1:v", ns = ns) %>%
        xml2::xml_text()
      if(!is.na(xml2::xml_attr(Y, "t"))){
        return(gsub("\\s+$", "", as.character(shared_strings[as.numeric(id) + 1])))
      } else {
        return(id)
      }
    })))
    if(length(strings) > 16) strings <- strings[c(1:16)]
    return(strings)
  })
  lst <- lst[which(lapply(lst, length)==16)]
  res <- data.frame(do.call(rbind, lst[-1]))

  names(res) <-
    c("Municipality_code", "Municipality_code_numeric", "Cadastral_code", "Region_code",
      "Region_description", "Province_code", "Province_initials", "Province_description", "Municipality_description",
      "Inner_area_code_2014_2020", "Inner_area_description_2014_2020",
      "Inner_area_code_2021_2027", "Inner_area_description_2021_2027",
      "Destination_municipality_code", "Destination_municipality_description",
      "Destination_pole_code")

  res <- res %>%
    dplyr::mutate(Province_code = as.numeric(.data$Municipality_code)%/%1000) %>%
    dplyr::mutate(Municipality_code_numeric = as.numeric(.data$Municipality_code_numeric)) %>%
    structure(class = c("tbl_df", "tbl", "data.frame"))

  if(dir.exists(temp)){
    temp.contents <- list.files(temp, full.names = T)
    for(i in (1:length(temp.contents))) {
      unlink(temp.contents[i], recursive = TRUE)
    }
  }
  endtime <- Sys.time()
  if(verbose){
    cat(paste("Time needed to download inner areas taxomony:",
              round(difftime(endtime, starttime, units="secs"), 2), "seconds \n"  ))
  }

  return(res)
}
