#' Display data fom the school buildings database
#'
#' @description  This function displays a map of the data downloaded trough the \code{\link{Get_DB_MIUR}} function.
#' It supports two kinds of map:
#' \itemize{
#'   \item Interactive map (default option), which allows the user to visualize all the data in scope through the interactive popup, and
#'   \item Static map (ggplot), which can be easily exported in \code{.pdf} objects.
#' }
#'
#'
#'
#' @param data Object of class \code{list} or \code{tbl_df}, \code{tbl} and \code{data.frame}. Input data obtained as output of the function \code{\link{Group_DB_MIUR}}
#' If NULL, it will be downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param field Character. The variable to display in the map.
#' @param order Character. The school order. Either \code{"Primary"}, \code{"Middle"}, or \code{"High"} (high school).
#' If \code{NULL}, an average of the three school orders will be displayed for the target variable. \code{NULL} by default.
#' @param level Character. The administrative level of detailed at which the target variable must be displayed.
#' Either \code{"LAU"}/\code{"Municipality"} or \code{"NUTS-3"}/\code{"Province"}. \code{"LAU"} by default.
#' @param region_code Numeric. The NUTS-2 codes of the units that must be displayed.
#' If the level is set to \code{"LAU"}, choosing a limited number of regions is recommended.
#' By default, \code{c(1:20)}, i.e. all Italian regions.
#' @param plot Character. The type of map to display; either \code{"mapview"} for interactive maps, or \code{"ggplot"} for static maps. \code{"mapview"} by default.
#' @param pal Character. The palette to use if the \code{"mapview"} mode is chose. \code{"Blues"} by default.
#' @param col_rev Logical. Whether the scale of the colour palette should be reverted or not, if the \code{"mapview"} mode is chosen. \code{FALSE} by default
#' @param popup_height Numeric. The height of the popup table in terms of pixels if the \code{"mapview"} mode is chosen. \code{200} by default.
#' @param main_pos Character. Where the header should be placed if the \code{ggplot} mode is chosen.
#' The header is located on the top if \code{"top"} is given as input, and above the legend scale otherwise. \code{"top"} by default.
#' @param main Character. The customary title to display in the \code{"ggplot"} rendering options
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.s
#' @param input_shp Object of class \code{sf}, \code{tbl_df}, \code{tbl}, \code{data.frame}. The relevant shapefiles of Italian administrative boudaries,
#' at the selected level of detail (LAU or NUTS-3). If \code{NULL} it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... If \code{data} is not provided, the arguments to \code{\link{Group_DB_MIUR}}.
#'
#'
#'
#' @return If \code{plot == "mapview"}, an object of class \code{mapview}. Otherwise, if \code{plot == "ggplot"}, an object of class \code{gg} and \code{ggplot}.
#'
#'
#'
#' @examples
#'
#'
#'
#'
#'
#'   library(magrittr)
#'
#'   DB23_MIUR <- example_input_DB23_MIUR %>%
#'     Util_DB_MIUR_num(track.deleted = FALSE) %>%
#'     Group_DB_MIUR(InnerAreas = FALSE, count_missing = FALSE)
#'
#'   DB23_MIUR %>% Map_School_Buildings(field = "School_bus",
#'      order = "Primary",level = "NUTS-3",  plot = "ggplot",
#'      input_shp = example_Prov22_shp)
#'
#'   DB23_MIUR %>% Map_School_Buildings(field = "Railway_transport",
#'      order = "High",level = "NUTS-3", plot = "ggplot",
#'      input_shp = example_Prov22_shp)
#'
#'   DB23_MIUR %>% Map_School_Buildings(field = "Context_without_disturbances",
#'      order = "Middle",level = "NUTS-3", plot = "ggplot",
#'      input_shp = example_Prov22_shp, col_rev = TRUE)
#'
#'
#'
#'
#'
#' @export


Map_School_Buildings <- function (data = NULL, field, order = NULL,  level = "LAU",
                                  region_code = c(1:20), plot = "mapview", pal = "Blues",
                                  col_rev = FALSE, popup_height = 200,
                                  main_pos = "top", main = "", verbose = TRUE,
                                  input_shp = NULL, autoAbort = FALSE, ... ) {
  options(dplyr.summarise.inform = FALSE)

  while(is.null(data)){
    if(verbose) cat("Loading input data: \n")
    data <- Group_DB_MIUR(autoAbort = autoAbort, ...)
    if(is.null(data)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during school buildings DB retrieving. Would you abort the whole operation or retry?",
                "    - To abort the operation, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          return(NULL)
        } else {
          cat("You chose to retry \n")
        }
      } else return(NULL)
    }
  }

  if((is.data.frame(data) & "School_code" %in% names(data)) |
     (!is.data.frame(data) & "School_code" %in% names(data[[1]]))){
       data <- Group_DB_MIUR(data, ...)
     }

  if(!is.data.frame(data)){
    if(any(grepl("missing", names(data)))){
      if(level %in% c("LAU", "Municipality")){
        DB <- data$Municipality_data %>%
          dplyr::left_join(data$Municipality_missing, by = c("Municipality_code", "Order"))
      } else {
        DB <- data$Province_data %>%
          dplyr::left_join(data$Province_missing, by = c("Province_code", "Order"))
      }
    } else {
      if(level %in% c("LAU", "Municipality")){
        DB <- data$Municipality_data
      } else DB <- data$Province_data
    }
  } else DB <- data

  Year <- as.numeric(DB$Year[1])%/%100 + 1
  YearMinus1 <- Year - 1

  while(is.null(input_shp)){
    if (verbose) cat("Loading shapefile: \n")
    input_shp <- Get_Shapefile(Year = ifelse(
      any(year.patternA(Year) %in% c(year.patternA(2016), year.patternA(2018))), Year, YearMinus1),
      level = level, autoAbort = autoAbort)
    if(is.null(input_shp)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during shapefile retrieving. Would you abort the whole operation or retry?",
                "    - To abort the operation, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          return(NULL)
        } else {
          cat("You chose to retry \n")
        }
      } else return(NULL)
    }
  }

#  if(level %in% c ("LAU", "Municipality")){
#    Mun_shp <- input_shp
#  } else if(level %in% c("NUTS-3", "Province") ) Prov_shp <- input_shp else {
#    stop("Please, choose either 'LAU'('Municipality') or 'NUTS-3' ('Province') as level")
#  }

  if(verbose) cat("Setting target variables... \n")

  if (level %in% c("LAU", "Municipality") ){
    dat.R <- DB %>% dplyr::filter(.data$Order != "NR")

    # formerly across(all_of(c(3:ncol(.)-4)))
    if (is.null(order)){
      dat.R <- dat.R %>%
        dplyr::filter(.data$Order != "IC") %>%
        dplyr::filter(.data$Order != "IS") %>%
        dplyr::group_by(
        .data$Municipality_code, .data$Municipality_description,
        .data$Province_code, .data$Province_initials) %>%
        dplyr::summarise(nbuildings = sum(.data$nbuildings),
                         dplyr::across(c(3:(ncol(dat.R)-4)), list(MeanOrMode))) %>%
        dplyr::ungroup()
      names(dat.R) <- names(dat.R) %>% stringr::str_remove_all("_1")
    } else {
      dat.R <- dat.R %>%
        dplyr::filter(.data$Order == order) %>%
        dplyr::select(-.data$Order)
    }

    res <- input_shp %>% dplyr::select(.data$COD_REG, .data$PRO_COM_T) %>%
      rename_by_idx(2, "Municipality_code") %>%
      dplyr::filter(.data$COD_REG %in% region_code) %>%
      dplyr::left_join(dat.R, by = "Municipality_code")

  } else {

    dat.R <- DB %>% dplyr::filter(.data$Order != "NR")

    if (is.null(order)){
      if(verbose){message("No school order has been selected. A global average will be displayed")}
      dat.R <- dat.R %>%
        dplyr::filter(.data$Order != "IC") %>%
        dplyr::filter(.data$Order != "IS") %>%
        dplyr::group_by(.data$Province_code, .data$Province_initials) %>%
        dplyr::summarise(nbuildings = sum(.data$nbuildings), dplyr::across(c(3:(ncol(dat.R)-2)), list(MeanOrMode))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Province_code = as.numeric(.data$Province_code))
      names(dat.R) <- names(dat.R) %>% stringr::str_remove_all("_1")
    } else {
      dat.R <- dat.R %>%
        dplyr::filter(.data$Order == order) %>%
        dplyr::select(-.data$Order) %>%
        dplyr::mutate(Province_code = as.numeric(.data$Province_code))
    }

    res <- input_shp %>% dplyr::select(.data$COD_REG, .data$COD_PROV)  %>%
      rename_by_idx(2, "Province_code")  %>%
      dplyr::filter(.data$COD_REG %in% region_code) %>%
      dplyr::left_join(dat.R , by ="Province_code")
  }

  fieldname <- ifelse(is.numeric(field), names(res)[field], field)
  nfield <- ifelse(is.numeric(field), field, match(field, names(res)))

  while(! fieldname %in% names(res)){
    message(paste("The variable", field, "does not seem to belong to the current database.
                  Please insert another one (do not use quotes in the prompt)"))
    field <- readline(prompt = "  > ")

    nfield <- ifelse(is.numeric(field), field, match(field, names(res)))
    fieldname <- ifelse(is.numeric(field), names(res)[field], field)
  }

  layername <- ifelse(main == "", paste(
    fieldname, ifelse(is.null(order),"",paste(order, "School"))), main)

  if(verbose) cat("Rendering:")

  if(plot == "ggplot"){

    if(col_rev == FALSE){
      fill.low = "#132B43"
      fill.high = "#56B1F7"
    } else {
      fill.low = "#56B1F7"
      fill.high = "#132B43"
    }

    if(main_pos == "top"){
      ggplot2::ggplot() + ggplot2::geom_sf(data = res, ggplot2::aes(fill = !!rlang::sym(fieldname))) +
        ggplot2::labs(fill = "") + ggplot2::ggtitle(layername) +
        ggplot2::scale_fill_gradient(high = fill.high, low = fill.low)
    } else {
      ggplot2::ggplot() + ggplot2::geom_sf(data = res, ggplot2::aes(fill = !!rlang::sym(fieldname))) +
        ggplot2::labs(fill = fieldname) +
        ggplot2::scale_fill_gradient(high = fill.high, low = fill.low)
    }
  } else {

    #n <- length(unique(unlist(sf::st_drop_geometry(res[,nfield])))) - 1
    n <- length(unique(unlist(dplyr::select(dplyr::filter(
      dat.R, .data$Province_code %in% res$Province_code), field)))) - 1
    if (col_rev == FALSE) {
      brew <- grDevices::hcl.colors(n, palette = pal)
    } else {
      brew <- rev(grDevices::hcl.colors(n, palette = pal))
    }
    pop = leafpop::popupTable(res)
    suppressWarnings(
      mapview::mapview(res, zcol = fieldname, col.regions = brew,
                       layer.name = layername,
                       popup = paste0(set_popup_height(popup_height) , pop) )
    )
  }
}
