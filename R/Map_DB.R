#' Map school data
#'
#' @description This function displays a map of the data arranged trough the function \code{\link{Set_DB}}.
#' It supports two kinds of map:
#' \itemize{
#'   \item Interactive map (default option), which allows the user to visualize all the data in scope through the interactive popup, and
#'   \item Static map (ggplot), which can be easily exported in \code{.pdf} objects.
#' }
#'
#'
#' The user must select a variable to display.
#' It is possible to insert either a readily-downloaded database obtained through the function \code{\link{Set_DB}} or the basic inputs to plug in that function, other than an input shapefile. Relevant arguments not provided by the user will be download automatically, but not saved into the global environment. However we suggest to plug in at least some inputs, as otherwise the running time may be long.
#' This function generalises the functionalities of the more data-specific functions \code{\link{Map_School_Buildings}} and \code{\link{Map_Invalsi}}.
#'
#' @param data Object of class \code{tbl.df}, \code{tbl} and \code{data.frame}, obtained as output of the \code{\link{Set_DB}} function. If NULL, it will be arranged automatically but not saved into the global environment. NULL by default.
#' @param field Character. The variable to display in the map.
#' @param level Character. The administrative level of detailed at which the target variable must be displayed. Either \code{"LAU"}/\code{"Municipality"} or \code{"NUTS-3"}/\code{"Province"}. If the \code{"data"} argument is plugged in, please select the same level. \code{"LAU"} by default.
#' @param plot Character. The type of map to display; either \code{"mapview"} for interactive maps, or \code{"ggplot"} for static maps. \code{"mapview"} by default.
#' @param popup_height Numeric. The height of the popup table in terms of pixels if the \code{"mapview"} mode is chosen. \code{200} by default.
#' @param main_pos Character.Where the header should be placed if the \code{ggplot} mode is chosen.
#' The header is located on the top if \code{"top"} is given as input, and above the legend scale otherwise. \code{"top"} by default.
#' @param main Character. The title to display in the \code{"ggplot"} rendering options.
#' @param col_rev Logical. Whether the scale of the colour palette should be reverted or not. \code{FALSE} by default.
#' @param pal Character. The palette to use if the \code{"mapview"} mode is chose. \code{"Blues"} by default.
#' @param Year Numeric or Character. The reference school year, needed if either \code{data} or \code{input_shp} are not provided.
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}.  \code{2023} by default.
#' @param input_shp Object of class \code{sf}, \code{tbl.df}, \code{tbl} and \code{data.frame}. The relevant shapefiles of Italian administrative boundaries,
#' at the selected level of detail (LAU or NUTS-3). If \code{NULL}, it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param region_code Numeric. The NUTS-2 codes of the units that must be displayed.
#' If the level is set to \code{"LAU"}, choosing a limited number of regions is recommended.
#' By default, \code{c(1,3,5:20)}, i.e. all Italian regions except the provinces of Aosta, Trento and Bozen which have data availability issues.
#' @param order Character. The educational level. Either \code{"Primary"} (primary school), \code{"Middle"} (middle school), or \code{"High"} (high school).
#' If the data include the Invalsi census survey, please select a level consistent with the chosen educational grade. \code{"Media"} by default.
#  @param Invalsi Logical. whether the data to map include the Invalsi survey. \code{TRUE} by default.
#  @param Invalsi.subj Character. If \code{Invalsi == TRUE}, the school subject(s) to include, among \code{"English_listening"}/\code{"ELI"}, \code{"English_reading"}/\code{"ERE"}, \code{"Italian"}/\code{"Ita"} and \code{"Mathematics"}/\code{"MAT"}. All four by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Additional arguments for the input database, if not provided; see \code{\link{Set_DB}}
#'
#'
#' @return If \code{plot == "mapview"}, an object of class \code{mapview}. Otherwise, if \code{plot == "ggplot"}, an object of class \code{gg} and \code{ggplot}.
#'
#'
#' @examples
#'
#'
#'
#' DB23 <- Set_DB(Year = 2023, level = "NUTS-3",
#'        Invalsi_grade = c(10,13), NA_autoRM = TRUE,
#'        input_Invalsi_IS = example_Invalsi23_prov, input_nstud = example_input_nstud23,
#'        input_InnerAreas = example_InnerAreas,
#'        input_School2mun = example_School2mun23,
#'        input_AdmUnNames = example_AdmUnNames20220630,
#'        nteachers = FALSE, BroadBand = FALSE, SchoolBuildings = FALSE)
#'
#'
#'
#'
#' Map_DB(DB23, field = "Students_per_class_13", input_shp = example_Prov22_shp, level = "NUTS-3",
#'  col_rev = TRUE, plot = "ggplot")
#'
#' Map_DB(DB23, field = "Inner_area", input_shp = example_Prov22_shp, order = "High",
#'  level = "NUTS-3",col_rev = TRUE, plot = "ggplot")
#'
#' Map_DB(DB23, field = "M_Mathematics_10", input_shp = example_Prov22_shp, level = "NUTS-3",
#'  plot = "ggplot")
#'
#'
#' @import sf
#'
#' @export

Map_DB <- function(
    data = NULL,
    Year = 2023,
    field,
    level = "LAU",
    plot = "mapview",
    popup_height = 200,
    col_rev = FALSE,
    pal = "Blues",
    input_shp = NULL,
    region_code = c(1:20),
    main_pos = "top",
    main = "",
    order = NULL,
    autoAbort = FALSE,
    ...){

  #rlang::check_installed("sf", reason = "Package \"sf\" must be installed to manage geometries in shapefiles.")
  #library(sf)

  Year.n <- as.numeric(substr(year.patternA(Year),1,4)) + 1
  YearMinus1.n <- Year - 1
  while(is.null(input_shp)){
    input_shp <- Get_Shapefile(Year = ifelse(
      Year.n %in% c(2016, 2018), Year.n, YearMinus1.n), level = level, lightShp = TRUE,
      autoAbort = autoAbort)
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

  #args <- as.list(match.call())[-1]
  #args <- args[which(names(args) %in% names(formals(Set_DB)))]

  while(is.null(data)) {
    data <- Set_DB(Year = Year, level = level, autoAbort = autoAbort, ...)  #do.call(Set_DB, args)
    if(is.null(data)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during data retrieving. Would you abort the whole operation or retry?",
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

  while(! field %in% names(data)){
    message(paste("The variable", field, "does not seem to belong to the current database.
                  Please insert another one (do not use quotes in the prompt)"))
    field <- readline(prompt = "  > ")
  }

  #if(!any(grep(c("M_Italian|M_Mathematics|M_English_L|M_English_R|S_Italian|S_Mathematics|S_English_L|S_English_R"), names(data)))){#}

  if(length(unique(data$Order)) == 1L) {
    order <- unique(data$Order)[1L]
  }

  if(any(grep("_2$|_3$|_4$|_5$", field))){
    order <- "Primary"
  } else if(any(grep("_6$|_7$|_8$", field))){
    order <- "Middle"
  }else if(any(grep("_9$|_10$|_11$|_12$|_13$", field))) {
    order <- "High"
  }

  data <- data %>% dplyr::filter(.data$Order == order)
  if(level %in% c("LAU", "Municipality")){
    res <- input_shp %>% dplyr::select(.data$COD_REG, .data$PRO_COM_T) %>%
      rename_by_idx(c(1,2), into = c("Region_code", "Municipality_code")) %>%
      dplyr::left_join(data, by= "Municipality_code") %>%
      dplyr::filter(.data$Region_code %in% region_code)
  } else {
    res <- input_shp %>% dplyr::select(.data$COD_REG, .data$COD_PROV) %>%
      rename_by_idx(c(1,2), into = c("Region_code", "Province_code")) %>%
      dplyr::left_join(data,by = "Province_code") %>%
      dplyr::filter(.data$Region_code %in% region_code)
  }

  nfield <- which(names(res) == field)

  if(main == ""){
    layername <- paste0(field, ", year: ", Year, ", ", order, " schools")
  } else layername <- main


  if(plot == "ggplot"){

    if(col_rev == FALSE){
      fill.low = "#132B43"
      fill.high = "#56B1F7"
    } else {
      fill.low = "#56B1F7"
      fill.high = "#132B43"
    }

    #res.nospatial <- sf::st_drop_geometry(res)

    if(main_pos == "top"){
      ggplot2::ggplot() + ggplot2::geom_sf(data = res, ggplot2::aes(
        fill = !!rlang::sym(field))) +
        ggplot2::labs(fill = "") + ggplot2::ggtitle(layername)  +
        ggplot2::scale_fill_gradient(high = fill.high, low = fill.low)
    } else {
      ggplot2::ggplot() + ggplot2::geom_sf(data = res, ggplot2::aes(
        fill = !!rlang::sym(field))) +
        ggplot2::labs(fill = field)  +
        ggplot2::scale_fill_gradient(high = fill.high, low = fill.low)
    }

  } else {

    n <- length(unique(unlist(dplyr::select(dplyr::filter(
      data, .data$Province_code %in% res$Province_code), field)))) - 1

    if (col_rev == FALSE) {
      brew <- grDevices::hcl.colors(n, palette = pal)
    } else {
      brew <- rev(grDevices::hcl.colors(n, palette = pal))
    }
    pop = leafpop::popupTable(res)
    suppressWarnings( mapview::mapview(
      res, zcol = field, col.regions = brew,layer.name = layername,
      popup = paste0(set_popup_height(popup_height) , pop) ) )
  }
}
