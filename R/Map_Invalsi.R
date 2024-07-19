#' Display a map of Invalsi scores
#'
#' @description This function displays either a static or interactive map of the Invalsi scores, either at the municipality or province level.
#' It supports two kinds of map:
#' \itemize{
#'   \item Interactive map (default option), which allows the user to visualize all the data in scope through the interactive popup, and
#'   \item Static map (ggplot), which can be easily exported in \code{.pdf} objects.
#' }
#'
#'
#' @param Year Numeric or character value. Reference school year for the data (last available is 2022/23).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022}. \code{2022} by default
#' @param subj_toplot Character. The school subject to display in the map,
#'  The school subject to include, one among:
#'  \code{"Englis_listening"}/\code{"ELI"}, \code{"English_reading"}/\code{"ERE"}, \code{"Italian"}/\code{"ITA"} and \code{"Mathematics"}/\code{"MAT"}. \code{"ITA"} (Italian) by default.
#' @param grade Numeric. The school grade to chose. Either \code{2} (2nd year of primary school), \code{5} (last year of primary school), \code{8} (last year of middle shcool), \code{10} (2nd year of high school) or \code{13} (last year of school). \code{8} by default
#' @param level Character. The level of aggregation of Invalsi census data. Either \code{"NUTS-3"}, \code{"Province"}, \code{"LAU"}, \code{"Municipality"}.
#' If an input dataframe is provided, please select the same level of aggregation.
#' \code{"LAU"} by default
#' @param main Character. A customary title to the map. If \code{NULL}, the title will mention: subject, year and school grade. Empty by default.
#' @param main_pos Character.Where the header should be placed if the \code{ggplot} mode is chosen.
#' The header is located on the top if \code{"top"} is given as input, and above the legend scale otherwise. \code{"top"} by default.
#' @param region_code Numeric. The NUTS-2 codes of the units that must be displayed.
#' If the level is set to \code{"LAU"}, choosing a limited number of regions is recommended.
#' By default, \code{c(1,3,5:20)}, i.e. all Italian regions except the provinces of Aosta, Trento and Bozen which have data availability issues.
#' @param plot Character. The type of map to display; either \code{"mapview"} for interactive maps, or \code{"ggplot"} for static maps. \code{"mapview"} by default.
#' @param pal Character. The palette to use if the \code{"mapview"} mode is chose. \code{"Blues"} by default.
#' @param WLE Logical. Whether the variable to chose should be the average WLE score rather that the percentage of sufficient tests, if both are available. \code{FALSE} by default
#' @param col_rev Logical. Whether the scale of the colour palette should be reverted or not, if the \code{mapview} mode is chosen. \code{FALSE} by default
#' @param popup_height Numeric. The height of the popup table in terms of pixels if the \code{"mapview"} mode is chosen. \code{200} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The raw Invalsi survey data that has to be filtered, obtained as output of the \code{\link{Get_Invalsi_IS}} function.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_shp Object of class \code{sf}, \code{tbl_df}, \code{tbl}, \code{data.frame}. The relevant shapefiles of Italian administrative boudaries,
#' at the selected level of detail (LAU or NUTS-3). If \code{NULL}, it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#'
#' @return If \code{plot == "mapview"}, an object of class \code{mapview}. Otherwise, if \code{plot == "ggplot"}, an object of class \code{gg} and \code{ggplot}.
#'
#'
#' @examples
#'
#'
#'
#'
#'  Map_Invalsi(subj = "Italian", grade = 13, level = "NUTS-3", Year = 2023, WLE = FALSE,
#'   data = example_Invalsi23_prov, input_shp = example_Prov22_shp, plot = "ggplot")
#'
#'  Map_Invalsi(subj = "Italian", grade = 5, level = "NUTS-3", Year = 2023, WLE = TRUE,
#'   data = example_Invalsi23_prov, input_shp = example_Prov22_shp, plot = "ggplot")
#'
#'
#'
#'
#'
#' @export

Map_Invalsi <- function(Year = 2023, data = NULL, subj_toplot = "ITA", grade = 8, level = "LAU",
                        main = "", main_pos = "top", region_code = c(1:20), plot="mapview", pal = "Blues",
                        WLE = FALSE, col_rev = FALSE, popup_height = 200, verbose = TRUE,
                        input_shp = NULL, autoAbort = FALSE){

  if (length(subj_toplot) > 1){
    warning("Only one subject can be selected for mapping. The first one will be plotted")
  }

  region_code <- as.numeric(region_code)
  grade <- as.numeric(grade)

  if(length(grade)>1L){
    message("It is possible to plot only one school grade. Only the first one will be selected")
    grade <- grade[1L]
  }

  while(is.null(data)){
    data <- Get_Invalsi_IS(level = level, verbose = verbose, autoAbort = autoAbort)
    if(is.null(data)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during Invalsi data retrieving. Would you abort the whole operation or retry?",
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

  while(is.null(input_shp)){
    YearMinus1 <- as.numeric(year.patternA(Year))%/%100
    input_shp <- Get_Shapefile(Year =
      ifelse(year.patternA(Year)=="201819", Year, YearMinus1), level = level, autoAbort = autoAbort)
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

  dat <- Util_Invalsi_filter(data = data, Year = Year,
                             subj = c("ELI", "ERE", "ITA", "MAT"),
                             grade = grade, level = level, WLE=WLE,
                             verbose = verbose)

  if(toupper(subj_toplot) %in% c("ELI", "ENGLISH L", "ENGLISH_L", "INGLESE L", "INGLESE_L", "ENGLISH LISTENING", "ENGLISH_LISTENING")){
    fieldname <-  "M_English_L"
  } else if(toupper(subj_toplot) %in% c("ERE", "ENGLISH R", "ENGLISH_R", "INGLESE R", "INGLESE_R", "ENGLISH_READING", "ENGLISH READING" )){
    fieldname <- "M_English_R"
  } else if (toupper(subj_toplot) %in% c("ITA", "ITALIANO", "ITALIAN")){
    fieldname <- "M_Italian"
  } else if (toupper(subj_toplot) %in% c("MAT", "MATEMATICA", "MATHEMATICS")){
    fieldname <- "M_Mathematics"
  }

  if (level %in% c("Municipality", "LAU") ){
    shp <- input_shp %>% dplyr::select(.data$COD_REG, .data$COD_PROV, .data$PRO_COM_T) %>%
      rename_by_idx(c(1,2,3), into=c("Region_code", "Province_code", "Municipality_code"))
  } else {
    shp <- input_shp %>% dplyr::select(.data$COD_REG, .data$COD_PROV) %>%
      rename_by_idx(c(1,2), into = c("Region_code", "Province_code"))
  }
  res <- dplyr::left_join(shp, dat, by = names(shp)[ncol(shp)-1]) %>%
    #dplyr::select(-.data$geometry) %>%
    dplyr::filter(.data$Region_code %in% region_code)
  fieldnum <- grep(fieldname, names(res))

  names(res)[fieldnum] <- "X"

  if (plot =="mapview"){

    n <- length(unique(res$X)) - 1
    if( col_rev == FALSE){
      brew <- grDevices::hcl.colors(n, palette = pal)
    } else {
      brew <- rev(grDevices::hcl.colors(n, palette = pal))
    }


    names(res)[fieldnum] = fieldname
    pop = leafpop::popupTable(res)


    mapview::mapview(res, zcol=fieldname, col.regions = brew,
                     layer.name = ifelse(main == "", paste0(
                       fieldname, ", ", "Year: ", Year, ", ", "School grade: ", grade), main),
                     popup = paste0(set_popup_height(popup_height) , pop))

  } else {

    if (main_pos == "top"){
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = res, ggplot2::aes(fill = .data$X)) +
        ggplot2::labs(fill = "") + ggplot2::ggtitle(ifelse(main == "", paste0(
          fieldname, ", ", "Year: ", Year, ", ", "School grade: ", grade), main))
    } else {
      ggplot2::ggplot() + ggplot2::geom_sf(data = res, ggplot2::aes(fill = .data$X)) +
        ggplot2::labs(fill = ifelse(main == "",paste0(
          fieldname, ", ", "Year: ", Year, ", ", "School grade: ", grade), main))
    }
  }
}
