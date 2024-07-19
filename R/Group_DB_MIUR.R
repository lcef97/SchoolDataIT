#' Aggregate the database of Italian public schools buildings at the municipality and province level
#'
#' @description This function transforms the output of the \code{\link{Util_DB_MIUR_num}} function (which is detailed at the level of single school buildings) at the municipality/LAU and province/NUTS-3 level.
#' It also allows the user to classify the grade of centrality of municipalities through the variable  \code{Inner_area}.
#'
#'
#'
#'
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. The database of school buildings, preferably already converted to numeric, obtained via \code{\link{Util_DB_MIUR_num}}
#' @param Year Numeric or Character. The reference school year, if either \code{data} or \code{input_InnerAreas} must be retrieved.
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}.
#' Important: use the same \code{Year} argument used to retrieve the input school buildings data if they are provided as input. \code{2023} by default
#' @param count_units Logical. Whether the rows to aggregate at each level must be counted or not. True by default.
#' @param countname character. The name of the variable indicating the number of schools included in each municipality of province,
#' if the argument 'count' is \code{TRUE}. \code{"nbuildings"}  by default.
#' @param count_missing Logical. Whether the function should return two dataframes including the percentage of NAs in the \code{data} object at the territorial level. \code{TRUE} by default
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param track_deleted Logical. If \code{TRUE}, the function returns the IDs of schools not included. \code{TRUE} by default.
#' @param InnerAreas Logical. Whether an indicator of the percentage of schools belonging to peripheral (Inner) areas mus be included or not.
#' @param ord_InnerAreas Logical. Whether the Inner areas classification should be treated as an ordinal variable rather than as a binary one (see \code{\link{Get_InnerAreas}} for the classification).
#' Please notice than the function creates a column for each class, and if this database must be used in a statistical model, one of the 6 resulting columns must be dropped.
#' False by default.
#' @param input_InnerAreas Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The classification of peripheral municipalities, needed only if \code{InnerAreas == TRUE}, obtained as output of the \code{\link{Get_InnerAreas}} function.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Additional arguments to the function \code{Util_DB_MIUR_num} in case no data are provided or data.
#'
#'
#' @return An object of class \code{list} including:
#'
#' \itemize{
#'   \item \code{$Municipality_data}:
#'    object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, the output dataframe detailed at the municipality level;
#'    all variables besides the first 5 (which identify the record) are numeric
#'
#'   \item \code{$Province_data}: object of class 'tbl_df', 'tbl' and 'data.frame', the output dataframe detailad at the province level;
#'   all variables besides the first 3 (which identify the record) are numeric
#'
#'   \item \code{$Municipality_missing} (Only if \code{count_missing == TRUE}); object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, the percentage of NAs in each variable at the municipality level.
#'
#'   \item \code{$Province_missing}: (Only if \code{count_missing == TRUE}); object of class 'tbl_df', 'tbl' and 'data.frame', the percentage of NAs in each variable at the province level.

#'   \item \code{$deleted}: character vector. The schools removed from the original dataframe for data quality reasons. This object is returned only if \code{track_deleted == TRUE}
#'  }
#'
#'
#' @details Numerical variables are summarised by the mean; Boolean variables are summarised by the mean as well, thus they become frequency indicators.
#' Qualitative values, if included, are summarised by the mode. Summary measures do not include NAs.
#' The output dataframes are also detailed at the school order level (i.e. Primary, Midde, High school, or different orders). This means that rows are unique combinations of territorial unities and school order.
#'
#'
#'
#' @examples
#'
#'
#' library(magrittr)
#' DB23_MIUR <- example_input_DB23_MIUR %>% Util_DB_MIUR_num(verbose = FALSE) %>%
#'     Group_DB_MIUR(InnerAreas = FALSE)
#'
#'
#'
#' DB23_MIUR$Municipality_data[, -c(1,2,4)]
#' summary(DB23_MIUR$Municipality_data)
#'
#' DB23_MIUR$Province_data[, -c(1,3)]
#' summary(DB23_MIUR$Province_data)
#'
#'
#'
#'
#'
#' @export


Group_DB_MIUR <- function(data = NULL, Year = 2023,
                          count_units = TRUE, countname="nbuildings",
                          count_missing = TRUE, verbose = TRUE,
                          track_deleted = TRUE, InnerAreas = TRUE, ord_InnerAreas = FALSE,
                          input_InnerAreas = NULL, autoAbort = FALSE, ...){

  options(dplyr.summarise.inform = FALSE)

  # No input provided: data are downloaded
  while(is.null(data)){
    data <- Get_DB_MIUR(Year = Year, verbose = verbose, autoAbort = autoAbort) %>%
      Util_DB_MIUR_num(track_deleted = track_deleted, ...)
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

  # To evaluate if data are the raw dataset (if data are the numeric dataset the fields list becomes trivial)
  fields <- lapply(data, is.numeric)

  if(is.data.frame(data) & ! any(unlist(fields[-which(names(fields) %in% c(
    "Province_code", "Year", "Floors_number", "National_seismic_classification"))]))){
    data <- data %>% Util_DB_MIUR_num(track_deleted = track_deleted, ...)
  }

  if(is.data.frame(data)){
    DB <- data
    track_deleted <- FALSE
  } else{
    DB <- data$data
    deleted <- data$deleted
  }

  if("Postal_code" %in% names(DB)) {
    DB <- DB %>% dplyr::select(-.data$Postal_code)
  }

  if(InnerAreas){
    if(is.null(input_InnerAreas)) {
      if(verbose) cat("Retrieving Inner areas classification: \n")
      input_InnerAreas <- Get_InnerAreas(autoAbort)
      if(is.null(input_InnerAreas)){
        message("Failed to import inner areas classification")
      }
    }
    if(!is.null(input_InnerAreas)){
      if(dplyr::between(as.numeric(substr(year.patternA(Year),1,4))+1, 2021, 2027)){
        InnerAreas.R <- input_InnerAreas %>%
          dplyr::select(.data$Municipality_code, .data$Inner_area_code_2021_2027) %>%
          dplyr::mutate(Inner_area = ifelse(
            .data$Inner_area_code_2021_2027 %in% c("A", "B", "C"), 0, 1)) %>%
          dplyr::rename(Inner_area_code = .data$Inner_area_code_2021_2027)
      } else {
        InnerAreas.R <- input_InnerAreas %>%
          dplyr::select(.data$Municipality_code, .data$Inner_area_code_2014_2020) %>%
          dplyr::mutate(Inner_area = ifelse(
            .data$Inner_area_code_2014_2020 %in% c("A", "B", "C"), 0, 1)) %>%
          dplyr::rename(Inner_area_code = .data$Inner_area_code_2014_2020)
      }

      if(ord_InnerAreas){
        InnerAreas.R <- InnerAreas.R %>% dplyr::mutate(
          A_mun = as.numeric(.data$Inner_area_code == "A"),
          B_mun = as.numeric(.data$Inner_area_code == "B"),
          C_mun = as.numeric(.data$Inner_area_code == "C"),
          D_mun = as.numeric(.data$Inner_area_code == "D"),
          E_mun = as.numeric(.data$Inner_area_code == "E"),
          F_mun = as.numeric(.data$Inner_area_code == "F") )
      }
      InnerAreas.R <-
        InnerAreas.R %>% dplyr::select(-.data$Inner_area_code)

      DB <- DB %>% dplyr::left_join(InnerAreas.R, by = "Municipality_code")

    }
   }

  groupcol.mun = c("Year", "Municipality_code", "Municipality_description", "Province_code", "Province_initials", "Order" )
  groupcol.prov = c("Year", "Province_code", "Province_initials", "Order" )

  Municipality_data <- DB %>%
    Group_Count(groupcol = groupcol.mun,  startgroup = 10,
                count = count_units, countname = countname, FUN = MeanOrMode)

  Province_data <- DB %>%
    Group_Count(groupcol = groupcol.prov, startgroup = 10,
                count = count_units, countname = countname, FUN = MeanOrMode)

  Municipality_missing <- DB %>%
    Group_Count(groupcol = c("Municipality_code", "Order"),  startgroup = 10,
                count = count_units, countname = countname, FUN = function(x){sum(is.na(x))/length(x)})
  names(Municipality_missing)[-c(1,2)] <-
    paste0(names(Municipality_missing)[-c(1,2)], "_MP")

  Province_missing <- DB %>%
    Group_Count(groupcol = c("Province_code", "Order"), startgroup = 10,
                count = count_units, countname = countname, FUN = function(x){sum(is.na(x))/length(x)})
  names(Province_missing)[-c(1,2)] <-
    paste0(names(Province_missing)[-c(1,2)], "_MP")

  res <- list(Municipality_data = Municipality_data, Province_data = Province_data)

  if(track_deleted) res$deleted <- deleted
  if(count_missing){
    res$Municipality_missing <- Municipality_missing
    res$Province_missing <- Province_missing
  }

  return(res)
}



