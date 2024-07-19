#' Aggregate the students number data by class at the municipality and province level
#'
#' @description  This function creates two dataframes with the number of students, classes and students by class, aggregated at the province and municipality level
#'
#'
#'
#'
#' @param data Either an object of class \code{list}, obtained as output of the \code{\link{Get_nstud}} function,
#' or an object of class class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the \code{\link{Util_nstud_wide}} function,
#' if \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param Year Numeric or character value. The reference school year, if either of the \code{input_} arguments must be retrieved.
#' Available in the formats: \code{2022}, \code{"2022/2023"}, \code{"202223"}, \code{"20222023"}. \code{2023} by default
#' @param check Logical. If \code{TRUE}, the function runs the test of the students number availability across all school included in the school registries (see \code{\link{Util_Check_nstud_availability}}). \code{TRUE} by default
#' @param check_ggplot Logical. If \code{check == TRUE}, whether to display or not a static map of the availability of the students number by province; see also \code{\link{Util_Check_nstud_availability}}. \code{TRUE} by default.
#' @param check_registry Character. If \code{check == TRUE}, the school registries included in the \code{input_School2mun} object (see \code{\link{Get_School2mun}}) whose availability has to be checked. Either \code{"Registry_from_buildings"} (buildings section), \code{"Registry_from_registry"} (registry section), \code{"Any"} or \code{"Both"}. \code{"Any"} by default.
#' @param InnerAreas Logical. If \code{check == TRUE}, Whether it must be checked if municipalities belong to Inner areas or not. \code{TRUE} by default.
#' @param ord_InnerAreas Logical. If \code{check == TRUE} and \code{InnerAreas == TRUE}, whether the Inner areas classification should be treated as an ordinal variable rather than as a categorical one (see \code{\link{Get_InnerAreas}} for the classification).
#' \code{FALSE} by default.
#' @param missing_to_1 Logical. Only needed if \code{data} is not provided in wide format. Whether the number of classes should be imputed to 1 when it is missing; see \code{\link{Util_nstud_wide}}. \code{FALSE} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param input_Registry Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_Registry}}
#' If \code{check == TRUE}, the school registry (the properly said one, from the registry section).
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_InnerAreas Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The classification of peripheral municipalities, obtained as output of the \code{\link{Get_InnerAreas}} function.
#' Needed only if \code{check == TRUE} and \code{InnerAreas == TRUE}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_Prov_shp Object of class \code{sf}, \code{tbl_df}, \code{tbl}, \code{data.frame}. The relevant shapefile of Italian municipalities, if both the \code{check} and \code{check_ggplot} options are chosen.
#' If \code{NULL} it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param input_School2mun Object of class \code{list} with elements of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_School2mun}}.
#' The mapping from school codes to municipality (and province) codes. Needed only if `check == TRUE`.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_AdmUnNames Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_AdmUnNames}}
#' The ISTAT file including all the codes and the names of the administrative units for the year in scope.
#' Only needed if \code{check == TRUE} and the argument \code{input_School2mun} is \code{NULL}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Additional arguments to the function \code{\link{Util_nstud_wide}} if \code{data} is not provided.
#'
#'
#'
#'
#' @return An object of class \code{list} including:
#'
#' \itemize{
#'   \item \code{$Municipality_data}:
#'    object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, the output dataframe detailed at the municipality level
#'
#'   \item \code{$Province_data}: object of class 'tbl_df', 'tbl' and 'data.frame', the output dataframe detailad at the province level
#'  }
#'
#' @details Numerical variables are summarised by the mean; Boolean variables are summarised by the mean as well, thus they become frequency indicators.
#' Qualitative values, if included, are summarised by the mode. Summary measures do not include NAs.
#'
#'
#' @examples
#
#' Year <- 2023
#'
#' nstud23_aggr <- Group_nstud(data = example_input_nstud23, Year = Year,
#'                            input_Registry = example_input_Registry23,
#'                            InnerAreas = FALSE,  input_School2mun = example_School2mun23)
#'
#' summary(nstud23_aggr$Municipality_data[,c(46,47,48)])
#'
#' summary(nstud23_aggr$Province_data[,c(44,45,46)])
#'
#'
#' @export


Group_nstud <- function(data = NULL, Year = 2023,
                        check = TRUE, verbose = TRUE, check_registry = "Any",
                        InnerAreas = TRUE,
                        ord_InnerAreas = FALSE, check_ggplot = FALSE,
                        missing_to_1 = FALSE, input_Registry = NULL,
                        input_InnerAreas = NULL, input_Prov_shp = NULL,
                        input_School2mun = NULL, input_AdmUnNames = NULL,
                        autoAbort = FALSE, ...) {

  options(dplyr.summarise.inform = FALSE)
  . <- NULL

  start.zero <- Sys.time()

  while(is.null(data)){
    data <- Get_nstud(Year = Year, verbose = verbose, autoAbort = autoAbort)
    if(is.null(data)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during students counts retrieving. Would you abort the whole operation or retry?",
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

  if(is.data.frame(data)){
    if(any(grepl("per.*class", names(data), ignore.case = TRUE))){
      nstud.byclass <- data
    } else {
      nstud.byclass <- Util_nstud_wide(data = data, missing_to_1 = missing_to_1, ... )
    }
  } else nstud.byclass <- Util_nstud_wide(data = data, missing_to_1 = missing_to_1, ... )


  if(verbose) cat("Linking schools to reference municipalities \n")
  while(is.null(input_School2mun)){
    input_School2mun <- Get_School2mun(
      Year = Year,verbose = verbose,
      input_Registry = input_Registry, input_AdmUnNames = input_AdmUnNames,
      autoAbort = autoAbort)
    if(is.null(input_School2mun)){
      if(!autoAbort){
        holdOn <- ""
        message("Error occurred in mapping school to municipalities. Would you abort the whole operation or retry?",
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
  if(!is.data.frame(input_School2mun)) School2mun.R <- input_School2mun[[check_registry]]

  if(verbose) cat("Aggregating schools data \n")

  suppressWarnings(
    nstud.byclass_Mun <- dplyr::left_join(
      nstud.byclass, School2mun.R ,by = "School_code") %>%
      dplyr::filter(!is.na(.data$Municipality_code)) %>% dplyr::relocate(
        c(.data$Municipality_code, .data$Municipality_description,
          .data$Province_code, .data$Province_initials), .after ="School_code") %>%
      dplyr::select(-grep("Students_per_class", names(.))) %>% Group_Count(
        groupcol = c("Municipality_code", "Municipality_description",
                     "Province_code", "Province_initials", "Order"),
        startgroup = 7, FUN = sum, countname = "nschools")
  )

  suppressWarnings(
    nstud.byclass_Prov <- dplyr::left_join(
      nstud.byclass, School2mun.R, by = "School_code") %>%
      dplyr::filter(!is.na(.data$Municipality_code)) %>% dplyr::relocate(
        c(.data$Municipality_code, .data$Municipality_description,
          .data$Province_code, .data$Province_initials), .after ="School_code") %>%
      dplyr::select(-grep("Students_per_class", names(.))) %>% Group_Count(
        groupcol = c("Province_code", "Province_initials", "Order"),
        startgroup = 7, FUN = sum, countname = "nschools")
  )


  for (i in (1:14)){
    j <- 2*i + 5
    nstud.byclass_Mun <- nstud.byclass_Mun %>%
      dplyr::mutate(xx = as.numeric(unlist(dplyr::select(.,j)/dplyr::select(.,j+1) ) ) )
    names(nstud.byclass_Mun)[ncol(nstud.byclass_Mun)] <- paste("Students_per_class_", ifelse(i<14, i,"Tot"), sep = "")
  }
  nstud.byclass_Mun[, c(6:ncol(nstud.byclass_Mun))][is.na(nstud.byclass_Mun[ ,c(6:ncol(nstud.byclass_Mun))])] <- 0

  for (i in c(1:13)){
    j <- i + 34
    k <- 3*i + 5
    nstud.byclass_Mun <- nstud.byclass_Mun %>% dplyr::relocate(j, .after = k)
  }

  for (i in (1:14)){
    j <- 2*i + 3
    nstud.byclass_Prov <- nstud.byclass_Prov %>%
      dplyr::mutate(xx = as.numeric(unlist(dplyr::select(.,j)/dplyr::select(.,j+1) ) ) )
    names(nstud.byclass_Prov)[ncol(nstud.byclass_Prov)] <- paste("Students_per_class_", ifelse(i<14, i,"Tot"), sep = "")
  }
  nstud.byclass_Prov[, c(4:ncol(nstud.byclass_Prov))][is.na(nstud.byclass_Prov[ ,c(4:ncol(nstud.byclass_Prov))])] <- 0

  for (i in c(1:13)){
    j <- i + 32
    k <- 3*i + 3
    nstud.byclass_Prov <- nstud.byclass_Prov %>%
      dplyr::relocate(j, .after = k)
  }

  nstud.check <- NULL
  if(check){
    if(verbose) cat("Checking whether schools are included in school registries \n")
    nstud.check <-
        Util_Check_nstud_availability(nstud.byclass, Year = Year,cutout = c("IC", "IS", "NR"),
                                      ggplot = check_ggplot,
                                      verbose = verbose, InnerAreas = InnerAreas,
                                      ord_InnerAreas = ord_InnerAreas,
                                      input_Registry = input_Registry, input_AdmUnNames = NULL,
                                      input_InnerAreas = input_InnerAreas,
                                      input_Prov_shp = input_Prov_shp, input_School2mun = input_School2mun,
                                      autoAbort = autoAbort)
  }
  if(is.null(nstud.check)){
    message("Error occurred during the students count availability check")
  } else {
    check_mun <- nstud.check$Municipality_data[[check_registry]]
    if(InnerAreas){
      if(ord_InnerAreas){
        check_mun <- check_mun %>% dplyr::select(.data$Order, .data$Municipality_code, .data$Availability, .data$Inner_area,
                                                 .data$A_mun, .data$B_mun, .data$C_mun, .data$D_mun, .data$E_mun, .data$F_mun)
      } else {
        check_mun <- check_mun %>% dplyr::select(.data$Order, .data$Municipality_code, .data$Availability, .data$Inner_area)
      }
    } else {
      check_mun <- check_mun %>% dplyr::select(.data$Order, .data$Municipality_code, .data$Availability)
    }
    check_mun <- check_mun %>%
      tidyr::unite("ID", c(.data$Municipality_code, .data$Order), sep = "___") %>%
      dplyr::mutate(ID = gsub(" ", "_", .data$ID))

    nstud.byclass_Mun <- nstud.byclass_Mun %>%
      tidyr::unite("ID", c(.data$Municipality_code, .data$Order), sep = "___") %>%
      dplyr::mutate(ID = gsub(" ", "_", .data$ID)) %>%
      dplyr::left_join(check_mun, by = "ID") %>%
      tidyr::separate(.data$ID, into = c("Municipality_code", "Order"), sep = "___") %>%
      dplyr::mutate(Order = gsub("_", " ", .data$Order))

    check_prov <- nstud.check$Province_data[[check_registry]]
    if(InnerAreas){
      if(ord_InnerAreas){
        check_prov <- check_prov %>%
          dplyr::select(.data$Order, .data$Province_code, .data$Availability, .data$Inner_area,
                        .data$A_mun, .data$B_mun, .data$C_mun, .data$D_mun, .data$E_mun, .data$F_mun)
      } else {
        check_prov <- check_prov %>%
          dplyr::select(.data$Order, .data$Province_code, .data$Availability, .data$Inner_area)
      }
    } else {
      check_prov <- check_prov %>%
        dplyr::select(.data$Order, .data$Province_code, .data$Availability)
    }
    check_prov  <- check_prov %>%
      tidyr::unite("ID", c(.data$Province_code, .data$Order), sep = "___") %>%
      dplyr::mutate(ID = gsub(" ", "_", .data$ID))

    nstud.byclass_Prov <- nstud.byclass_Prov %>%
      tidyr::unite("ID", c(.data$Province_code, .data$Order), sep = "___") %>%
      dplyr::mutate(ID = gsub(" ", "_", .data$ID)) %>%
      dplyr::left_join(check_prov, by = "ID") %>%
      tidyr::separate(.data$ID, into = c("Province_code", "Order"), sep = "___") %>%
      dplyr::mutate(Order = gsub("_", " ", .data$Order)) %>%
      dplyr::mutate(Province_code = as.numeric(.data$Province_code))
  }

  endtime <- Sys.time()

  if(verbose){
    cat(paste("Total time needed to aggregate students number data",
              ifelse(check, "and check for data availability in school registries", ""),
              ifelse(InnerAreas, "and for schools belonging to inner areas", ""),
              ":"), difftime(endtime, start.zero, units="secs"), "seconds \n"  )
  }

  return(list(Municipality_data = nstud.byclass_Mun, Province_data = nstud.byclass_Prov))
}
