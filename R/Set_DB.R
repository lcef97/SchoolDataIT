#' Build up a comprehensive database regarding the school system
#'
#' @description This function generates a unique dataframe of the school system data including a customary choice of available datasets. This function allows the user to aggregate the desired datasets, when available, among these:
#' \itemize{
#'   \item Invalsi census survey
#'   \item School buildings
#'   \item Number of students and school classes
#'   \item Number of teachers
#'   \item Broadband connection availability
#' }
#'
#'
#' To save as much time as possible it is possible to plug in ready-made input data; otherwise they will be downloaded automatically but not saved in the global environment
#' When a new dataset is joined to the existing ones, it is possible that some observations in this datasets are missing. In this case, by default, the choice of keeping as much observational units as possible, or to remove units with missing variables is left to the user.
#'
#'
#' @param Year Numeric or Character. The relevant school year. Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}.
#' Important: if input datasets are plugged in, please select the same \code{Year} argument used to download the input data. \code{2023} by default.
#' @param level Character. The administrative level of detail at which data must be aggregated.
#' Either \code{"LAU"}/\code{"Municipality"} or \code{"NUTS-3"}/\code{"Province"}. \code{"LAU"} by default.
#' @param conservative Logical. If \code{FALSE}, only the schools included in all the datasets are taken as input. \code{TRUE} by default.
#' @param Invalsi Logical. Whether the Invalsi census data must be included (see \code{\link{Get_Invalsi_IS}}. \code{TRUE} by default.
#' @param SchoolBuildings Logical. Whether the school buildings dataset must be included (see \code{link{Get_DB_MIUR}}, \code{\link{Util_DB_MIUR_num}}. \code{TRUE} by default.
#' @param nstud Logical. Whether the students number per class must be included (see \code{\link{Get_nstud}}. \code{TRUE} by default.
#' @param nteachers Logical. Whether the number of teachers by province must be included (see \code{link{Get_nteachers_prov}}). \code{TRUE} by default.
#' @param BroadBand Logical. Whether the broadband availability in schools must be included (see \code{\link{Get_BroadBand}}). \code{TRUE} by default
#' @param InnerAreas Logical. Whether the percentage of schools belonging to inner/internal areas must be included (see \code{\link{Get_InnerAreas}}). TRUE by default.
#' @param ord_InnerAreas Logical. If \code{check == TRUE} and \code{InnerAreas == TRUE}, whether the Inner areas classification should be treated as an ordinal variable rather than as a categorical one (see \code{\link{Get_InnerAreas}} for the classification). \code{FALSE} by default.
#' @param Invalsi_subj Character. If \code{Invalsi == TRUE}, the school subject(s) to include, among \code{"Englis_listening"}/\code{"ELI"}, \code{"English_reading"}/\code{"ERE"}, \code{"Italian"}/\code{"Ita"} and \code{"Mathematics"}/\code{"MAT"}. All four by default.
#' @param Invalsi_grade Numeric. If \code{Invalsi == TRUE}, the educational grade to choose. Either \code{2} (2nd year of primary school), \code{5} (last year of primary school), \code{8} (last year of middle shcool), \code{10} (2nd year of high school) or \code{13} (last year of school). All by default.
#' @param Invalsi_WLE Logical. Whether to express Invalsi scores as averagev WLE score rather that the percentage of sufficient tests, if both are Invalsi_grade is either or \code{2} \code{5}. \code{FALSE} by default
#' @param SchoolBuildings_include_numerics Logical. Whether to include strictly numeric variables alongside with Boolean ones in the school buildings database (see \code{\link{Util_DB_MIUR_num}}). \code{TRUE} by default.
#' @param SchoolBuildings_include_qualitatives Logical. Whether to include qualitative variables alongside with Boolean ones in the school buildings database (see \code{\link{Util_DB_MIUR_num}}). \code{FALSE} by default.
#' @param SchoolBuildings_row_cutout Logical. Whether to filter out rows including missing fields in the school buildings database (see \code{\link{Util_DB_MIUR_num}}). \code{FALSE} by default.
#' @param SchoolBuildings_col_cut_thresh Numeric. The threshold of missing values allowed for each variable in the school buildings database (see \code{\link{Util_DB_MIUR_num}}).
#' If a variable as a higher number of missing observations, then it is cut out. \code{20.000} by default.
#' Warning: if the option \code{SchoolBuildings_row_cutout} is active, please select a lower threshold (e.g. \code{1000})
#' @param SchoolBuildings_flag_outliers Logical. Whether to assign NA to outliers in numeric variables; see \code{\link{Util_DB_MIUR_num}} for more details. \code{TRUE} by default.
#' @param SchoolBuildings_count_missing Logical. Whether the function should return the percentage of NAs in the input school buildings database (see also \code{\link{Group_DB_MIUR}}). \code{FALSE} by default.
#' @param nstud_missing_to_1 Numeric. If \code{nstud == TRUE}, whether the number of classes should be imputed to 1 when it is missing and the number of students is below a threshold (argument \code{nstud_imputation_thresh}, see \code{\link{Util_nstud_wide}}). \code{FALSE} by default.
#' @param nstud_imputation_thresh Numeric. If \code{nstud_missing_to_1 == TRUE}, the minimum threshold below which the number of classes is imputed to 1 if missing;
#' see also \code{\link{Util_nstud_wide}}. \code{19} by default.
#' @param UB_nstud_byclass Numeric. The upper limit of the acceptable school-level average of the number of students by class if \code{nstud == TRUE}; see also \code{\link{Util_nstud_wide}}.  \code{99} by default, i.e. no restriction is made. Please notice that boundaries are included in the acceptance interval.
#' @param LB_nstud_byclass Numeric. The lower limit of the acceptable school-level average of the number of students by class if \code{nstud == TRUE}; see also \code{\link{Util_nstud_wide}}. \code{1} by default. Please notice that boundaries are included in the acceptance interval.
#' @param nstud_check Logical. If \code{nstud == TRUE}, whether to check the students number availability across all school included in the school registries (see \code{\link{Util_Check_nstud_availability}}). \code{TRUE} by default.
#' @param nstud_check_registry Character. If \code{nstud == TRUE} and \code{nstud_check == TRUE}, the school registries whose availability has to be checked. Either \code{"Registry_from_buildings"} (buildings registry), \code{"Registry_from_registry"} (proper registry), \code{"Any"} or \code{"Both"}. \code{"Any"} by default.
#' @param BroadBand_impute_missing Whether the schools not included in the Broadband dataset must be considered in the total of schools (i.e. the denominator to the Broadband availability indicator). \code{TRUE} by default.
#' @param Date Character or Date. The threshold date to broadband activation to consider it activated for a school, i.e. the date before which the works of broadband activation must be finished in order to consider a school as provided with the broadband. By default, September 1st at the beginning of the school year.
#' @param NA_autoRM Logical. Either \code{TRUE}, \code{FALSE} or \code{NULL}. If \code{TRUE}, the values missing in a single dataset are automatically deleted from the final DB. If \code{FALSE}, the missing observations are kept automatically. If \code{NULL}, the choice is left to the user by an interactive menu. \code{NULL} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the \code{verbose} argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param input_Invalsi_IS Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' If \code{INVALSI == TRUE}, the raw Invalsi survey data, obtained as output of the \code{\link{Get_Invalsi_IS}} function.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_Registry Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The school registry corresponding to the year in scope, obtained as output of the function \code{\link{Get_Registry}}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_SchoolBuildings Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. If \code{SchoolBuildings == TRUE}, the raw school buildings dataset obtained as output of the function \code{\link{Get_DB_MIUR}}.
#' If \code{NULL}, it will be downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param input_nstud Object of class \code{list}, including two objects of class\code{tbl_df}, \code{tbl} and \code{data.frame}.
#' If \code{nstud == TRUE}, the students and classes counts, obtained as output of the function \code{\link{Get_nstud}} with default \code{filename} parameter.
#' If \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param input_School2mun Object of class \code{list} with elements of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#' If \code{nstud == TRUE}, the mapping from school codes to municipality (and province) codes. Needed only if \code{check == TRUE}, obtained as output of the function \code{\link{Get_School2mun}}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_AdmUnNames Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_AdmUnNames}}
#' If necessary,the ISTAT file including all the codes and the names of the administrative units for the year in scope. Required either if \code{nstud == TRUE & nstud_check == TRUE}, or if \code{SchoolBuildings == TRUE}, \code{input_DB_MIUR} is not provided, and the school year is one of 2015/16, 2017/18 or 1018/19
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_InnerAreas Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' If \code{InnerAreas == TRUE}, the classification of peripheral municipalities, obtained as output of the function \code{\link{Get_InnerAreas}}
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_nteachers Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. If \code{nteachers == TRUE}, the number of teachers by province, obtained as output of the function \code{\link{Get_nteachers_prov}}. If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_teachers4student Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. If \code{nteachers == TRUE} and \code{nstud = TRUE}, the number of teachers for studets by province. Please notice that
#' this object cannot be considered a substitute for the number of students by class since it provides no information on the number of schools in single educational grades but only at the school order level.
#' Obtained as output of the function \code{\link{Group_teachers4stud}}.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default.
#' @param input_BroadBand Object of classs \code{tbl_df}, \code{tbl} and \code{data.frame}. If BroadBand == TRUE, the raw Broadband connection dataset obtaned as output of the function \code{\link{Get_BroadBand}}
#' If \code{NULL}, it will be downloaded automatically but not saved in the global environment. \code{NULL} by default.
#'
#' @seealso \code{\link{Util_DB_MIUR_num}}, \code{\link{Group_DB_MIUR}}, \code{\link{Group_nstud}}, \code{\link{Util_Check_nstud_availability}}, \code{\link{Get_School2mun}}
#' for similar arguments.
#'
#' @return An object of  class \code{tbl_df}, \code{tbl} and \code{data.frame}
#'
#'
#'
#'
#' @examples
#'
#'
#'
#' DB23_prov <- Set_DB(Year = 2023, level = "NUTS-3",Invalsi_grade = c(5, 8, 13),
#'       Invalsi_subj = "Italian",nteachers = FALSE, BroadBand = FALSE,
#'       SchoolBuildings_count_missing = FALSE,NA_autoRM= TRUE,
#'       input_SchoolBuildings = example_input_DB23_MIUR[, -c(11:18, 10:27)],
#'       input_Invalsi_IS = example_Invalsi23_prov,
#'       input_nstud = example_input_nstud23,
#'       input_InnerAreas = example_InnerAreas,
#'       input_School2mun = example_School2mun23,
#'       input_AdmUnNames = example_AdmUnNames20220630)
#'
#'
#' DB23_prov
#'
#' summary(DB23_prov[, -c(22:62)])
#'
#'
#'
#'
#'
#' @export
Set_DB <- function( Year = 2023,
                    level = "LAU",
                    conservative = TRUE,
                    Invalsi = TRUE,
                    SchoolBuildings = TRUE,
                    nstud = TRUE,
                    nteachers = TRUE,
                    BroadBand = TRUE,
                    verbose = TRUE,
                    show_col_types = FALSE,
                    Invalsi_subj = c("ELI", "ERE", "ITA", "MAT"),
                    Invalsi_grade = c(2,5,8,10,13),
                    Invalsi_WLE = FALSE,
                    SchoolBuildings_include_numerics = TRUE,
                    SchoolBuildings_include_qualitatives = FALSE,
                    SchoolBuildings_row_cutout = FALSE,
                    SchoolBuildings_col_cut_thresh = 2e+4,
                    SchoolBuildings_flag_outliers = TRUE,
                    SchoolBuildings_count_missing = FALSE,
                    nstud_imputation_thresh = 19,
                    nstud_missing_to_1 = FALSE,
                    UB_nstud_byclass = 99,
                    LB_nstud_byclass = 1,
                    InnerAreas = TRUE,
                    ord_InnerAreas = FALSE,
                    nstud_check = TRUE,
                    nstud_check_registry = "Any",
                    BroadBand_impute_missing = TRUE,
                    Date = as.Date(paste0(substr(year.patternA(Year),1,4), "-09-01")),
                    NA_autoRM = NULL,
                    input_Invalsi_IS = NULL,
                    input_Registry = NULL,
                    input_SchoolBuildings = NULL,
                    input_nstud = NULL,
                    input_School2mun = NULL,
                    input_AdmUnNames = NULL,
                    input_InnerAreas = NULL,
                    input_teachers4student = NULL,
                    input_nteachers = NULL,
                    input_BroadBand = NULL,
                    autoAbort = FALSE){

  start.zero <- Sys.time()
  YearMinus1 <- as.numeric(substr(year.patternA(Year),1,4))

  datasets <- list()

  if(BroadBand || is.null(input_School2mun) ||
     (SchoolBuildings && is.null(input_SchoolBuildings)) ||
     (nstud && is.null(input_nstud))){
    while(is.null(input_Registry)){
      input_Registry <- Get_Registry(Year = Year, show_col_types = show_col_types, autoAbort = autoAbort)
      if(is.null(input_Registry)){
        if(!autoAbort){
          holdOn <- ""
          message("Error during schools registry retrieving. Would you abort the whole operation or retry? \n",
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
  }

  if(SchoolBuildings || nstud || BroadBand){
    while(is.null(input_AdmUnNames)){
      input_AdmUnNames <- Get_AdmUnNames(
        Year = ifelse(any(year.patternA(Year) %in% c(
          year.patternA(2016), year.patternA(2018))), Year, YearMinus1),
        date = ifelse(any(year.patternA(Year) %in%c(
          year.patternA(2016), year.patternA(2018))), "01_01", "06_30"), autoAbort = autoAbort)
      if(is.null(input_AdmUnNames)){
        if(!autoAbort){
          holdOn <- ""
          message("Error during administrative codes retrieving. Would you abort the whole operation or retry? \n",
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
  }

  while(is.null(input_School2mun)){
    input_School2mun <- Get_School2mun(
      Year = Year, verbose = verbose, show_col_types = show_col_types,
      input_Registry = input_Registry, input_AdmUnNames = input_AdmUnNames,
      autoAbort = autoAbort)
    if(is.null(input_School2mun)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during mapping schools to municipalities. Would you abort the whole operation or retry? \n",
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

  while(Invalsi && is.null(input_Invalsi_IS)){
    input_Invalsi_IS <- Get_Invalsi_IS(level = level, verbose = verbose,
                                       show_col_types = show_col_types, autoAbort = autoAbort)
    if(is.null(input_Invalsi_IS)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during Invalsi data retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          Invalsi <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else Invalsi <- FALSE
    }
  }

  while(SchoolBuildings && is.null(input_SchoolBuildings)){
    input_SchoolBuildings <-
      Get_DB_MIUR(Year = Year, verbose = verbose, show_col_types = show_col_types,
                  input_Registry = input_Registry, input_AdmUnNames = input_AdmUnNames,
                  autoAbort = autoAbort)

    if(is.null(input_SchoolBuildings)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during school buildings DB retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          SchoolBuildings <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else SchoolBuildings <- FALSE
    }
    if(verbose) cat("\n")
  }

  while(InnerAreas && is.null(input_InnerAreas)){
    input_InnerAreas <- Get_InnerAreas(autoAbort)
    if(is.null(input_InnerAreas)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during inner areas classification retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the element \n")
          InnerAreas <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else InnerAreas <- FALSE
    }
  }

  while(nstud && is.null(input_nstud)){
    if(nstud_missing_to_1){
      nstud_filename <- c("ALUCORSOETASTA", "ALUCORSOINDCLASTA")
    } else {
      nstud_filename <- "ALUCORSOINDCLASTA"
    }
    input_nstud <- Get_nstud(Year = Year, verbose = verbose, filename = nstud_filename, autoAbort = autoAbort)
    if(is.null(input_nstud)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during students counts retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          nstud <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else nstud <- FALSE
    }
  }

  while(nteachers && is.null(input_nteachers)){
    input_nteachers <- Get_nteachers_prov(Year = Year, verbose = verbose,
                                          show_col_types = show_col_types, autoAbort = autoAbort)
    if(is.null(input_nteachers)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during teachers counts retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the element \n")
          nteachers <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else nteachers <- FALSE
    }
  }

  while(BroadBand && is.null(input_BroadBand)){
    input_BroadBand <- Get_BroadBand(verbose = verbose, Date = Date, autoAbort = autoAbort)
    if(is.null(input_BroadBand)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during broadband data retrieving. Would you abort this element or retry? \n",
                "    - To abort the element, press `A` \n",
                "    - To retry data retrieving, press any other key \n")
        holdOn <- readline(prompt = "    ")
        if(toupper(holdOn) == "A"){
          cat("You chose to abort the operation \n")
          BroadBand <- FALSE
        } else {
          cat("You chose to retry \n")
        }
      } else BroadBand <- FALSE
    }
  }

  if(verbose) cat("\n")

  if(! conservative){
    if(!is.null(input_SchoolBuildings)){
      if(!is.null(input_nstud)){
        input_nstud_byclass <- input_nstud$ALUCORSOINDCLASTA
        input_nstud_byclass <- filterCommonRows(input_nstud_byclass, input_SchoolBuildings, verbose)
        input_SchoolBuildings <- filterCommonRows(input_SchoolBuildings, input_nstud_byclass, verbose)
      }
      if(!is.null(input_BroadBand)){
        input_BroadBand <- input_BroadBand %>%
          dplyr::filter(!grepl("[^A-Z]", substr(.data$School_code,1,4)) &
                        !grepl("X", substr(.data$School_code,1,4), ignore.case = TRUE)) %>%
          filterCommonRows(input_SchoolBuildings, verbose)
      }
    } else if(!is.null(input_nstud) && !is.null(input_BroadBand)){
      input_BroadBand <- input_BroadBand %>%
        dplyr::filter(!grepl("[^A-Z]", substr(.data$School_code,1,4)) &
                      !grepl("X", substr(.data$School_code,1,4), ignore.case = TRUE)) %>%
        filterCommonRows(input_nstud_byclass, verbose)
    }
    if(!is.null(input_nstud)) input_nstud$ALUCORSOINDCLASTA <- input_nstud_byclass
    if(verbose) cat("\n")
  } else {
    if(!is.null(input_SchoolBuildings)){
      input_SchoolBuildings <- input_SchoolBuildings %>%
        dplyr::filter(.data$School_code %in% input_School2mun$Registry_from_registry$School_code)
    }
    if(!is.null(input_nstud)){
      if(!is.data.frame(input_nstud)){
        input_nstud <- input_nstud %>% lapply(function(x){
          x <- x %>% dplyr::filter(.data$School_code %in% input_School2mun$Registry_from_registry$School_code)})
      } else {
        input_nstud <- input_nstud %>%
          dplyr::filter(.data$School_code %in% input_School2mun$Registry_from_registry$School_code)

      }
    }
    if(!is.null(input_BroadBand)){
      input_BroadBand <- input_BroadBand %>%
        dplyr::filter(!grepl("[^A-Z]", substr(.data$School_code,1,4)) &
                      !grepl("X", substr(.data$School_code,1,4), ignore.case = TRUE)) %>%
        dplyr::filter(.data$School_code %in% input_School2mun$Registry_from_registry$School_code)
    }
  }

  if(!is.null(input_SchoolBuildings)){
    DB_SchoolBuildings_num <- input_SchoolBuildings %>% Util_DB_MIUR_num(
      include_numerics = SchoolBuildings_include_numerics,
      include_qualitatives = SchoolBuildings_include_qualitatives,
      row_cutout = SchoolBuildings_row_cutout, flag_outliers = SchoolBuildings_flag_outliers,
      track_deleted = FALSE,
      col_cut_thresh = SchoolBuildings_col_cut_thresh,
      verbose = verbose)
    DB_SchoolBuildings <- DB_SchoolBuildings_num %>%
      Group_DB_MIUR(verbose = verbose, track_deleted = FALSE, Year = Year,
                    count_units = TRUE, countname = "nbuildings",
                    count_missing = SchoolBuildings_count_missing,
                    InnerAreas = InnerAreas, ord_InnerAreas = ord_InnerAreas,
                    input_InnerAreas = input_InnerAreas)
    if(level %in% c("LAU", "Municipality")){
      DB_SchoolBuildings <- DB_SchoolBuildings$Municipality_data
    } else DB_SchoolBuildings <- DB_SchoolBuildings$Province_data
    DB_SchoolBuildings <- DB_SchoolBuildings %>%
      dplyr::filter(!.data$Order %in% c("IC", "IS", "NR")) %>%
      dplyr::select(-.data$Year)

    datasets[["SchoolBuildings"]] <- DB_SchoolBuildings
  }

  if(!is.null(input_nstud)){
    nstud_InnerAreas <- InnerAreas && length(datasets) == 0
    nstud_ord_InnerAreas <- nstud_InnerAreas && ord_InnerAreas
    nstud_aggr <-
      Group_nstud(data = input_nstud, Year = Year, nstud_imputation_thresh = nstud_imputation_thresh,
                  missing_to_1 = nstud_missing_to_1, UB_nstud_byclass = UB_nstud_byclass,
                  LB_nstud_byclass = LB_nstud_byclass, check = nstud_check, verbose = verbose,
                  check_registry = nstud_check_registry, InnerAreas = nstud_InnerAreas,
                  ord_InnerAreas = nstud_ord_InnerAreas,
                  input_Registry = input_Registry, input_InnerAreas = input_InnerAreas,
                  input_School2mun = input_School2mun, input_AdmUnNames = input_AdmUnNames)
    if(!is.data.frame(nstud_aggr)){
      if(level %in% c("LAU", "Municipality")){
        datasets[["nstud"]] <- nstud_aggr$Municipality_data
      } else datasets[["nstud"]] <- nstud_aggr$Province_data
    } else datasets[["nstud"]] <- nstud_aggr
  }

  if(!is.null(input_nteachers)){
    if(!is.null(input_nstud)){
      if(is.null(input_teachers4student)) {
        input_teachers4student <- Group_teachers4stud(
          Year = Year, input_nteachers = input_nteachers,
          verbose = verbose,
          input_nstud_aggr = nstud_aggr$Province_data,
          input_Registry = input_Registry,
          input_InnerAreas = input_InnerAreas,
          input_School2mun = input_School2mun)
      }
      datasets[["teachers4student"]] <- input_teachers4student %>%
        dplyr::mutate(Province_code = as.numeric(.data$Province_code))
    } else {
      datasets[["nteachers"]] <- input_nteachers
    }
  }

  if(!is.null(input_BroadBand)){

    if(BroadBand_impute_missing){
      BroadBand_missing <- input_School2mun$Registry_from_registry %>%
        School.order() %>%
        dplyr::filter(!.data$School_code %in% input_BroadBand$School_code) %>%
        dplyr::filter( !.data$Order %in% c("IC", "IS", "NR")) %>%
        dplyr::select(.data$School_code, .data$Order, .data$Province_code, .data$Province_initials,
                      .data$Municipality_code, .data$Municipality_description) %>%
        unique() %>% dplyr::mutate(BB_Activation_status = 0) %>%
        dplyr::left_join(dplyr::select(
          prov.names(), -.data$Province_initials),
          by = "Province_code")
      BroadBand_missing <- BroadBand_missing[,names(
        input_BroadBand[which(names(input_BroadBand) %in% names(BroadBand_missing))])]
      for(i in which(! names(input_BroadBand) %in% names(BroadBand_missing))){
        BroadBand_missing <- cbind(BroadBand_missing, rep(NA, nrow(BroadBand_missing)))
        names(BroadBand_missing)[ncol(BroadBand_missing)] <- names(input_BroadBand)[i]
      }
      BroadBand_missing <- BroadBand_missing %>% dplyr::select(names(input_BroadBand)) %>%
        dplyr::mutate(Province_description = stringr::str_to_title(.data$Province_description),
                      Region_description = stringr::str_to_title(.data$Region_description)) %>%
        structure(class = c("tbl_df", "tbl", "data.frame"))
      input_BroadBand <- rbind(input_BroadBand, BroadBand_missing) %>%
        dplyr::filter(.data$Order %in% c("Primary", "Middle", "High"))
    }

    BB <- Group_BroadBand(
      Date = Date, verbose = verbose, data = input_BroadBand )
    if(level %in% c("LAU", "Municipality")){
      datasets[["BroadBand"]] <- BB$Municipality_data %>%
        dplyr::select( -.data$Region_code, -.data$Region_description, -.data$nschools )
    } else {
      datasets[["BroadBand"]] <- BB$Province_data %>%
        dplyr::select(-.data$Region_code, -.data$Region_description, -.data$nschools)
    }
  }

  if(!is.null(input_Invalsi_IS)){

    Invalsi_IS <- Util_Invalsi_filter(data = input_Invalsi_IS,
                                      Year = Year, subj = Invalsi_subj, grade = Invalsi_grade, level = level,
                                      WLE = Invalsi_WLE, verbose = verbose)

    SchoolOrder <- c(ifelse(any(Invalsi_grade < 6), "Primary", NA),
                     ifelse(8 %in% Invalsi_grade, "Middle", NA),
                     ifelse(any(Invalsi_grade>8), "High", NA))
    SchoolOrder <- SchoolOrder[!is.na(SchoolOrder)]

    Invalsi_primary <- Invalsi_IS  %>%
      dplyr::mutate(Order = NA) %>%
      dplyr::relocate(.data$Order, .after = 2) %>%
      dplyr::filter(!is.na(.data$Order))
    Invalsi_mid <- Invalsi_primary
    Invalsi_high <- Invalsi_primary

    if("Primary" %in% SchoolOrder){
      Invalsi_primary <- Invalsi_IS %>%
        dplyr::mutate(Order = "Primary") %>%
        dplyr::relocate(.data$Order, .after = 2)
      primcols <- grep(c("_2$|_5$"), names(Invalsi_primary))
      Invalsi_primary[, grep(c("_8|_10|_13"), names(Invalsi_primary))] <- NA
      Invalsi_primary <- Invalsi_primary[apply(
        Invalsi_primary[, primcols], 1, function(x) !all(is.na(x))), ]
    }
    if("Middle" %in% SchoolOrder){
      Invalsi_mid <- Invalsi_IS %>%
        dplyr::mutate(Order = "Middle") %>%
        dplyr::relocate(.data$Order, .after = 2)
      midcols <- grep(c("_8$"), names(Invalsi_mid))
      Invalsi_mid[, grep(c("_2$|_5$|_10$|_13$"), names(Invalsi_mid))] <- NA
      Invalsi_mid <- Invalsi_mid[apply(
        Invalsi_mid[, midcols], 1, function(x) !all(is.na(x))), ]
    }
    if("High" %in% SchoolOrder){
      Invalsi_high <- Invalsi_IS %>%
        dplyr::mutate(Order = "High") %>%
        dplyr::relocate(.data$Order, .after = 2)
      highcols <- grep(c("_10$|_13$"), names(Invalsi_high))
      Invalsi_high[, grep(c("_2$|_5$|_8$"), names(Invalsi_high))] <- NA
      Invalsi_high <- Invalsi_high[apply(
        Invalsi_high[, highcols], 1, function(x) !all(is.na(x))), ]
    }

    datasets[["Invalsi_IS"]] <-
      rbind(Invalsi_primary, Invalsi_mid, Invalsi_high)

    endtime <- Sys.time()
    if(verbose){
      cat(difftime(endtime, start.zero, units = "secs"), " seconds needed to import all input data \n \n")
    } #datasets <- datasets %>% lapply(function(x){#  if("Order" %in% colnames(x)) {#    x <- x %>% dplyr::filter(.data$Order %in% SchoolOrder)#    return(x)#  } else return(x)    #})
  }

  init <- input_School2mun$Registry_from_registry %>%
    School.order() %>%
    dplyr::filter(.data$Order %in% c("Primary", "Middle", "High")) %>%
    dplyr::distinct(.data$Province_code, .data$Province_initials,
                    .data$Municipality_code, .data$Municipality_description, .data$Order)
  datasets[["Registry"]] <- init

  datasets <- list(datasets$Registry, datasets$Invalsi_IS, datasets$SchoolBuildings,
                   datasets$nstud, datasets$BroadBand, datasets$teachers4student,
                   datasets$nteachers)
  names(datasets) <- c("Registry", "Invalsi_IS", "SchoolBuildings", "nstud", "BroadBand",
                       "teachers4student", "nteachers")
  datasets <- Filter(Negate(is.null), datasets)

  if(level %in% c("LAU", "Municipality")){

    if(!is.null(datasets[["Invalsi_IS"]])){
      datasets[["Invalsi_IS"]] <- datasets[["Invalsi_IS"]] %>%
        dplyr::mutate(Province_code = as.numeric(substr(.data$Municipality_code, 1, 3))) %>%
        dplyr::relocate(.data$Province_code, .before = 1)
    }

    datasets[2:length(datasets)] <- datasets[2:length(datasets)] %>%
      lapply(function(x){
        if(!any(grepl("Teachers", names(x)))){
          x <- x[, which(!names(x) %in% c("Year",
                                          "Municipality_description", "Province_description", "Province_code",
                                          "Province_initials", "nschools",
                                          "Region_description", "Region_code"))]
        } else{
          if(is.null(input_nstud)){
            x <- x[, which(names(x) %in%  c(
              "Province_code", "Order", "Tot_teachers", "Tot_ATA", "Tot_Students", "Students_per_class_Tot",
              "Tot_Classes", "Teachers_per_student", "Teachers_per_class",
              "ATA_per_student", "ATA_per_class"))]
          } else {
            x <- x[, which(names(x) %in%  c(
              "Province_code", "Order", "Tot_teachers", "Teachers_per_student", "Teachers_per_class",
              "Tot_ATA", "ATA_per_student", "ATA_per_class"))]
          }
        }
      })

    res <- datasets[[1]]
    remaining <- list()
    for(i in (2:length(datasets))){
      ncol.old <- ncol(res)
      if("Municipality_code" %in% names(datasets[[i]])){
        if("Order" %in% names(datasets[[i]])){
          res <- res %>%
            dplyr::left_join(datasets[[i]],by = c("Municipality_code", "Order"))
          notfound <- which(apply(res[-c(1:ncol.old)], MARGIN = 1, function(x) all(is.na(x))))
          if(length(notfound)>0){
            if(verbose){
              cat(paste(length(notfound)), " units in ",
                  names(datasets[i-1]), ifelse(i>2, " and in previous datasets", ""),
                  " are missing in ", names(datasets[i]), "\n")
            }
            res <- res %>% NA.join.manage(lastcol = ncol.old, NA_autoRM = NA_autoRM, verbose = verbose)
          }
        } else {
          remaining[[length(remaining)+1]] <- datasets[[i]]
          names(remaining)[[length(remaining)]] <- names(datasets[[i]])
        }
      } else if("Province_code" %in% names(datasets[[i]])){
        if("Order" %in% names(res) && !all(is.na(datasets[[i]]$Order))){
          res <- res %>% dplyr::left_join(datasets[[i]], by = c("Province_code", "Order"))
        } else {
          # WARNING: this is specific to nteachers - not robust to different data
          datasets[[i]] <- datasets[[i]][,which(
            names(datasets[[i]]) %in% c("Province_code", "Tot_Students", "Tot_Classes",
                                        "Tot_teachers", "Tot_ATA"))]
          startcol <- 1
          endcol <- ncol(datasets[[i]]) - 1
          datasets[[i]] <- datasets[[i]] %>%
            dplyr::group_by(.data$Province_code) %>%
            dplyr::summarise(dplyr::across(c(startcol:endcol), list(sum))) %>%
            dplyr::ungroup()
          names(datasets[[i]]) <- stringr::str_remove(names(datasets[[i]]), "_1")
          if("Tot_Students" %in% names(datasets[[i]])){
            if("Tot_teachers" %in% names(datasets[[i]])){
              datasets[[i]] <- datasets[[i]] %>%
                dplyr::mutate(Teachers_per_student = .data$Tot_teachers/.data$Tot_Students)
            }
            if("Tot_ATA" %in% names(datasets[[i]])){
              datasets[[i]] <- datasets[[i]] %>%
                dplyr::mutate(ATA_per_student = .data$Tot_ATA/.data$Tot_Students)
            }

            if("Tot_Classes" %in% names(datasets[[i]])){
              datasets[[i]] <- datasets[[i]] %>%
                dplyr::mutate(Students_per_class_Tot = .data$Tot_Students/.data$Tot_Classes)
            }
          }
          if("Tot_Classes" %in% names(datasets[[i]])){
            if("Tot_teachers" %in% names(datasets[[i]])){
              datasets[[i]] <- datasets[[i]] %>%
                dplyr::mutate(Teachers_per_class = .data$Tot_teachers/.data$Tot_Classes)
            }
            if("Tot_ATA" %in% names(datasets[[i]])){
              datasets[[i]] <- datasets[[i]] %>%
                dplyr::mutate(Teachers_per_class = .data$Tot_teachers/.data$Tot_Classes)

            }
          }
          res <- res %>% dplyr::left_join(datasets[[i]], by = "Province_code")
        }
        if(sum(is.na(res[,ncol.old+1]))>0){
          if(verbose){
            cat(paste(sum(is.na(res[,ncol.old+1]))), " units in ",
                names(datasets[i-1]), ifelse(i>2, " and in previous datasets", ""),
                " are missing in ", names(datasets[i]), "\n")
          }
          res <- res %>% NA.join.manage(lastcol = ncol.old, NA_autoRM = NA_autoRM, verbose = verbose)
        }
      }
    }
    if(length(remaining) != 0){
      for(i in 1:length(remaining)){
        ncol.old <- ncol(res)
        res <- res %>% dplyr::left_join(remaining[[i]], by = "Municipality_code")
        if(sum(is.na(res[,ncol.old+1]))>0){
          if(verbose){
            message(paste(sum(is.na(res[,ncol.old+1]))), " units in previous datasets",
                    " are missing in ", names(remaining)[i], "\n")
          }
          NA.join.manage(res, ncol.old, NA_autoRM = NA_autoRM, verbose = verbose)
        }
      }
    }
  } else {
    datasets[["Registry"]] <- datasets[["Registry"]] %>%
      dplyr::distinct(.data$Province_code, .data$Province_initials, .data$Order)

    datasets[2:length(datasets)] <- datasets[2:length(datasets)] %>%
      lapply(function(x){
        x <- x[,which(!names(x) %in% c(
          "Year","Municipality_description", "Province_description",
          "Municipality_code","Province_initials", "nschools",
          "Region_description", "Region_code"))] })
    res <- datasets[[1]]
    #remaining <- list() #CURRENTLY not needed. Currently.
    for(i in (2:length(datasets))){
      ncol.old <- ncol(res)
      if("Order" %in% names(datasets[[i]])){
        res <- res %>%  dplyr::left_join(datasets[[i]], by = c("Province_code", "Order"))
      } else {
        res <- res %>%  dplyr::left_join(datasets[[i]], by = "Province_code")
      }
      notfound <- which(apply(res[-c(1:ncol.old)], MARGIN = 1, function(x) all(is.na(x))))
      if(length(notfound)>0){
        if(verbose){
          cat(paste(length(notfound)), " units in ",
              names(datasets[i-1]), ifelse(i>2, " and in previous datasets", ""),
              " are missing in ", names(datasets[i]), "\n")
        }
        res <- res %>% NA.join.manage(lastcol = ncol.old, NA_autoRM = NA_autoRM, verbose = verbose)
      }
    }
  }

  names(res) <- names(res) %>%  stringr::str_remove_all("\\.y") %>% stringr::str_remove_all("\\.x")
  res <- res[,!duplicated(colnames(res))]

  endtime <- Sys.time()
  if(verbose){
    cat("Total time needed to build the database: ",
        difftime(endtime, start.zero, units = "secs"), " seconds \n \n")
  }

  return(res)
}
