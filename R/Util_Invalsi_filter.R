#' Filter the Invalsi data by subject, school grade and year.
#'
#' @description  This function filters the database of Invalsi scores (see \code{\link{Get_Invalsi_IS}}) by school year, education grade and subject and returns a dataframe in wide format.
#' Each row corresponds to one territorial unit (either municipality or province); the numerical variables are three (the mean score, the score's standard deviation and the students coverage percentage) for each selected subject.
#'
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The raw Invalsi survey data that has to be filtered, obtained as output of the \code{\link{Get_Invalsi_IS}} function.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param subj Character. The school subject(s) to include, among \code{"Englis_listening"}/\code{"ELI"}, \code{"English_reading"}/\code{"ERE"}, \code{"Italian"}/\code{"ITA"} and \code{"Mathematics"}/\code{"MAT"}. All four by default.
#' @param grade Numeric. The school grade to chose. Either \code{2} (2nd year of primary school), \code{5} (last year of primary school), \code{8} (last year of middle shcool), \code{10} (2nd year of high school) or \code{13} (last year of school). \code{8} by default
#' @param level Character. The level of aggregation of Invalsi census data. Either \code{"NUTS-3"}, \code{"Province"}, \code{"LAU"}, \code{"Municipality"}.
#' If an input dataframe is provided, please select the same level of aggregation. \code{"LAU"} by default
#' @param WLE Logical. Whether the variable to choose should be the average WLE score rather that the percentage of sufficient tests, if both are available. \code{FALSE} by default
#' @param Year Numeric or character value. Reference school year for the data (last available is 2022/23).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022}. \code{2023} by default
#' @param verbose Logical. If \code{TRUE}, the function informs about the time needed. \code{TRUE} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. For all subjects and school grades, the variables indicate:
#'
#' \itemize{
#' \item \code{M} The mean score, either WLE or percentage of sufficient tests
#' \item \code{S} The standard deviation of the score
#' \item \code{C} The students coverage percentage (expressed in the scale 1 - 100)
#' }
#'
#'
#'
#'
#' @examples
#'
#'
#'
#' Util_Invalsi_filter(subj = c("Italian", "Mathematics"), grade = 5, level = "NUTS-3", Year = 2023,
#'                    WLE = FALSE, data = example_Invalsi23_prov)
#'
#' Util_Invalsi_filter(subj = c("Italian", "Mathematics"), grade = 5, level = "NUTS-3", Year = 2023,
#'                     WLE = TRUE, data = example_Invalsi23_prov)
#'
#' Invalsi23_high <- Util_Invalsi_filter(subj = "Italian", grade = c(10,13), level = "NUTS-3",
#'                                       Year = 2023, data = example_Invalsi23_prov)
#'
#'
#'  summary(Invalsi23_high)
#'
#' @export

Util_Invalsi_filter <- function(data = NULL, subj=c("ELI", "ERE", "ITA", "MAT"), grade = 8, level = "LAU", WLE=FALSE,
                                Year = 2023, verbose = TRUE, autoAbort = FALSE){

  starttime <- Sys.time()

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

  Y <- year.patternA(Year)
  for(i in 1:length(subj)){
    if(toupper(subj[i]) %in% c("ELI", "ENGLISH L", "ENGLISH_L", "INGLESE L", "INGLESE_L",
                               "ENGLISH LISTENING", "ENGLISH_LISTENING")){
      subj[i] <- "English_R"
    } else if(toupper(subj[i]) %in% c("ERE", "ENGLISH R", "ENGLISH_R", "INGLESE R", "INGLESE_R",
                                      "ENGLISH READING", "ENGLISH_READING", "INGLESE SCRITTO", "INGLESE_SCRITTO")){
      subj[i] <- "English_L"
    } else if (toupper(subj[i]) %in% c("ITA", "ITALIANO", "ITALIAN")){
      subj[i] <- "Italian"
    } else if (toupper(subj[i]) %in% c("MAT", "MATEMATICA", "MATHS", "MATHEMATICS")){
      subj[i] <- "Mathematics"
    }
  }

  Invalsi_IS <- data %>%
    dplyr::mutate(Year =  stringr::str_remove_all(.data$Year, "-")) %>%
    dplyr::mutate(Subject =  stringr::str_replace_all(.data$Subject, " ", "_")) %>%
    dplyr::filter(.data$Year == Y) %>%
    dplyr::filter(.data$Grade %in% grade) %>%
    dplyr::filter(.data$Subject %in% subj)

  if(nrow(Invalsi_IS) == 0){
    message("No Invalsi data found for year: ", Year, ", grade: ", grade, ", subject: ", subj,
      ". We apologise for the inconvenience. \n")
    return(NULL)
  }

  if (13 %in% grade | as.numeric(substr(Y, 1, 4)) > 2016 & any(grade >5)){
    WLE <- TRUE
  }

  if (level %in% c("LAU","Municipality")){

    doub <- which(duplicated(dplyr::select(Invalsi_IS, .data$Municipality_code, .data$Year,
                                           .data$Subject, .data$Grade)))
    if(length(doub) > 0) Invalsi_IS <- Invalsi_IS[-doub,]

    if (WLE){
      DB.Invalsi <- Invalsi_IS %>%
        dplyr::select(.data$Municipality_code, .data$Municipality_description, .data$Grade, .data$Subject,
                      .data$Year, .data$WLE_average_score, .data$Std_dev_WLE_score, .data$Students_coverage)
    } else {
      DB.Invalsi <- Invalsi_IS %>%
        dplyr::select(.data$Municipality_code, .data$Municipality_description, .data$Grade, .data$Subject,
                      .data$Year, .data$Average_percentage_score, .data$Std_dev_percentage_score, .data$Students_coverage)
    }

    names(DB.Invalsi)[c(6,7,8)] <- c("Mean", "StDev", "Coverage")
    DB.Invalsi <- DB.Invalsi %>% dplyr::filter(.data$Mean != 999 & .data$StDev != 999)


    # Necessary since in some cases different names are associated to the same municipality code
    # (for the same year) and this ambiguity would cause inconvenience when the dataframe is spreaded
    Mun.rename <- DB.Invalsi %>% dplyr::select(.data$Municipality_code, .data$Municipality_description) %>%
      dplyr::filter(!duplicated(.data$Municipality_code))
    DB.Invalsi <- DB.Invalsi %>% dplyr::select(-.data$Municipality_description) %>%
      dplyr::left_join(Mun.rename, by = "Municipality_code") %>%
      dplyr::relocate(.data$Municipality_description, .after = "Municipality_code")

    DB.Invalsi.m <- DB.Invalsi %>% dplyr::select(-.data$StDev, -.data$Coverage) %>%
      tidyr::spread(key = .data$Subject, value = .data$Mean)
    DB.Invalsi.s <- DB.Invalsi %>% dplyr::select(-.data$Mean, -.data$Coverage) %>%
      tidyr::spread(key = .data$Subject, value = .data$StDev) %>%
      dplyr::select(-.data$Municipality_description)
    DB.Invalsi.c <- DB.Invalsi %>% dplyr::select(-.data$Mean, -.data$StDev) %>%
      tidyr::spread(key = .data$Subject, value = .data$Coverage) %>%
      dplyr::select(-.data$Municipality_description)

    names(DB.Invalsi.m)[c(5:8)] <- paste ("M",names(DB.Invalsi.m)[c(5:8)],sep="_")
    names(DB.Invalsi.s)[c(4:7)] <- paste ("S",names(DB.Invalsi.s)[c(4:7)],sep="_")
    names(DB.Invalsi.c)[c(4:7)] <- paste ("C",names(DB.Invalsi.c)[c(4:7)],sep="_")

    DB.Invalsi <- DB.Invalsi.m %>%
      dplyr::left_join(DB.Invalsi.s, by = c("Municipality_code", "Grade", "Year")) %>%
      dplyr::left_join(DB.Invalsi.c, by = c("Municipality_code", "Grade", "Year")) %>%
      dplyr::select(-.data$Year)
    values_from <- names(DB.Invalsi)[4:ncol(DB.Invalsi)]
    DB.Invalsi <- DB.Invalsi %>%
      tidyr::pivot_wider(id_cols = c(.data$Municipality_code, .data$Municipality_description),
                         names_from = .data$Grade,
                         values_from = values_from)

  } else if (level %in% c("Province", "NUTS-3") ) {

    # The abbreviation for the province of Naples is confused with "Not Available"
    Invalsi_IS$Province_initials <-
      Invalsi_IS$Province_initials %>% stringr::str_replace_na("NA")

    doub <- which(duplicated(dplyr::select(Invalsi_IS, .data$Province_initials,
                                           .data$Year, .data$Subject, .data$Grade)))
    if(length(doub) > 0) Invalsi_IS <- Invalsi_IS[-doub,]

    if (!WLE){
      DB.Invalsi <- Invalsi_IS %>%
        dplyr::select(.data$Year, .data$Grade, .data$Subject, .data$Province_code,
                      .data$Province_initials, .data$Average_percentage_score, .data$Std_dev_percentage_score, .data$Students_coverage)
    } else {
      DB.Invalsi <- Invalsi_IS %>%
        dplyr::select(.data$Year, .data$Grade, .data$Subject, .data$Province_code,
                      .data$Province_initials, .data$WLE_average_score, .data$Std_dev_WLE_score, .data$Students_coverage)
    }
    names(DB.Invalsi)[c(6,7,8)] <- c("Mean", "StDev", "Coverage")

    DB.Invalsi <- DB.Invalsi %>% dplyr::filter(.data$Mean != 999 & .data$StDev != 999)
    DB.Invalsi.m <- DB.Invalsi %>% dplyr::select(-.data$StDev, -.data$Coverage) %>%
      tidyr::spread(key = .data$Subject, value=.data$Mean)
    DB.Invalsi.s <- DB.Invalsi %>% dplyr::select(-.data$Mean, -.data$Province_initials, -.data$Coverage) %>%
      tidyr::spread(key = .data$Subject, value=.data$StDev)
    DB.Invalsi.c <- DB.Invalsi %>% dplyr::select(-.data$Mean, -.data$Province_initials, -.data$StDev) %>%
      tidyr::spread(key = .data$Subject, value = .data$Coverage)

    names(DB.Invalsi.m)[c(5:8)] <- paste ("M",names(DB.Invalsi.m)[c(5:8)],sep="_")
    names(DB.Invalsi.s)[c(4:7)] <- paste ("S",names(DB.Invalsi.s)[c(4:7)],sep="_")
    names(DB.Invalsi.c)[c(4:7)] <- paste ("C",names(DB.Invalsi.c)[c(4:7)],sep="_")

    DB.Invalsi <- DB.Invalsi.m %>%
      dplyr::left_join(DB.Invalsi.s, by = c("Province_code", "Grade", "Year")) %>%
      dplyr::left_join(DB.Invalsi.c, by = c("Province_code", "Grade", "Year")) %>%
      dplyr::select(-.data$Year)
    values_from <- names(DB.Invalsi)[4:ncol(DB.Invalsi)]
    DB.Invalsi <- DB.Invalsi %>%
      tidyr::pivot_wider(id_cols = c(.data$Province_code, .data$Province_initials),
                         names_from = .data$Grade,
                         values_from = values_from)

  } else {
    stop("Please, select either Municipality/LAU or Province/NUTS-3 as administrative level")
  }

  if(nrow(DB.Invalsi)==0){
    stop("No Invalsi data available", .call = TRUE)
  }

  DB.Invalsi <- DB.Invalsi[, which(!unlist(lapply(DB.Invalsi, function(x) all(is.na(x)))))]
  DB.Invalsi <- DB.Invalsi[,c(1,2, 2 + c(order(as.numeric(stringr::str_extract(names(DB.Invalsi)[-c(1,2)], "\\d+$")))))]
  endtime <- Sys.time()

  if(verbose) {
    cat(paste("Total running time to", ifelse(is.null(data), "retrieve and", ""),
              "process Invalsi", level, "data", "for year",
              Y, "subject", paste(subj, collapse = ", "),"School year n.",
              paste(grade, collapse = ", "), ": \n"))
    cat(paste(round(difftime(endtime, starttime, units="secs") ,2), "seconds \n"))

  }

  return(DB.Invalsi)

}


