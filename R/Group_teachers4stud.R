#' Arrange the number of teachers per students in public Italian schools at the province level
#'
#' @description  This function provides the average number of teachers per students in Italian public schools at the province level.
#'
#'
#'
#'
#' @param Year Numeric or character value. Reference school year for the school registry data (last available is 2022).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022}. \code{2023} by default
#' @param input_nteachers Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. The number of teachers by province, obtained as output of the function \code{\link{Get_nteachers_prov}}. If \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param input_nstud_raw Object of class 'list', including two objects of class \code{tbl_df}', \code{tbl} and \code{data.frame}, obtainded as output of the \code{\link{Get_nstud}} function with the default \code{filename} parameter.
#' Not necessary if the argument \code{input_nstud_aggr} is provided. If \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param input_nstud_aggr Object of class \code{list}, including two objects of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Group_nstud}}. If \code{NULL}, the function will compute it manually but it will not be saved in the global environment. \code{NULL} by default.
#' @param nteachers_filename Character. If \code{input_nteachers} is not provided, which data to retrieve regarding the number of teachers/personnel; see \code{\link{Get_nteachers_prov}}
#'  \code{c("DOCTIT", "DOCSUP")} by default, i.e. tenured theachers and temporary teachers.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. TRUE by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Arguments to \code{\link{Group_nstud}} if argument \code{input_nstud_aggr} is not provided
#'
#'
#'
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}
#'
#'
#'
#' @examples
#'
#'
#' \donttest{
#'   input_nstud23 <- Get_nstud(2023, filename ="ALUCORSOINDCLASTA", autoAbort = TRUE)
#'   Registry23 <- Get_Registry(2023, autoAbort = TRUE)
#'   School2mun23 <- Get_School2mun(2023, input_Registry = Registry23, autoAbort = TRUE)
#'
#'
#'   nstud23.aggr <- Group_nstud(Year = 2023, data = input_nstud23,
#'     input_Registry = Registry23, input_School2mun = School2mun23,
#'     autoAbort = TRUE)
#'
#'   input_nteachers23 <- Get_nteachers_prov(2023, autoAbort = TRUE)
#'
#'   teachers4stud <- Group_teachers4stud(Year = 2023,
#'                    input_nteachers = input_nteachers23,
#'                    input_nstud_aggr = nstud23.aggr, autoAbort = TRUE)
#'
#'   teachers4stud[, -c(1, 2, 10, 11)]
#'
#'   summary(teachers4stud)
#'}
#'
#' @export



Group_teachers4stud <- function(Year = 2023, input_nteachers = NULL,
                                nteachers_filename = c("DOCTIT", "DOCSUP"),
                                verbose = TRUE,input_nstud_raw = NULL,
                                input_nstud_aggr = NULL, autoAbort = FALSE, ...){

  while(is.null(input_nteachers)){
    if(verbose) cat("Retrieving number of teachers by province")
    input_nteachers <- Get_nteachers_prov(Year = Year, verbose = verbose,
                                          filename = nteachers_filename, autoAbort = autoAbort)
    if(is.null(input_nteachers)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during teachers counts retrieving. Would you abort the whole operation or retry?",
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

  while (is.null(input_nstud_aggr)){
    if(is.null(input_nstud_raw)){
      cat("Retrieving and aggregating number of students by province \n" )} else {
        cat("Aggregating number of students by province \n")
      }
    input_nstud_aggr <- Group_nstud(Year = Year, data = input_nstud_raw, autoAbort = autoAbort, ...)
    if(is.null(input_nstud_aggr)){
      if(!autoAbort){
        holdOn <- ""
        message("Error during students counts processing Would you abort the whole operation or retry?",
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

  if(!is.data.frame(input_nstud_aggr)){
    input_nstud_aggr <- input_nstud_aggr$Province_data
  }
  nstud <- input_nstud_aggr[, which(names(input_nstud_aggr)%in% c(
      "Province_code", "Order", "nschools", "Tot_Students", "Tot_Classes",
      "Students_per_class_Tot", "Availability", "Availabili", "Inner_area"))]
  if("Availability"%in% names(nstud)){
    names(nstud) <- names(nstud) %>%
      stringr::str_replace("Availability", "nstud_availability")
  }

  if("TOT_TEACHERS" %in% toupper(names(input_nteachers))){
    nstud4teacher <- input_nteachers %>% dplyr::left_join(nstud, by = c("Province_code", "Order")) %>%
      dplyr::mutate(Teachers_per_student = .data$Tot_teachers/.data$Tot_Students,
                    Teachers_per_class = .data$Tot_teachers/.data$Tot_Classes)

    if("TOT_ATA" %in% toupper(names(input_nteachers))){
      nstud4teacher <- nstud4teacher %>%
        dplyr::mutate(ATA_per_student = .data$Tot_ATA/.data$Tot_Students,
                      ATA_per_class = .data$Tot_ATA/.data$Tot_Classes)
    }

  } else {
    nstud4teacher <- input_nteachers %>% dplyr::left_join(nstud, by = "Province_code") %>%
      dplyr::mutate(ATA_per_student = .data$Tot_ATA/.data$Tot_Students,
                    ATA_per_class = .data$Tot_ATA/.data$Tot_Classes)
  }



  return(nstud4teacher)
}
