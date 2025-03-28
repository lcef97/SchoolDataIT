#' Clean the raw dataframe of the number of students and arrange it in a wide format
#'
#' @description  This function rearranges the output of the \code{\link{Get_nstud}} function in such a way to represent the
#' counts of students and, if required, either the number of students by class and number of classes, or
#' the counts of students per school timetable (running time) in a unique observation per school.
#' If the focus is on class size, this function  firstly cleans the data from the outliers in terms of
#' average number of students by class at the school level and imputates the number of classes to 1 when missing.
#'
#'
#'
#'
#'
#' @param data Object of class \code{list}, including two objects of class \code{tbl_df},  \code{tbl} and \code{data.frame}, obtainded as output of the \code{\link{Get_nstud}} function with the default \code{filename} parameter.
#' If \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param missing_to_1 Logical. If focus is on class size, whether the number of classes should be imputed to 1 when it is missing and the number of students is below a threshold (argument \code{nstud_imputation_thresh}). \code{TRUE} by default.
#' @param nstud_imputation_thresh Numeric. If focus is on class size, the minimum threshold below which the number of classes is imputed to 1 if missing, if \code{missing_to_1 == TRUE}.
#'  E.g. if the threshold is 19, for all the schools in which there are 19 or less students in a given grade but the number of classes for that grade is missing, the number of classes is imputated to 1. \code{19} by default.
#' @param UB_nstud_byclass Numeric. Either a unique value for all school orders, or a vector of three order-specific values in the order: primary, middle, high.
#' If focus is on class size, the upper limit of the acceptable school-level (if \code{filter_by_grade == FALSE}) or grade-level (otherwise) average of the number of students by class.
#' If a whole school or any grade in a school respectively has a higher number of students by class, the record is considered an outlier and filtered out. \code{99} by default, i.e. no restriction is made.
#'  Please notice that boundaries are included in the acceptance interval.
#' @param LB_nstud_byclass Numeric. Either a unique value for all school orders, or a vector of three order-specific values in the order: primary, middle, wide.
#' If focus is on class size, the lower limit of the acceptable school-level (if \code{filter_by_grade == FALSE}) or grade_level (otherwise) average of the number of students by class.
#'  If a whole school or any grade in a school respectively has a smaller number of students by class, the record is considered an outlier and filtered out. \code{1} by default.
#'  Please notice that boundaries are included in the acceptance interval.
#' @param filter_by_grade Logical. If focus is on class size, whether to remove all school grades with average class size outside of the acceptance boundaries. \code{FALSE} by default.
#' @param UB_nstud_byclass_grade Numeric. IF \code{filter_by_grade == TRUE}, the upper limit of the acceptable grade-level average class size.
#' If \code{NULL} it is set equal to \code{UB_nstud_byclass}. \code{NULL} by default.
#' @param LB_nstud_byclass_grade Numeric. IF \code{filter_by_grade == TRUE}, the lowrer limit of the acceptable grade-level average class size.
#'  If \code{NULL} it is set equal to \code{LB_nstud_byclass}. \code{NULL} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Arguments to \code{\link{Get_nstud}}, needed if \code{data} is not provided.
#'
#'
#'
#' @return An object of class  \code{tbl_df}, \code{tbl} and \code{data.frame}
#'
#'
#' @details In the example, we compare the dataframe obtained with the default settings
#'  and the one imposed setting narrow inclusion criteria
#'
#' @examples
#'
#'
#' nstud.default <- Util_nstud_wide(example_input_nstud23)
#'
#'
#' nstud.narrow <- Util_nstud_wide(example_input_nstud23,
#'   UB_nstud_byclass = 35, LB_nstud_byclass = 5 )
#'
#' nrow(nstud.default)
#' nrow(nstud.narrow)
#'
#' nstud.default
#'
#' summary(nstud.default)
#'
#'
#' @export


Util_nstud_wide <- function(data = NULL, missing_to_1 = FALSE,
                            nstud_imputation_thresh = 19,
                            UB_nstud_byclass = 99, LB_nstud_byclass = 1,
                            filter_by_grade = FALSE,
                            UB_nstud_byclass_grade = NULL,
                            LB_nstud_byclass_grade = NULL,
                            verbose = TRUE, autoAbort = FALSE, ...){

  options(dplyr.summarise.inform = FALSE)

  while (is.null(data)){
    if(verbose) cat("Downloading input data \n")
    #if(missing_to_1){
    #  filename <- c("ALUCORSOETASTA", "ALUCORSOINDCLASTA")
    #} else {
    #  filename <- "ALUCORSOINDCLASTA"
    #}
    data  <- Get_nstud(verbose = verbose, autoAbort = autoAbort, ...)
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

  if(is.data.frame(data) || length(data) == 1L ||
     (!is.data.frame(data) && !"ALUCORSOINDCLASTA" %in% names(data))) missing_to_1 <- FALSE

  if (is.data.frame(data)){
    nstud.wide <- data %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)
  }  else if(length(data) == 1L){
    nstud.wide <- data[[1L]] %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)
  } else {
    data <- data %>% lapply(function(x){
      if(! "ID" %in% colnames(data)){
        tidyr::unite(x, "ID", c(.data$School_code, .data$Grade), remove = FALSE)
      }} )
    nstud.wide <- data[names(data)[which(names(data)!="ALUCORSOETASTA")]][[1]] %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)

    data$ALUCORSOETASTA <- data$ALUCORSOETASTA %>% dplyr::group_by(
      .data$Year, .data$ID, .data$School_code, .data$Order, .data$Grade) %>%
      dplyr::summarise(Students = sum(.data$Students)) %>% dplyr::ungroup()

    probl.ID <- data$ALUCORSOETASTA %>% dplyr::filter(! .data$ID %in% nstud.wide$ID) %>%
      dplyr::select(.data$ID) %>% unique() %>% unlist() %>% as.vector()

    if(missing_to_1){

      if(verbose){
        cat("Imputating missing number of classes to 1 for school years with ",
            nstud_imputation_thresh, "students or less\n")
      }

      probl <- data$ALUCORSOETASTA %>%
        dplyr::filter(.data$ID %in% probl.ID & .data$Students <= nstud_imputation_thresh) %>%
        dplyr::mutate(Classes = 1) %>% dplyr::relocate(.data$Students, .after ="Classes")
      nrow.old <- nrow(nstud.wide)
      nstud.wide <- rbind(nstud.wide, probl)
      nrow.new <- nrow(nstud.wide)
      if(verbose) {
        message(paste("Missing number of classes imputated to 1 for",
                      nrow.new - nrow.old, "schools"))
      }
    }
    nstud.wide <- nstud.wide %>%
      dplyr::select(-.data$ID)
  }

  if("Classes" %in% names (nstud.wide)){

    if(length(UB_nstud_byclass) != 3L){
      UB_nstud_byclass <- rep(UB_nstud_byclass[1L], 3)
    }
    if(length(LB_nstud_byclass) != 3L){
      LB_nstud_byclass <- rep(LB_nstud_byclass[1L], 3)
    }

    if(filter_by_grade){
      if(is.null(LB_nstud_byclass_grade)) LB_nstud_byclass_grade <- LB_nstud_byclass
      if(is.null(UB_nstud_byclass_grade)) UB_nstud_byclass_grade <- UB_nstud_byclass
      nrow.old <- nrow(nstud.wide)
      nstud.wide <- nstud.wide %>%
        dplyr::filter(.data$Order == "Primary" &
                        dplyr::between(.data$Students/.data$Classes,
                                       LB_nstud_byclass_grade[1L],
                                       UB_nstud_byclass_grade[1L]) |
                        .data$Order == "Middle" &
                        dplyr::between(.data$Students/.data$Classes,
                                       LB_nstud_byclass_grade[2L],
                                       UB_nstud_byclass_grade[2L]) |
                        .data$Order == "High" &
                        dplyr::between(.data$Students/.data$Classes,
                                       LB_nstud_byclass_grade[3L],
                                       UB_nstud_byclass_grade[3L]))
      nrow.new <- nrow(nstud.wide)
      if(verbose & nrow.new < nrow.old){
        message(paste("Filtered out", nrow.old - nrow.new, "schools with either less than",
                      LB_nstud_byclass_grade[1L], "(primary),",
                      LB_nstud_byclass_grade[2L], "(middle),",
                      LB_nstud_byclass_grade[3L], "(high)\n",
                      "or more than",
                      UB_nstud_byclass_grade[1L], "(primary),",
                      UB_nstud_byclass_grade[2L], "(middle),",
                      UB_nstud_byclass_grade[3L], "(high)",
                      "students per class in any grade"))
      }
    }

    nstud.wide <- nstud.wide %>% dplyr::select(-.data$Year) %>%
      dplyr::filter(.data$Order != "Primary" | .data$Grade < 6) %>%
      dplyr::filter(.data$Grade < 14) %>%
      dplyr::mutate(dplyr::across(.data$Grade,  ~paste0("grade_", .data$Grade))) %>%
      tidyr::pivot_wider(names_from = .data$Grade, values_from = c(.data$Classes, .data$Students))

    nn <- paste(rep(c("Students_grade_", "Classes_grade_"),13),
                sort(as.numeric(gsub("\\D", "", names(nstud.wide)[3:28] )) ), sep = "")

    nstud.wide <- nstud.wide[,c(1,2,match(nn, names(nstud.wide)))]

    nstud.wide[is.na(nstud.wide)] <- 0

    nstud.wide <- nstud.wide %>%
      dplyr::mutate(Tot_Students = rowSums(nstud.wide[,which(grepl("Students", names(nstud.wide)))])) %>%
      dplyr::mutate(Tot_Classes =rowSums(nstud.wide[,which(grepl("Classes", names(nstud.wide)))]))

    for (i in (1:14)){
      j <- 2*i + 1
      nstud.wide <- nstud.wide %>%
        dplyr::mutate(xx = as.numeric(unlist(dplyr::select(nstud.wide, j)/dplyr::select(nstud.wide,j+1) ) ) )
      names(nstud.wide)[ncol(nstud.wide)] <- paste("Students_per_class_", ifelse(i<14, i,"Tot"), sep = "")
    }
    nstud.wide[is.na(nstud.wide)] <- 0

    for (i in c(1:13)){
      j <- i + 30
      k <- 3*i + 1
      nstud.wide <- nstud.wide %>% dplyr::relocate(j, .after = k)
    }

    nrow.old <- nrow(nstud.wide)
    nstud.wide <- nstud.wide %>%
      dplyr::filter(.data$Order == "Primary" &
                      dplyr::between(.data$Students_per_class_Tot,
                                     LB_nstud_byclass[1L],
                                     UB_nstud_byclass[1L]) |
                      .data$Order == "Middle" &
                      dplyr::between(.data$Students_per_class_Tot,
                                     LB_nstud_byclass[2L],
                                     UB_nstud_byclass[2L]) |
                      .data$Order == "High" &
                      dplyr::between(.data$Students_per_class_Tot,
                                     LB_nstud_byclass[3L],
                                     UB_nstud_byclass[3L]))
    nrow.new <- nrow(nstud.wide)
    if(verbose & nrow.new < nrow.old){
      message(paste("Filtered out", nrow.old - nrow.new, "schools with either less than",
                    LB_nstud_byclass[1L], "(primary),",
                    LB_nstud_byclass[2L], "(middle),",
                    LB_nstud_byclass[3L], "(high)\n",
                    "or more than",
                    UB_nstud_byclass[1L], "(primary),",
                    UB_nstud_byclass[2L], "(middle),",
                    UB_nstud_byclass[3L], "(high)",
                    "students per class on average"))
    }
  } else if("Running_time" %in% names(nstud.wide)){
    nstud.wide <- nstud.wide %>% dplyr::select(-.data$Year) %>%
      dplyr::filter(.data$Order != "Primary" | .data$Grade < 6) %>%
      dplyr::filter(.data$Grade < 14) %>%
      dplyr::mutate(dplyr::across(.data$Grade,  ~ paste0("Students_G_", .data$Grade))) %>%
      tidyr::pivot_wider(names_from = c(.data$Grade, .data$Running_time), values_from = c(.data$Students))

    nn <- paste(rep("Students_G_", 8),
                sort(as.numeric(gsub("\\D", "", names(nstud.wide)[3:18] )) ),
                c(rep(c("_Ordinary", "_Full_time"),5),  rep(c("_Ordinary", "_Music_oriented"),3)),
                sep = "")

    nstud.wide <- nstud.wide[,c(1,2,match(nn, names(nstud.wide)))]

    nstud.wide[is.na(nstud.wide)] <- 0

    nstud.wide <- nstud.wide %>%
      dplyr::mutate(Tot_Students_ordinary =
                      rowSums(nstud.wide[which(grepl("Ordinary", names(nstud.wide)))])) %>%
      dplyr::mutate(Tot_Students_full_time =
                      rowSums(nstud.wide[which(grepl("Full_time", names(nstud.wide)))])) %>%
      dplyr::mutate(Tot_Students_music_oriented =
                      rowSums(nstud.wide[which(grepl("Music_oriented", names(nstud.wide)))])) %>%
      dplyr::mutate(Tot_Students = .data$Tot_Students_ordinary +
                      .data$Tot_Students_full_time +
                      .data$Tot_Students_music_oriented)
  }


  return(nstud.wide)
}
