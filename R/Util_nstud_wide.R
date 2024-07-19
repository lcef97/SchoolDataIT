#' Clean the raw dataframe of the number of students and arrange it in a wide format
#'
#' @description  This function firstly cleans the output of the \code{\link{Get_nstud}} function from the outliers in terms of average number of students by class at the school level and imputates the number of classes to 1 when missing,
#' then it rearranges the data into a wide format, in such a way to represent the number of students, the number of classes and the average number of students by class at each school grade in a unique observation for each school.
#'
#'
#'
#'
#' @param data Object of class \code{list}, including two objects of class \code{tbl_df},  \code{tbl} and \code{data.frame}, obtainded as output of the \code{\link{Get_nstud}} function with the default \code{filename} parameter.
#' If \code{NULL}, the function will download it automatically but it will not be saved in the global environment. \code{NULL} by default.
#' @param missing_to_1 Logical. Whether the number of classes should be imputed to 1 when it is missing and the number of students is below a threshold (argument \code{nstud_imputation_thresh}). \code{TRUE} by default.
#' @param nstud_imputation_thresh Numeric. The minimum threshold below which the number of classes is imputed to 1 if missing, if \code{missing_to_1 == TRUE}.
#'  E.g. if the threshold is 19, for all the schools in which there are 19 or less students in a given grade but the number of classes for that grade is missing, the number of classes is imputated to 1. \code{19} by default.
#' @param UB_nstud_byclass Numeric. The upper limit of the acceptable school-level average of the number of students by class. If a school has, on average, a higher number of students by class, the record is considered an outlier and filtered out. \code{99} by default, i.e. no restriction is made. Please notice that boundaries are included in the acceptance interval.
#' @param LB_nstud_byclass Numeric. The lower limit of the acceptable school-level average of the number of students by class. If a school has, on average, a smaller number of students by class, the record is considered an outlier and filtered out. \code{1} by default. Please notice that boundaries are included in the acceptance interval.
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
                            verbose = TRUE, autoAbort = FALSE, ...){

  options(dplyr.summarise.inform = FALSE)

  while (is.null(data)){
    if(verbose) cat("Downloading input data \n")
    if(missing_to_1){
      filename <- c("ALUCORSOETASTA", "ALUCORSOINDCLASTA")
    } else {
      filename <- "ALUCORSOINDCLASTA"
    }
    data  <- Get_nstud(filename = filename, verbose = verbose, autoAbort = autoAbort, ...)
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

  if(is.data.frame(data) || length(data) == 1L) missing_to_1 <- FALSE

 if (is.data.frame(data)){
    nstud.byclass <- data %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)
  }  else if(length(data) == 1L){
    nstud.byclass <- data[[1L]] %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)
  } else {
    data <- data %>% lapply(function(x){
      if(! "ID" %in% colnames(data)){
        tidyr::unite(x, "ID", c(.data$School_code, .data$Grade), remove = FALSE)
      }} )
    nstud.byclass <- data$ALUCORSOINDCLASTA %>%
      dplyr::mutate(Students = .data$Male_students + .data$Female_students) %>%
      dplyr::select(-.data$Male_students, -.data$Female_students)

    data$ALUCORSOETASTA <- data$ALUCORSOETASTA %>% dplyr::group_by(
      .data$Year, .data$ID, .data$School_code, .data$Order, .data$Grade) %>%
      dplyr::summarise(Students = sum(.data$Students)) %>% dplyr::ungroup()

    probl.ID <- data$ALUCORSOETASTA %>% dplyr::filter(! .data$ID %in% nstud.byclass$ID) %>%
      dplyr::select(.data$ID) %>% unique() %>% unlist() %>% as.vector()

    if(missing_to_1){

      if(verbose){
        cat("Imputating missing number of classes to 1 for school years with ",
            nstud_imputation_thresh, "students or less\n")
      }

      probl <- data$ALUCORSOETASTA %>%
        dplyr::filter(.data$ID %in% probl.ID & .data$Students <= nstud_imputation_thresh) %>%
        dplyr::mutate(Classes = 1) %>% dplyr::relocate(.data$Students, .after ="Classes")
      nrow.old <- nrow(nstud.byclass)
      nstud.byclass <- rbind(nstud.byclass, probl)
      nrow.new <- nrow(nstud.byclass)
      if(verbose) {
        message(paste("Missing number of classes imputated to 1 for",
                      nrow.new - nrow.old, "schools"))
      }
    }
    nstud.byclass <- nstud.byclass %>%
      dplyr::select(-.data$ID)
  }

  nstud.byclass <- nstud.byclass %>% dplyr::select(-.data$Year) %>%
    dplyr::filter(.data$Order != "Primary" | .data$Grade < 6) %>%
    dplyr::filter(.data$Grade < 14) %>%
    dplyr::mutate(dplyr::across(.data$Grade,  ~paste0("grade_", .data$Grade))) %>%
    tidyr::pivot_wider(names_from = .data$Grade, values_from = c(.data$Classes, .data$Students))

  nn <- paste(rep(c("Students_grade_", "Classes_grade_"),13),
              sort(as.numeric(gsub("\\D", "", names(nstud.byclass)[3:28] )) ), sep = "")

  nstud.byclass <- nstud.byclass[,c(1,2,match(nn, names(nstud.byclass)))]

  nstud.byclass[is.na(nstud.byclass)] <- 0

  nstud.byclass <- nstud.byclass %>%
    dplyr::mutate(Tot_Students = rowSums(nstud.byclass[,which(grepl("Students", names(nstud.byclass)))])) %>%
    dplyr::mutate(Tot_Classes =rowSums(nstud.byclass[,which(grepl("Classes", names(nstud.byclass)))]))

  for (i in (1:14)){
    j <- 2*i + 1
    nstud.byclass <- nstud.byclass %>%
      dplyr::mutate(xx = as.numeric(unlist(dplyr::select(nstud.byclass, j)/dplyr::select(nstud.byclass,j+1) ) ) )
    names(nstud.byclass)[ncol(nstud.byclass)] <- paste("Students_per_class_", ifelse(i<14, i,"Tot"), sep = "")
  }
  nstud.byclass[is.na(nstud.byclass)] <- 0

  for (i in c(1:13)){
    j <- i + 30
    k <- 3*i + 1
    nstud.byclass <- nstud.byclass %>% dplyr::relocate(j, .after = k)
  }

  nrow.old <- nrow(nstud.byclass)
  nstud.byclass <- nstud.byclass %>%
    dplyr::filter(dplyr::between(.data$Students_per_class_Tot, LB_nstud_byclass, UB_nstud_byclass))
  nrow.new <- nrow(nstud.byclass)
  if(verbose & nrow.new < nrow.old){
    message(paste("Filtered out", nrow.old - nrow.new, "schools with less than", LB_nstud_byclass,
                  " or more than", UB_nstud_byclass, "students per class"))
  }

  return(nstud.byclass)
}
