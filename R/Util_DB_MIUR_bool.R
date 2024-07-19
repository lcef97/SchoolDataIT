#' Clean and convert the raw school buildings data to Boolean variables
#'
#' @description This function cleans the output of the \code{\link{Get_DB_MIUR}} function from missing values in two steps:
#' \itemize{
#' \item First, it deletes both the columns exceeding a threshold of missing values (1000 by default) and the columns that cannot be converted into Boolean variables
#' \item Then, it deletes the rows in which missing values remain
#' }
#'
#'  Finally, the remaining data are converted into Boolean variables.
#'  It is possible to keep track of the deleted rows.
#'
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. Input data obtaned through the function \code{\link{Get_DB_MIUR}}.
#' If \code{NULL} it will be downloaded automatically with the appropriate arguments, but not saved in the global environment. \code{NULL} by default.
#' @param cutout Character. The columns to cut out. If \code{NULL}, it will be determined automatically.
#'  \code{NULL} by default.
#' @param col_cut_thresh Numeric. The threshold of missing values allowed for each variable.
#' If a variable as a higher number of missing observations, then it is cut out. \code{1000} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. TRUE by default.
#' @param track_deleted Logical. If \code{TRUE}, the function returns the names of the school not included in the output dataframe. \code{TRUE} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#' @return If \code{track_deleted == TRUE}, An object of class \code{list} including two objects:
#' \itemize{
#'   \item \code{$data}: object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, the output dataframe. All variables besides the first 8 ones (which identify the record) are numeric.
#'   \item \code{$deleted}: character. The school codes corresponding to deleted rows
#'
#' }
#'
#' If \code{track_deleted == FALSE}, the output is only the first element of the list.
#'
#'
#'
# @examples
#'
#'
#'
# library(magrittr)
# input_DB23_MIUR <- Get_DB_MIUR(2023, verbose = FALSE)
# DB23_MIUR_Bool <- input_DB23_MIUR %>% SchoolDataIT:::Util_DB_MIUR_bool(track_deleted = FALSE)
# DB23_MIUR[,-c(1,4,6,9)]
#'
#'
#'
#' @keywords internal



Util_DB_MIUR_bool <- function(data = NULL, cutout = NULL, col_cut_thresh = 10^3,
                              verbose = TRUE, track_deleted = TRUE, autoAbort = autoAbort, ...){

  starttime <- Sys.time()

  init.cutout = c("Address_type","Address_name","Civic_number", "Building_status")
  pattern.out = c("NON DEFINITO","-", "Informazione assente", "Non Comunicato", "ND", "Non richiesto", "NA")

  while(is.null(data)){
    data <- Get_DB_MIUR(autoAbort = autoAbort, verbose = verbose, ...)
    if(is.null(data)){
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
    }
  }

  if(is.null(cutout)){
    cutout <- cutout.default(data, startcol = 10,
                             init.cutout = init.cutout,
                             pattern.out = pattern.out)
  }

  m <-  sum(names(data)[c(1:9)] %in% init.cutout)
  input <- data %>% Clean_DB(cutout = cutout, col_cut_thresh = col_cut_thresh,
                             IDcol = c(2,5), startcol = 10,
                             init.cutout = init.cutout,
                             pattern.out = pattern.out,
                             track_deleted = track_deleted, verbose = verbose)

  if(track_deleted == TRUE){
    DB <- input$data
  } else DB <- input

  DB_repl <- lapply(DB[,-c(1:(9 - m))], function(x){
    gsub(
      "^SI$", 1, gsub(
        "^NO$", 0, gsub(
          "^Esiste$", 1, ignore.case = TRUE, gsub(
            "^Non Esiste$", 0, ignore.case = TRUE, gsub(
              "^IN PARTE$", 1, ignore.case = TRUE, gsub(
                "^ND$", 0, ignore.case = TRUE, gsub(
                  "^Non Definito$", 0, ignore.case = TRUE, gsub(
                    "^Non Comunicato$", 0, ignore.case = TRUE, gsub(
                      "^Non Richiesto$", 0, ignore.case = TRUE, gsub(
                        "^-$", 0 , gsub("^NA$", 0 , x)))))))))))})
  DB_repl <- lapply(DB_repl,  FUN=as.numeric ) %>% as.data.frame()
  DB[-c(1:(9 - m))] <- DB_repl


  DB <- DB %>%
    dplyr::mutate(Province_code = as.numeric(substr(
      .data$Municipality_code, 1, nchar(.data$Municipality_code)-3)) ) %>%
    dplyr::relocate(.data$Province_code, .after = "Municipality_description")

  if(track_deleted == TRUE){
    res <- list(data = DB, deleted = input$deleted)
  } else res <- DB

  endtime <- Sys.time()
  if(verbose){
    cat("Total running time needed to convert data to Boolean variables:",
        round(difftime(endtime, starttime, units="secs") ,2), "seconds \n"  )
  }

  return(res)
}
