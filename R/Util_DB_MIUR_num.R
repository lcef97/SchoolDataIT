#' Convert the raw school buildings data to numeric or Boolean variables
#'
#' @description This function transforms the output variables of the \code{\link{Get_DB_MIUR}} into Boolean or Numeric.
#' Additionally, it removes the columns with an excessive number of missing observations (20.000 by default), and if required it may also delete the rows including missing fields.
#'  In this case, it is possible to keep track of the deleted rows.
#'
#' @param data Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. Input data obtaned through the function \code{\link{Get_DB_MIUR}}.
#' If \code{NULL} it will be downloaded automatically with the appropriate arguments, but not saved in the global environment. \code{NULL} by default.
#' @param include_numerics Logical. Whether to include strictly numeric variables alongside with Boolean ones. \code{TRUE} by default.
#' @param include_qualitatives Logical. Whether to include qualitative variables alongside with Boolean ones. \code{FALSE} by default.
#' @param row_cutout Logical. Whether to filter out rows including missing fields. \code{FALSE} by default.
#' @param col_cut_thresh Numeric. The threshold of missing values allowed for each variable.
#' If a variable as a higher number of missing observations, then it is cut out. \code{20.000} by default.
#' Warning: if the option \code{row_cutout} is active, please select a lower threshold (e.g. \code{1000})
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. TRUE by default.
#' @param track_deleted Logical. If \code{TRUE}, the function returns the names of the school not included in the output dataframe. \code{TRUE} by default.
#' @param flag_outliers Logical. Whether to assign NA to outliers in numeric variables. \code{TRUE} by default.
#' @param autoAbort Logical. In case any data must be retrieved, whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#' @param ... Additional arguments to the function \code{Get_DB_MIUR} if \code{data} is not provided.
#'
#' @details The outliers to be set to \code{NA} if \code{flag_outliers} is active are defined as follows: School area or free area surface of less than 50 squared meters,
#' building volume of less than 150 cubic meters, 0 floors in the building.
#'
#'
#'
#' @return If \code{track_deleted == TRUE}, An object of class \code{list} including two objects:
#' \itemize{
#'   \item \code{$data}: object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, the output dataframe.
#'   \item \code{$deleted}: object of class \code{tbl_df}, \code{tbl} and \code{data.frame}. The school IDs of the deleted units.
#' }
#'
#' If \code{track_deleted == FALSE}, the output is only the first element of the list.
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' library(magrittr)
#'
#' DB23_MIUR_num <- example_input_DB23_MIUR %>% Util_DB_MIUR_num(track_deleted = FALSE)
#'
#'
#' DB23_MIUR_num[, -c(1,4,6,8,9,10)]
#' summary(DB23_MIUR_num)
#'
#'
#' @export

Util_DB_MIUR_num <- function(data = NULL, include_numerics = TRUE, include_qualitatives = FALSE,
         row_cutout = FALSE, track_deleted = TRUE, verbose = TRUE, col_cut_thresh = 2e+4,
         flag_outliers = TRUE, autoAbort = FALSE, ...){

  starttime <- Sys.time()

  while(is.null(data)) {
    data <- Get_DB_MIUR(autoAbort = autoAbort, verbose = verbose, ...)
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
  y <- as.numeric(data$Year[1])%/%100+1
  startcol <- 10
  uncomma <- function(c, na.rm = FALSE){
    if(na.rm){
      c <- c[which(!is.na(c))]
    }
    c <- stringr::str_remove_all(c, "\\.")
    c <- stringr::str_replace_all(c, ",", ".")
  }
  nc <- ncol(data)
  #init.cutout = c("Address_type","Address_name","Civic_number", "Building_status")
  pattern.out = c("NON DEFINITO","-", "Informazione assente", "Non Comunicato", "ND", "Non richiesto", "IN PARTE")

  starttime <- Sys.time()
  if("Adaptation_year" %in% names(data)){
    data <- data %>% dplyr::mutate(
      Adaptation_year = y - suppressWarnings(as.numeric(.data$Adaptation_year))) %>%
      dplyr::rename(Adaptation_age = .data$Adaptation_year)
  }
  if("Building_year" %in% names(data)){
    data <- data %>% dplyr::mutate(
      Building_year = y - suppressWarnings(as.numeric(.data$Building_year))) %>%
      dplyr::rename(Building_age = .data$Building_year)
  }

  booleans <- c()
  numerics <- c()
  qualitatives <- c()

  for(j in (startcol:nc)){
    C <- unlist(unique(data[which(! data[,j][[1]] %in% pattern.out),j]))
    if(all(toupper(C[which(!is.na(C))]) %in% c ("SI", "NO", "ESISTE", "NON ESISTE"))){
      booleans <- c(booleans,j)
    } else if(all(is.na(suppressWarnings(as.numeric(uncomma(C)))))){
      qualitatives <- c(qualitatives, j)
    } else {
      numerics <- c(numerics, j)
    }
  }

  if("National_seismic_classification" %in% names(data)){
    SC.idx <- which(names(data) == "National_seismic_classification")
    numerics <- numerics[which(numerics != SC.idx)]
    qualitatives <- c(qualitatives, SC.idx)
  }

  if(verbose) cat("Converting dichotomic variables to Boolean:\n")
  dat.bool <- lapply(data[, booleans], function(x){
    suppressWarnings(as.numeric(gsub(
      "^SI$", 1, gsub(
        "^NO$", 0, gsub(
          "Esiste", 1, ignore.case = TRUE, gsub(
            "Non Esiste", 0 , ignore.case = TRUE, x))))))}) %>%
    as.data.frame()

  if(include_numerics){
    if(verbose) cat("Setting numeric variables: \n")
    dat.num <- lapply(data[, numerics], function(x){
      suppressWarnings(as.numeric(uncomma(x)))
    }) %>% as.data.frame()
    if(flag_outliers){
      if("School_area_surface" %in% names(dat.num)){
        dat.num$School_area_surface[which(dat.num$School_area_surface < 50)] <- NA
      }
      if("Free_area_surface" %in% names(dat.num)){
        dat.num$Free_area_surface[which(dat.num$Free_area_surface < 50)] <- NA
      }
      if("Gross_building_volume" %in% names(dat.num)){
        dat.num$Gross_building_volume[which(dat.num$Gross_building_volume < 150)] <- NA
      }
      if("Floors_number" %in% names(dat.num)){
        dat.num$Floors_number[which(dat.num$Floors_number == 0)] <- NA
      }
      #if("Building_age" %in% names(dat.num)){
      #  dat.num$Building_age[which(dat.num$Building_age > 1500)] <- NA
      #}
    }
  }

  if(include_qualitatives){
    if(verbose) cat("Setting qualitative variables: \n")
    dat.qual <- data[, qualitatives] %>% apply(MARGIN = 2, function(x){
      x[which(x %in% pattern.out)] <- NA
      return(x)
    }) %>% as.data.frame()
  }

  DB <- data
  DB[, booleans] <- dat.bool

  if(include_numerics & include_qualitatives){
    DB[, numerics] <- dat.num
    DB[, qualitatives] <- dat.qual
  } else if(include_numerics & !include_qualitatives){
    DB[, numerics] <- dat.num
    DB <- DB[-qualitatives]
  } else if(!include_numerics & include_qualitatives){
    DB[, qualitatives] <- dat.qual
    DB <- DB[-numerics]
  } else DB <- DB[-c(qualitatives, numerics)]

  NAs <- data.frame(
    NAs = apply(DB[startcol:ncol(DB)], MARGIN = 2,function(x){
      sum(is.na(x))}))

  # Column cutout
  DB <- DB %>% dplyr::select( -rownames(
    dplyr::filter(NAs, .data$NAs > col_cut_thresh)))

  #Row cutout
  IDcol <- c(2,5)
  deleted <- data.frame(matrix(nrow = 0, ncol = length(IDcol)))
  if(row_cutout) {

    names(deleted) <- names(data)[IDcol]
    IDcolName <- names(DB)[IDcol]
    for(j in c(startcol:ncol(DB))){
      deleted.new <- DB %>% dplyr::filter(is.na(!!rlang::sym(names(DB)[j]))) %>%
        dplyr::select(IDcol)
      DB <- DB %>% dplyr::anti_join(deleted.new, colnames(deleted.new))
      if (nrow(deleted.new) > 0 & verbose == TRUE){
        cat("deleted", nrow(deleted.new), "units whose field", names(DB)[j], "is missing \n")
      }
      deleted <- rbind(deleted, deleted.new)
    }
  }

  DB <- DB %>%
    dplyr::mutate(Province_code = as.numeric(substr(
      .data$Municipality_code, 1, nchar(.data$Municipality_code)-3)) ) %>%
    dplyr::relocate(.data$Province_code, .after = "Municipality_description")


  if(track_deleted & row_cutout){
    res <- list(data = DB, deleted = deleted)
  } else res <- DB

  endtime <- Sys.time()
  if(verbose){
    cat("Total running time needed to convert data to numeric variables:",
        round(difftime(endtime, starttime, units="secs") ,2), "seconds \n"  )
  }

  return(res)
}






