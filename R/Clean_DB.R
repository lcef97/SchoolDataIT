#' @keywords internal
#'
Clean_DB <- function(data, cutout = NULL, col_cut_thresh = 10^3, IDcol = c(2,5), startcol = 10,
                     pattern.out = c(), init.cutout = c(), track_deleted = TRUE, verbose = TRUE) {

  Nuisance_int <- Nuisance(data, startcol = startcol,
                           missing.pattern = toupper(pattern.out), partial.pattern = "")

  #column cutout

  DB <- data %>% dplyr::select( - rownames(Nuisance_int)[which(
    Nuisance_int$Non_definiti > col_cut_thresh )])
  if(is.null(cutout)){
    cutout <- cutout.default(data, startcol = startcol, init.cutout = init.cutout, pattern.out = pattern.out)
  }
  cols_in <- names(DB)[which(!names(DB) %in% cutout)]
  DB <- DB %>%  dplyr::select(cols_in)

  #row cutout
  deleted <- data.frame(matrix(nrow = 0, ncol = length(IDcol)))
  names(deleted) <- names(data)[IDcol]

  for (j in  c(startcol:ncol(DB))){
    deleted.new <- DB %>% dplyr::filter((!!rlang::sym(names(DB)[j])) %in% pattern.out) %>%
      dplyr::select(IDcol)

    DB <- DB %>% dplyr::anti_join(deleted.new, colnames(deleted.new))

    if (nrow(deleted.new) > 0 & verbose == TRUE){
      cat("deleted", nrow(deleted.new), "units whose field", names(DB)[j], "is missing \n")
    }
    deleted <- rbind(deleted, deleted.new)
  }

  if(track_deleted == TRUE){
    res <- list (data = DB, deleted = deleted)
  } else res <- DB

  return(res)
}
