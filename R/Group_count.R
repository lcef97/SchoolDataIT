#' @keywords internal
#'
Group_Count <- function(data, groupcol, startgroup, count = TRUE, countname = NULL, FUN){

  options(dplyr.summarise.inform = FALSE)

  startcol <- startgroup - length(groupcol)
  endcol <- ncol(data) - length(groupcol)
  res <- data %>%
    dplyr::group_by_at(groupcol) %>%
    dplyr::summarise(dplyr::across(c(startcol:endcol), list(FUN)), Count = dplyr::n()) %>% dplyr::ungroup()
  res <- res %>% dplyr::relocate(.data$Count, .after = names(res)[length(groupcol)])
  if(count == FALSE){
    res <- res %>% dplyr::select(-.data$Count)
  }
  if(!is.null(countname)){
    names(res)[which(names(res)=="Count")] = countname
  }

  names(res) <- sub("_1$", "", names(res))
  return(res)
}
