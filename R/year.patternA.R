#' @keywords internal
#'

year.patternA <- function(Year){
  if(!grepl("\\W", as.character(Year))){
    if(1900 <= as.numeric(Year) & as.numeric(Year) <= 2100){
      Year.num <- as.numeric(Year)
    } else if(190001 <= as.numeric(Year) & as.numeric(Year) <= 209900) {
      Year.num <- as.numeric(Year) %/% 100 + 1
    } else if(19001901 <= as.numeric(Year) & as.numeric(Year) <= 20992100){
      Year.num <- as.numeric(Year) %% 10000
    }
  } else if(length(strsplit(Year, split = "/")[[1]]) == 2L){
    y1 <- as.numeric(strsplit(Year, split = "/")[[1]][1])
    y2 <- as.numeric(strsplit(Year, split = "/")[[1]][2])
    if(y2 == y1 + 1) Year.num <- y2
    else Year.num <- y2 %% 100 + 100*y1%/% 100
  }

  res <- paste0((Year.num-1), Year.num%%100)
  return(res)
}

#year.patternA  <- function(Year){

#  Year <- as.character(Year)
#  pattern <- dplyr::case_when(
#    Year %in% c("2016", "201516", "2015/16", "2015/2016")  ~ "201516",
#    Year %in% c("2017", "201617", "2016/17", "2016/2017")  ~ "201617",
#    Year %in% c("2018", "201718", "2017/18", "2017/2018")  ~ "201718",
#    Year %in% c("2019", "201819", "2018/19", "2018/2019")  ~ "201819",
#    Year %in% c("2020", "201920", "2019/20", "2019/2020")  ~ "201920",
#    Year %in% c("2021", "202021", "2020/21", "2020/2021")  ~ "202021",
#    Year %in% c("2022", "202122", "2021/22", "2021/2022")  ~ "202122",
#    Year %in% c("2023", "202223", "2022/23", "2022/2023")  ~ "202223",
#    Year %in% c("2024", "202324", "2023/24", "2023/2024")  ~ "202324",
#    Year %in% c("2024", "202324", "2023/24", "2023/2024")  ~ "202324",
#    Year %in% c("2025", "202425", "2024/25", "2024/2025")  ~ "202425",
#    Year %in% c("2026", "202526", "2025/26", "2025/2026")  ~ "202526"
#  )
#  return(pattern)
#}
