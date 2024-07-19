#' @keywords internal
#'

fixMun.manual <- function(data, Year){
  pattern <- year.patternB(Year)
  if("Municipality_description" %in% names(data)){
    data$Municipality_description <-
      stringr::str_replace(data$Municipality_description, "\\s*\\(.*$", "")
  }
  if("Province_initials" %in% names(data)){
    data <- data %>% dplyr::mutate(dplyr::across(.data$Province_initials, ~ dplyr::case_when(
      toupper(.data$Municipality_description) == "S. OMOBONO TERME" ~ "BG",
      toupper(.data$Municipality_description) == "PORRETTA TERME" ~ "BO",
      toupper(.data$Municipality_description) == "CORTEOLONA" ~ "PV",
      toupper(.data$Municipality_description) == "ZIBELLO" ~ "PR",
      toupper(.data$Municipality_description) == "MACCAGNO" ~ "VA",
      toupper(.data$Municipality_description) %in% c("VIRGILIO", "BORGOFORTE") ~ "MN",
      toupper(.data$Municipality_description) == "BREMBILLA" ~ "BG",
      toupper(.data$Municipality_description) == "RAMISETO" ~ "RE",
      toupper(.data$Municipality_description) == "BUSANA" ~ "RE",
      toupper(.data$Municipality_description) == "OSSUCCIO" ~ "CO",
      toupper(.data$Municipality_description) == "TREMEZZO" ~ "CO",
      toupper(.data$Municipality_description) == "BELLAGIO" ~ "CO",
      toupper(.data$Municipality_description) == "LESSONA" ~ "BI",
      toupper(.data$Municipality_description) == "MONTESCUDO" ~ "RN",
      toupper(.data$Municipality_description) == "CAMPIGLIA CERVO" ~ "BI",
      toupper(.data$Municipality_description) == "CAVACURTA" ~ "LO",
      toupper(.data$Municipality_description) == "CASTIGLIONE D'INTELVI" ~ "CO",
      toupper(.data$Municipality_description) == "PERGINE VALDARNO" ~ "AR",
      toupper(.data$Municipality_description) == "VILLA POMA" ~ "MN",
      toupper(.data$Municipality_description) == "CASSANO SPINOLA" ~ "AL",
      toupper(.data$Municipality_description) == "ALLUVIONI CAMBIO'" ~ "AL",
      toupper(.data$Municipality_description) == "LATERINA" ~ "AR",
      toupper(.data$Municipality_description) == "REVERE" ~ "MN",
      toupper(.data$Municipality_description) == "VESTRENO" ~ "LC",
      toupper(.data$Municipality_description) == "RIO NELL'ELBA" ~ "LI",
      toupper(.data$Municipality_description) == "SAN FEDELE INTELVI" ~ "CO",
      toupper(.data$Municipality_description) == "RIO MARINA" ~ "LI",
      toupper(.data$Municipality_description) == "PIEVE DI CORIANO" ~ "MN",
      toupper(.data$Municipality_description) == "PECORARA" ~ "PC",
      toupper(.data$Municipality_description) %in% c("CELLIO", "CELLIO (FINO AL 01/01") ~ "VC",
      toupper(.data$Municipality_description) == "NIBBIANO" ~ "PC",
      toupper(.data$Municipality_description) == "FIUMICELLO VILLA VICENTINA" ~ "UD",
      toupper(.data$Municipality_description) == "TREPPO LIGOSULLO" ~ "UD",
      toupper(.data$Municipality_description) == "CASSANO SPINOLA" ~ "AL",
      TRUE ~ .data$Province_initials
    )))
  }
  if("Province_code" %in% names(data)){
    data <- data %>% dplyr::mutate(dplyr::across(.data$Province_code, ~ dplyr::case_when(
      toupper(.data$Municipality_description) == "S. OMOBONO TERME" ~ 16,
      toupper(.data$Municipality_description) == "PORRETTA TERME" ~ 37,
      toupper(.data$Municipality_description) == "CORTEOLONA" ~ 18,
      toupper(.data$Municipality_description) == "ZIBELLO" ~ 34,
      toupper(.data$Municipality_description) == "MACCAGNO" ~ 12,
      toupper(.data$Municipality_description) %in% c("VIRGILIO", "BORGOFORTE") ~ 20,
      toupper(.data$Municipality_description) == "BREMBILLA" ~ 16,
      toupper(.data$Municipality_description) == "RAMISETO" ~ 35,
      toupper(.data$Municipality_description) == "BUSANA" ~ 35,
      toupper(.data$Municipality_description) == "OSSUCCIO" ~ 13,
      toupper(.data$Municipality_description) == "TREMEZZO" ~ 13,
      toupper(.data$Municipality_description) == "BELLAGIO" ~ 13,
      toupper(.data$Municipality_description) == "LESSONA" ~ 96,
      toupper(.data$Municipality_description) == "MONTESCUDO" ~ 99,
      toupper(.data$Municipality_description) == "CAMPIGLIA CERVO" ~ 96,
      toupper(.data$Municipality_description) == "CAVACURTA" ~ 98,
      toupper(.data$Municipality_description) == "CASTIGLIONE D'INTELVI" ~ 13,
      toupper(.data$Municipality_description) == "PERGINE VALDARNO" ~ 51,
      toupper(.data$Municipality_description) == "VILLA POMA" ~ 20,
      toupper(.data$Municipality_description) == "CASSANO SPINOLA" ~ 6,
      toupper(.data$Municipality_description) == "ALLUVIONI CAMBIO'" ~ 6,
      toupper(.data$Municipality_description) == "LATERINA" ~ 51,
      toupper(.data$Municipality_description) == "REVERE" ~ 20,
      toupper(.data$Municipality_description) == "VESTRENO" ~ 97,
      toupper(.data$Municipality_description) == "RIO NELL'ELBA" ~ 49 ,
      toupper(.data$Municipality_description) == "SAN FEDELE INTELVI" ~ 13,
      toupper(.data$Municipality_description) == "RIO MARINA" ~ 49,
      toupper(.data$Municipality_description) == "PIEVE DI CORIANO" ~ 20,
      toupper(.data$Municipality_description) == "PECORARA" ~ 33 ,
      toupper(.data$Municipality_description) %in% c("CELLIO", "CELLIO (FINO AL 01/01") ~ 2,
      toupper(.data$Municipality_description) == "NIBBIANO" ~ 33,
      toupper(.data$Municipality_description) == "FIUMICELLO VILLA VICENTINA" ~ 30,
      toupper(.data$Municipality_description) == "TREPPO LIGOSULLO" ~ 30,
      toupper(.data$Municipality_description) == "CASSANO SPINOLA" ~ 6,
      TRUE ~ .data$Province_code
    )))
  }
  if("Municipality_code" %in% names(data)){
    data <- data %>% dplyr::mutate(dplyr::across(.data$Municipality_code, ~ dplyr::case_when(
      toupper(.data$Municipality_description) == "S. OMOBONO TERME" ~ "016252",
      toupper(.data$Municipality_description) == "PORRETTA TERME" ~ "037062",
      toupper(.data$Municipality_description) == "CORTEOLONA" ~ "018046",
      toupper(.data$Municipality_description) == "ZIBELLO" ~ "034048",
      toupper(.data$Municipality_description) == "MACCAGNO" ~ "012042",
      toupper(.data$Municipality_description) %in% c("VIRGILIO", "BORGOFORTE") ~ "020071",
      toupper(.data$Municipality_description) == "BREMBILLA" ~ "016253",
      toupper(.data$Municipality_description) == "RAMISETO" ~ "035031",
      toupper(.data$Municipality_description) == "BUSANA" ~ "035007",
      toupper(.data$Municipality_description) == "OSSUCCIO" ~ "013257",
      toupper(.data$Municipality_description) == "TREMEZZO" ~ "013252",
      toupper(.data$Municipality_description) == "BELLAGIO" ~ "013250",
      toupper(.data$Municipality_description) == "LESSONA" ~ "096085",
      toupper(.data$Municipality_description) == "MONTESCUDO" ~ "099029",
      toupper(.data$Municipality_description) == "CAMPIGLIA CERVO" ~ "096086",
      toupper(.data$Municipality_description) == "CAVACURTA" ~ "098016",
      toupper(.data$Municipality_description) == "CASTIGLIONE D'INTELVI" ~ "013060",
      toupper(.data$Municipality_description) %in% c("LATERINA", "PERGINE VALDARNO") &
        !any(pattern %in% year.patternB(2016)) ~ "051042",
      toupper(.data$Municipality_description) == "VILLA POMA" ~ "020067",
      toupper(.data$Municipality_description) == "CASSANO SPINOLA" ~ "006191",
      toupper(.data$Municipality_description) == "ALLUVIONI CAMBIO'" ~ "006006",
      toupper(.data$Municipality_description) == "REVERE" ~ "020049",
      toupper(.data$Municipality_description) == "VESTRENO" ~ "097089",
      toupper(.data$Municipality_description) == "RIO NELL'ELBA" ~ "04916",
      toupper(.data$Municipality_description) == "SAN FEDELE INTELVI" ~ "013205",
      toupper(.data$Municipality_description) == "RIO MARINA" ~ "049015",
      toupper(.data$Municipality_description) == "PIEVE DI CORIANO" ~ "020040",
      toupper(.data$Municipality_description) == "PECORARA" ~ "033031",
      toupper(.data$Municipality_description) %in% c("CELLIO", "CELLIO (FINO AL 01/01") ~ "002038",
      toupper(.data$Municipality_description) == "NIBBIANO" ~ "033029",
      toupper(.data$Municipality_description) == "FIUMICELLO VILLA VICENTINA" ~ "030190",
      toupper(.data$Municipality_description) == "TREPPO LIGOSULLO" ~ "030191",
      TRUE ~ .data$Municipality_code
    )))
  }
  return(data)
}

