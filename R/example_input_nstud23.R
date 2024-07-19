#' Subset of the students and classes counts in school year 2022/23
#'
#'
#' This dataframe includes students and classes counts for the schools from four regions: Molise, Campania, Apulia and Basilicata.
#' The whole dataset can be retrieved with the command \code{Get_nstud(2023, filename = "ALUCORSOINDCLASTA")}
#' @seealso \code{\link{Get_nstud}}
#'
#' @format ## `example_input_nstud23`
#' A data frame with 21208 rows and 7 columns:
#' \itemize{
#'   \item \code{Year} Numeric; the school year.
#'   \item \code{School_code} Character; the school ID.
#'   \item \code{Order} Character; the school order, either primary, middle or high school.
#'   \item \code{Grade} Numeric; the school grade.
#'   \item \code{Classes} Numeric; the count of classes of a given grade in each school
#'   \item \code{Male_students} Numeric; the count of male students in all classes of a given educational grade in each school
#'   \item \code{Female_students} Numeric; the count of female students in all classes of a given educational grade in each school
#' }
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Studenti&datasetId=DS0030ALUCORSOINDCLASTA}{Specific link}
#'
"example_input_nstud23"
