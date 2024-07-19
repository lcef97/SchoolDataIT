#' Subset of the school registry in school year 2022/23
#'
#'
#' This dataframe includes the schools directly identifiable as primary, middle or high school, from four regions: Molise, Campania, Apulia and Basilicata.
#' Only the first 10 columns are included.
#' The whole dataset can be retrieved with the command \code{Get_Registry(2023)}
#' @seealso \code{\link{Get_Registry}}
#'
#' @format ## `example_input_Registry23`
#' A data frame with 5929 rows and 10 columns:
#' \itemize{
#'   \item \code{Year} Numeric; the school year.
#'   \item \code{Area} Character; the macro-area of the municipality, i.e. North, Center or South.
#'   \item \code{Region_description} Character; the region (NUTS-2 administrative level) name.
#'   \item \code{Province_description} Character; the province (NUTS-3 administrative level) name.
#'   \item \code{Reference_institute_code} Character; the ID of the reference institute.
#'   \item \code{School_code} Character; the school ID.
#'   \item \code{Cadastral_code} Character; a LAU - level ID code, different from the official LAU municipality code.
#'                               The Italian Ministry of Education does provide this code in the place of the LAU code for both the Schools registry and the early school buildings DBs.
#'   \item \code{Municipality_description} Character; the municipality name.
#'   \item \code{School_address} Character; the school physical address.
#'   \item \code{Postal_code} Character; the ZIP code, slightly finer than municipality boundaries for big municipalities.

#' }
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Scuole&datasetId=DS0400SCUANAGRAFESTAT}{Source link}
#'
"example_input_Registry23"
