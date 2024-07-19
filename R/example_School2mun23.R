#' Association of the municipality code to a subset of public schools 2022/23
#'
#'
#' This list maps the IDs of the schools from four regions (Molise, Campania, Apulia and Basilicata) to the corresponding LAU codes.
#' The whole dataset can be retrieved with the command \code{Get_School2mun(2023)}
#' @seealso \code{\link{Get_School2mun}}
#'
#' @format ## `example_School2mun23`
#' A list of four elements
#' \itemize{
#'   \item \code{Registry_from_buildings} A data frame of 5527 rows and 5 columns, including the schools listed in the buildings registry.
#'   \item \code{Registry_from_registry} A data frame of 5929 rows and 5 columns, including the schools listed in the schools registry.
#'   \item \code{Any} A data frame of 5954 rows and 5 columns, including schools listed in any of the registryes
#'   \item \code{Both} A data frame of 5510 rows and 5 columns, including schools listed in both registries
#' }
#' For each element, rows correspond to school IDs; the columns are:
#' \itemize{
#'   \item \code{School_code} Character; the school ID.
#'   \item \code{Province_code} Numeric; the NUTS-3 administrative code.
#'   \item \code{Province_initials} Character; abbreviated NUTS-3 denomination.
#'   \item \code{Municipality_code} Character; the ISTAT LAU (municipality) ID.
#'   \item \code{Municipality_description} Character; the municipality name.
#' }
#'
#'
#' @source
#' \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Edilizia+Scolastica&datasetId=DS0101EDIANAGRAFESTA2021}{Buildings registry (2021 onwards)};
#'  \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Edilizia+Scolastica&datasetId=DS0200EDIANAGRAFESTA}{Buindings registry(until 2019)};
#'   \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole}{Schools registry}
#'
"example_School2mun23"
