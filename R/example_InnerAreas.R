#' Subset of the school registry in school year 2022/23
#'
#'
#' This dataframe includes the classification of municipalities , from four regions: Molise, Campania, Apulia and Basilicata.
#' Only the first 10 columns are included;
#' some strings in field \code{Municipality_description} including accents have been forced to ASCII.
#' The whole dataset can be retrieved with the command \code{Get_InnerAreas()}.
#' For the definition of ISTAT inner areas class, see \code{\link{Get_InnerAreas}}
#'
#' @seealso \code{\link{Get_InnerAreas}}
#'
#' @format ## `example_InnerAreas`
#' A data frame with 1074 rows and 10 columns:
#' \itemize{
#'   \item \code{Municipality_code} Character; the ISTAT LAU (municipality) ID.
#'   \item \code{Municipality_code_numeric} Numeric; the ISTAT LAU (municipality) ID in numeric format.
#'   \item \code{Cadastral_code} Character; a LAU - level ID code, different from the official ISTAT municipality code.
#'   \item \code{Region_code} Numeric; the region (NUTS-2 administrative level) ID
#'   \item \code{Region_description} Character; the region (NUTS-2 administrative level) name.
#'   \item \code{Province_code} Numeric; the NUTS-3 administrative code.
#'   \item \code{Province_initials} Character; abbreviated NUTS-3 denomination.
#'   \item \code{Province_description} Character; the province (NUTS-3 administrative level) denomination.
#'   \item \code{Municipality_description} Character; the municipality name.
#'   \item \code{Inner_area_code_2014_2020} Character; the ISTAT inner areas classification between 2014 and 2020.
#'   \item \code{Inner_area_description_2014_2020} Character; the description of the classes identified in the previous column
#'   \item \code{Inner_area_code_2021_2027} Character; the ISTAT inner areas classification between 2021 and 2027.
#'   \item \code{Inner_area_description_2021_2027} Character; the description of the classes identified in the previous column
#'   \item \code{Destination_municipality_code} Character; For non-central municipalities (classes C, D, E, F), the ID of the closest pole municipality according to the 2021-2027 classification
#'   \item \code{Destination_municipality_code} Character; The denomination of the municipalities in the previous column
#'   \item \code{Destination_pole_code} Character; An internal ID convention for the destination poles; it includes a letter
#'   (the class of the destination pole, either A or B); a number of two digits (the region code of the destination pole) and the progressive number of poles within a region.
#' }
#' @source <https://www.istat.it/it/archivio/273176>
#'
"example_InnerAreas"
