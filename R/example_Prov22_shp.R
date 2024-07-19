#' Subset of Italian provinces shapefile
#'
#'
#' This is the shapefile for the provinces belonging to four regions: Molise, Campania, Apulia and Basilicata,
#' as of January 1st 2022. These are the latest administrative units boundaries relevant at the beginning of the school year 2022/23.
#' The whole shapefile can be retrieved with the command \code{Get_Shapefile(Year = 2022, level = "NUTS-3")}
#' @seealso \code{\link{Get_Shapefile}}
#'
#' @format ## `example_Prov22_shp`
#' A Spatial polygon data frame with 13 rows/polygons and 15 columns:
#' \itemize{
#'   \item \code{COD_RIP} Numeric; the code for the macroarea (1 for Northwest, 2 for Northeast, 3 for Center, 4 for South and 5 for Isles)
#'   \item \code{COD_REG} Numeric; the region (NUTS-2 administrative level) ID
#'   \item \code{COD_PROV} Numeric; the NUTS-3 administrative code
#'   \item \code{COD_CM} Numeric; the administrative code for Metropolitan Cities (which are always at the NUTS-3 level), obtained as 200 + NUTS-3 code, if the unit is a Metropolitan city; 0 otherwise.
#'   \item \code{COD_UTS} Numeric; the administrative code for Metropolitan cities if the unit is a Metropolitan City; the province code otherwise.
#'   \item \code{DEN_PROV} Character; the province (NUTS-3 administrative level) name, if the unit is not a Metropolitan City; blank otherwise.
#'   \item \code{DEN_CM} Character; the Metropolitan City (NUTS-3 administrative level) name, if the unit is a Metropolitan City; blank otherwise.
#'   \item \code{DEN_UTS} Character; the province or Metropolitan City (NUTS-3 administrative level) name.
#'   \item \code{SIGLA} Character; abbreviated NUTS-3 denomination.
#'   \item \code{TIPO_UTS} Character; the NUTS-3 type of the unit; either "Provincia" (Province) or "Citta metropolitana" (Metropolitan City)
#'   \item \code{Shape_Leng} Numeric; the polygon perimeter.
#'   \item \code{Shape_Area} Numeric; the polygon area.
#'   \item \code{geometry} the polygon geometry.
#'
#' }
#' @source <https://www.istat.it/it/archivio/222527>
"example_Prov22_shp"
