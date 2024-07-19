#' Subset of the Invalsi scores in school year 2022/23
#'
#'
#' This dataframe includes the Invalsi scores of the schools from four regions: Molise, Campania, Apulia and Basilicata, for the school year 2022/23.
#' The whole dataset can be retrieved with the command \code{Get_Invalsi_IS(level = "NUTS-3")}
#' @seealso \code{\link{Get_Invalsi_IS}}
#'
#' @format ## `example_Invalsi23_prov`
#' A data frame with 240 rows and 11 columns:
#' \itemize{
#'   \item \code{Year} Character; the school year.
#'   \item \code{Grade} Numeric; the school grade; only includes the school grades subjected to the Invalsi survey. Either 2, 5, 8, 10 or 13.
#'   \item \code{Subject} Character; the school subject in which the test is taken; either Italian, Mathematics, English reading or English listening.
#'   \item \code{Province_code} Numeric; the NUTS-3 administrative code.
#'   \item \code{Province_initials} Character; abbreviated NUTS-3 denomination.
#'   \item \code{Province_description} Character; the province (NUTS-3 administrative level) denomination.
#'   \item \code{Average_percentage_score} Numeric; the province-level percentage of sufficient tests, only for primary schools; ranges 0-100.
#'   \item \code{Std_dev_percentage_score} Numeric; the standard deviation of the percentage of sufficient tests, only for primary schools.
#'   \item \code{WLE_average_score} Numeric; the province-level average WLE (Weighted Likelihood Estimator) score.
#'   \item \code{Std_dev_WLE_score} Numeric; the standard deviation of WLE scores.
#'   \item \code{Students_coverage} Numeric; the percentage of students for which the Invalsi tests are reported.
#' }
#' @source \href{https://serviziostatistico.invalsi.it/invalsi_ss_data/dati-provinciali-di-popolazione/}{Source page}
#'
#'
#'
"example_Invalsi23_prov"
