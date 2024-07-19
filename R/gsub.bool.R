#' @keywords internal
#'

gsub.bool <- function(data, startcol){
  DB_repl <- lapply(data[,-c(1:(startcol - 1))], function(x){
    gsub(
      "SI", 1, gsub(
        "NO", 0, gsub(
          "Esiste", 1, ignore.case = TRUE, gsub(
            "Non Esiste", 0, ignore.case = TRUE, gsub(
              "IN PARTE", 1, ignore.case = TRUE, gsub(
                "ND", 0, ignore.case = TRUE, gsub(
                  "Non Definito", 0, ignore.case = TRUE, gsub(
                    "Non Comunicato", 0, ignore.case = TRUE, gsub(
                      "Non Richiesto", 0, ignore.case = TRUE, gsub(
                        "^-$", 0 , gsub("^NA$", 0 , x)))))))))))})
  DB_repl <- lapply(DB_repl,  FUN=as.numeric ) %>% as.data.frame()
  res <- data
  res[-c(1:(startcol - 1))] <- DB_repl
  return(res)
}



#res <- data
#DB_repl <- lapply(data[,-c(1:(startcol - 1))], function(x){
#  gsub("SI", 1,
#       gsub("NO", 0,
#            gsub("NON DEFINITO",0,
#                 gsub("IN PARTE", 1,
#                      gsub("Esiste", 1,
#                           gsub("Non Esiste", 0 ,
#                                gsub("ND", 0, x)))))))
#}) %>% as.data.frame()
#DB_repl <- lapply(DB_repl,  FUN=as.numeric ) %>% as.data.frame()
#res[-c(1:(startcol - 1))] <- DB_repl
#return(res)
