#' @keywords internal
#'
tabrename.nstud <- function(){
  manualdf <- data.frame(
    Input = c("ANNOSCOLASTICO", "CODICESCUOLA", "ORDINESCUOLA", "ANNOCORSO", "FASCIAETA",
              "ALUNNI", "CLASSI", "ALUNNIMASCHI", "ALUNNIFEMMINE",
              "ALUNNICITTADINANZAITALIANA", "ALUNNICITTADINANZANONITALIANA",
              "ALUNNICITTADINANZANONITALIANAPAESIUE", "ALUNNICITTADINANZANONITALIANAPAESINONUE",
              "TIPOPERCORSO", "PERCORSO", "INDIRIZZO", "TEMPOSCUOLA"),
    Output = c("Year", "School_code", "Order", "Grade", "Age_class",
                 "Students", "Classes", "Male_students", "Female_students",
               "ITA_students", "Foreign_students", "EU_Foreign_students", "Non_EU_students",
               "Curriculum_lv1", "Curriculum_lv2", "Curriculum_lv3", "Running_time") )
  return(manualdf)
}
