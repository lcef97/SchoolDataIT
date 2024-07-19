#' @keywords internal
#'
prov.names <- function(){

  res <- data.frame(
    Region_code = c(
      rep(1,6),2, rep(7,4), rep(3,9), rep(4,2), rep(5,7), rep(6,3), rep(8,8), rep(11,4),
      rep(9,9), rep(10,2), rep(12,5), rep(15,5), rep(13,4), 14, rep(16,5), rep(17,2), rep(18,3),
      rep(19,9), rep(20,3), 6, 14, 20, 1, 3, 3, 8, 9, 18, 18, 1, 20, 20, 20, 20, 3, 11, 16, 20),
    Province_code = seq(1:111),
    Region_description = c(
      rep("PIEMONTE",6),"VALLE D'AOSTA", rep("LIGURIA",4), rep("LOMBARDIA", 9),
      rep("TRENTINO ALTO ADIGE",2), rep("VENETO",7), rep("FRIULI VENEZIA GIULIA",3),
      rep("EMILIA ROMAGNA",8), rep("MARCHE",4),rep("TOSCANA",9), rep("UMBRIA",2), rep("LAZIO",5),
      rep("CAMPANIA",5), rep("ABRUZZO",4), "MOLISE", rep("PUGLIA",5), rep("BASILICATA",2), rep("CALABRIA",3),
      rep("SICILIA",9), rep("SARDEGNA",3), "FRIULI VENEZIA GIULIA", "MOLISE", "SARDEGNA",
      "PIEMONTE", "LOMBARDIA", "LOMBARDIA", "EMILIA ROMAGNA", "TOSCANA", "CALABRIA",
      "CALABRIA", "PIEMONTE", "SARDEGNA", "SARDEGNA", "SARDEGNA", "SARDEGNA", "LOMBARDIA",
      "MARCHE", "PUGLIA", "SARDEGNA"),
    Province_initials = c(
      "TO", "VC", "NO", "CN", "AT", "AL", "AO", "IM", "SV", "GE", "SP", "VA", "CO",
      "SO", "MI", "BG", "BS", "PV", "CR", "MN", "BZ", "TN", "VR", "VI", "BL", "TV",
      "VE", "PD", "RO", "UD", "GO", "TS", "PC", "PR", "RE", "MO", "BO", "FE", "RA",
      "FC", "PU", "AN", "MC", "AP", "MS", "LU", "PT", "FI", "LI", "PI", "AR", "SI",
      "GR", "PG", "TR", "VT", "RI", "RM", "LT", "FR", "CE", "BN", "NA", "AV", "SA",
      "AQ", "TE", "PE", "CH", "CB", "FG", "BA", "TA", "BR", "LE", "PZ", "MT", "CS",
      "CZ", "RC", "TP", "PA", "ME", "AG", "CL", "EN", "CT", "RG", "SR", "SS", "NU",
      "CA", "PN", "IS", "OR", "BI", "LC", "LO", "RN", "PO", "KR", "VV", "VB", "OT",
      "OG", "VS", "CI", "MB", "FM", "BT", "SU" ) ,
    Province_description =c(
      "TORINO", "VERCELLI", "NOVARA", "CUNEO", "ASTI", "ALESSANDRIA", "AOSTA", "IMPERIA",
      "SAVONA", "GENOVA", "LA SPEZIA","VARESE","COMO", "SONDRIO", "MILANO", "BERGAMO",
      "BRESCIA", "PAVIA",  "CREMONA", "MANTOVA", "BOLZANO", "TRENTO", "VERONA", "VICENZA",
      "BELLUNO", "TREVISO", "VENEZIA", "PADOVA", "ROVIGO", "UDINE", "GORIZIA", "TRIESTE",
      "PIACENZA", "PARMA", "REGGIO EMILIA", "MODENA", "BOLOGNA", "FERRARA", "RAVENNA",
      "FORLI'-CESENA", "PESARO E URBINO","ANCONA", "MACERATA", "ASCOLI PICENO", "MASSA-CARRARA",
      "LUCCA", "PISTOIA", "FIRENZE", "LIVORNO",  "PISA", "AREZZO", "SIENA", "GROSSETO",
      "PERUGIA", "TERNI", "VITERBO", "RIETI", "ROMA", "LATINA", "FROSINONE", "CASERTA",
      "BENEVENTO", "NAPOLI", "AVELLINO", "SALERNO", "L'AQUILA", "TERAMO", "PESCARA",
      "CHIETI", "CAMPOBASSO", "FOGGIA", "BARI","TARANTO","BRINDISI", "LECCE", "POTENZA",
      "MATERA", "COSENZA", "CATANZARO", "REGGIO CALABRIA",   "TRAPANI", "PALERMO", "MESSINA",
      "AGRIGENTO", "CALTANISSETTA", "ENNA", "CATANIA", "RAGUSA", "SIRACUSA", "SASSARI",
      "NUORO", "CAGLIARI", "PORDENONE", "ISERNIA", "ORISTANO", "BIELLA", "LECCO", "LODI",
      "RIMINI", "PRATO", "CROTONE", "VIBO VALENTIA", "VERBANO-CUSIO-OSSOLA", "OLBIA-TEMPIO",
      "OGLIASTRA", "MEDIO CAMPIDANO", "CARBONIA-IGLESIAS", "MONZA E DELLA BRIANZA", "FERMO",
      "BARLETTA-ANDRIA-TRANI","SUD SARDEGNA" ))
  return(res)
}
