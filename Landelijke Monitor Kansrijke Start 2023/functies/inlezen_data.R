#Functie voor inlezen data
#locatie = string met de map waar de data staat (bv. "G:/Bevolking/KINDOUDERTAB")
#kolommen = string met relevante kolommen (tidy, bv. c(c(RINPERSOONSHKW, RINPERSOONHKW, INHP100HGEST, INHARMLAG, INHAHL))
#Als alle kolommen meegenomen moeten worden, selecteer dan everything()
#Vergeet niet dat jaren al geselecteed en beschikbaar moet zijn
inlezen_data <- function(locatie, kolommen, specific = "") {
  #Bepalen locaties laatste dataset
  data_locatie <- list.files(locatie, pattern = ".sav|.SAV", recursive = TRUE, full.names = TRUE)
  
  #Jaren filteren
  if (specific == "schuld") {
    data_locatie <- data_locatie[grep(paste(jaren, collapse = "|"), data_locatie)]
    data_locatie <- data_locatie[2:length(data_locatie)]
  } else if (specific == "zvw") {
    data_locatie <- data_locatie[grep(paste(c(jaren[1] - 2, jaren[1] - 1, jaren), collapse = "|"), data_locatie)]
  } else {
    data_locatie <- data_locatie[grep(paste(jaren, collapse = "|"), data_locatie)]
  }
  
  
  #unieke jaren selecteren
  data_locatie <- as.data.table(data_locatie) #omzetten naar data.table om te kunnen filteren
  data_locatie[, "jaar" := str_extract(basename(data_locatie), "\\d{4}")] #jaar kolom maken
  data_locatie <- unique(data_locatie, fromLast = TRUE, by = "jaar") #laatste versie van elk jaar pakken
  data_locatie <- list(data_locatie$data_locatie)
  
  #printen ter controle
  print(data_locatie[[1]])
  
  #Inlezen relevante kolommen
  if (specific == "zvw") {
    data_list <- lapply(data_locatie[[1]], read_spss, col_select = starts_with(c("rin", "zvw", "NOPZVWKHUI")))
  } else {
    data_list <- lapply(data_locatie[[1]], read_spss, col_select = all_of(kolommen))
  }
  
  #specifieke aanpassingen voor datasets
  if(specific == "opleiding") {
    data_list <- lapply(data_list, function(x) {
      x$OPLNRHB <- as.character(x$OPLNRHB)
      return(x)
    })
  }
  
  #Datasets combineren tot 1 lange set
  if (specific == "zvw") {
    data <- rbindlist(data_list, idcol = "jaar", fill = TRUE)
  } else {
    data <- rbindlist(data_list, idcol = "jaar")
  }
  
  rm(data_list)
  
  #Toevoegen jaar in id
  data[, jaar := factor(jaar, labels = str_extract(basename(data_locatie[[1]]), "\\d{4}"))]
  
  data[, jaar := as.numeric(as.character(jaar))]
  
  return(data)
}