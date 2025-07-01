# -----------------------------------------------------------------------------
#SET UP BEGIN
# -----------------------------------------------------------------------------
#Log set up
datum <- format(Sys.Date(), "%Y-%m-%d")
sink(paste0("voeg_leeftijden_toe_logfile_", 
            datum, ".txt"))
print("Log begonnen")

#Inladen utils
source("utils.R")

#Jaren instellen (staat ook in utils)
jaren <- (2017:2023)
#File path instellen
file_path <- "mypath"

# -----------------------------------------------------------------------------
#LEEFTIJD FUNCTIE
# -----------------------------------------------------------------------------
leeftijd_opbouw <- function(jaren, file_path){

  #--------------------
  #LOOP VOOR ELK JAAR
  #--------------------
  for(year in jaren){
    cat("Begin met verwerken jaar", year, "\n")
    
    cat("Inlezen BEVOLKINGSOPBOUW", year, "\n")
    #Lijst files in BEVOLKINGSOPBOUW
    bevolkingsopbouw_locatie <- list.files("temp/Bevolkingsopbouw", 
                                           pattern = "\\.csv$", recursive = TRUE, 
                                           full.names = TRUE)
    #Relevante jaar in naam BEVOLKINGSOPBOUW selecteren
    bevolkingsopbouw_locatie <- bevolkingsopbouw_locatie[str_detect(bevolkingsopbouw_locatie, 
                                                                    as.character(year))]
    #Error als geen file voor jaar in kwestie
    if(length(bevolkingsopbouw_locatie) == 0){
      stop(paste("Error: Geen BEVOLKINGSOPBOUW voor", year, "gevonden"))
    }
    #RIN kolommen groeperen zodat de nullen niet wegvallen met inlezen
    kolommen_met_nullen <- c("RINPERSOON", "RINPERSOONMa",
                             "RINPERSOONpa")
    #Inlezen BEVOLKINGSOPBOUW met RIN kolommen als character
    bevolkingsopbouw <- fread(bevolkingsopbouw_locatie, sep = ";",
                              header = TRUE, stringsAsFactors = FALSE,
                              colClasses = setNames(rep("character", 
                                                        length(kolommen_met_nullen)), 
                                                    kolommen_met_nullen))
    #BEVOLKINGSOPBOUW als data table
    bevolkingsopbouw <- as.data.table(bevolkingsopbouw)

    #--------------------
    #GBAPERSOONTAB
    #--------------------
    #Inlezen GBAPERSOONTAB
    cat("Inlezen GBAPERSOONTAB", year, "\n")
    #Lijst files in GBAPERSOONTAB
    gbapersoon_locatie <- list.files("G:/Bevolking/GBAPERSOONTAB", pattern = ".sav", 
                                     recursive = TRUE, full.names = TRUE)
    #Relevante jaar in naam GBAPERSOONTAB selecteren
    gbapersoon_locatie <- gbapersoon_locatie[str_detect(gbapersoon_locatie, 
                                                            as.character(year))]
    #Error als geen file voor jaar in kwestie
    if(length(gbapersoon_locatie) == 0){
      stop(paste("Error: Geen GBAPERSOONTAB voor", year, "gevonden"))
    }
    #Relevante kolommen GBAPERSOONTAB inlezen
    gbapersoon <- read_spss(gbapersoon_locatie, col_select = c(RINPERSOONS, 
                                                               RINPERSOON, 
                                                               GBAGEBOORTEJAAR, 
                                                               GBAGEBOORTEMAAND, 
                                                               GBAGEBOORTEDAG,
                                                               GBAGESLACHT))
    #GBAPERSOONTAB als data table
    gbapersoon <- as.data.table(gbapersoon)
    #Geboortedatumvariabelen combineren tot één datum
    gbapersoon[, ':=' ("geboortedatum" = as.Date(str_c(GBAGEBOORTEJAAR, 
                                                       GBAGEBOORTEMAAND, 
                                                       GBAGEBOORTEDAG),
                                                 format = "%Y%m%d"))]
    #Weghalen onnodige kolommen
    gbapersoon[, c("GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG") := NULL]
    
    #--------------------
    #GEBOORTEDATA TOEVOEGEN
    #--------------------
    cat("Geboortedata toevoegen", year, "\n")
    #Bevolkingsopbouw DT splitsen in dg en gb
    #Splitsing dg
    dgbevolkingsopbouw <- bevolkingsopbouw[RINPERSOONS == "D"] 
    #Hernoemen dggeboortedatum kolom naar geboortedatum
    setnames(dgbevolkingsopbouw, "dggeboortedatum", "geboortedatum")
    #Splitsing gb
    gbevolkingsopbouw <- bevolkingsopbouw[RINPERSOONS == "R"]
    #Mergen met gbapersoon op basis van RINS en RIN
    gbevolkingsopbouw <- merge(gbevolkingsopbouw, gbapersoon,
                               by = c("RINPERSOONS", "RINPERSOON"),
                               all.x = TRUE)
    #dggeboortedatum kolom weghalen
    gbevolkingsopbouw[, dggeboortedatum := NULL]
    #dg en gb weer combineren --> nu één geboortedatum kolom met alle geboortedata
    leeftijd <- rbindlist(list(dgbevolkingsopbouw, gbevolkingsopbouw),
                          use.names = TRUE, fill = TRUE)
    

    #Geboortedatum vader toevoegen vanuit GBAPERSOONTAB
    leeftijd <- leeftijd %>% 
      left_join(leeftijd %>% 
                  select(RINPERSOONS, RINPERSOON, geboortedatum) %>%
                  rename(RINPERSOONSpa = RINPERSOONS, RINPERSOONpa = RINPERSOON, 
                         gb_vader = geboortedatum),
                by = c("RINPERSOONSpa", "RINPERSOONpa"),
                suffix = c("", "")
      )
    
    #Geboortedatum moeder toevoegen vanuit GBAPERSOONTAB
    leeftijd <- leeftijd %>% 
      left_join(leeftijd %>% 
                  select(RINPERSOONS, RINPERSOON, geboortedatum) %>%
                  rename(RINPERSOONSMa = RINPERSOONS, RINPERSOONMa = RINPERSOON, 
                         gb_moeder = geboortedatum),
                by = c("RINPERSOONSMa", "RINPERSOONMa"),
                suffix = c("", "")
      )
    
    #--------------------
    #LEEFTIJDEN TOEVOEGEN
    #--------------------
    cat("Leeftijd bij bevalling toevoegen", year, "\n")
    #Leeftijden uitrekenen
    leeftijd[, ':=' (
      "leeftijd" = lft_functie(as.Date(paste("1", "12", jaar, sep = "-")), geboortedatum), 
      #1-12 ipv 31-12 omdat alle andere data op eerste vd maand staan
      "lft_bevalling_vader" = lft_functie(gb_vader, geboortedatum),
      "lft_bevalling_moeder" = lft_functie(gb_moeder, geboortedatum)
    )]
    
    #--------------------
    #NA'S FORMATTEN
    #--------------------
    cat("NA's in gelijk format zetten", year, "\n")
    leeftijd[, (names(leeftijd)) := lapply(.SD, function(x) fifelse(x %in% c("", "---------"),
                                                                      NA, x))]
   
    #--------------------
    #EXPORT
    #--------------------
    cat("Export uitdraaien", year, "\n")
    
    #Naam en locatie export csv definieren
    output_leeftijd_csv <- file.path(file_path, paste0("Bevolkingsopbouw met leeftijden_",
                                                        as.character(year), ".csv"))
    
    #Gebruikte files als comment definieren
    gebruikte_files <- paste0("# Gebruikte files: ", basename(bevolkingsopbouw_locatie),
                              ", ", basename(gbapersoon_locatie), "\n")
    
    #Gebruikte files comment toevoegen
    writeLines(gebruikte_files, output_leeftijd_csv)
    
    #Als csv exporteren
    fwrite(leeftijd, output_leeftijd_csv, append = TRUE, 
           sep = ";", col.names = TRUE)
    
    
    cat("Einde verwerking", year, "\n")
  }
  cat("Alle relevante jaren verwerkt", "\n")
}

# -----------------------------------------------------------------------------
#SET UP EINDE
# -----------------------------------------------------------------------------
#Leeftijd_opbouw runnen
leeftijd_opbouw(jaren, file_path)

#Log stoppen
sink()
    