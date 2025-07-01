# -----------------------------------------------------------------------------
#SET UP BEGIN
# -----------------------------------------------------------------------------
#Log set up
datum <- format(Sys.Date(), "%Y-%m-%d")
sink(paste0("Bevolkingsopbouw_logfile_", 
            datum, ".txt"))
print("Log begonnen")

#Inladen utils
source("utils.R")

#Jaren instellen
#NB: ENKEL MOGELIJK VANAF 2015!!
jaren <- (2017:2023)

#File path instellen
folder_path <- "mypath"

# -----------------------------------------------------------------------------
#BEVOLKINGSOPBOUW FUNCTIE
# -----------------------------------------------------------------------------
bevolkings_opbouw <- function(jaren, folder_path){

  #--------------------
  #KINDEROUDERTAB
  #--------------------
  cat("Inlezen KINDOUDERTAB \n")
  #Lijst files in KINDOUDERTAB
  kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav",
                                  recursive = TRUE, full.names = TRUE)
  #Nieuwste KINDOUDERTAB selecteren
  kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]
  #Error als geen file voor jaar in kwestie  
  if(length(kindouder_locatie) == 0){
    stop(paste("Error: Geen KINDOUDERTAB gevonden"))
  }
  #Relevante kolommen KINDOUDERTAB inlezen
  kindouder <- read_sav(kindouder_locatie, col_select = -c(XKOPPELNUMMER))
  #KINDOUDERTAB als data table
  kindouder <- as.data.table(kindouder)
  
  #--------------------
  #LOOP VOOR ELK JAAR
  #--------------------
  for(year in jaren){
    cat("Begin met verwerken jaar", year, "\n")
    
    #--------------------
    #GBASTANDBEVOLKINGTAB - bevat de gemeente en de 4-cijferige postcode op 1 januari van het jaar 
    # en de woongemeente op 31 december voorafgaand aan dat jaar van personen die per 1 januari van 
    # het jaar wettelijk vastgestelde bevolking van Nederland behoren.
    #--------------------
    cat("Inlezen GBASTANDBEVOLKING", year+1, "\n")
    #Lijst files in GBASTANDBEVOLKING
    gbastand_locatie <- list.files("G:/Bevolking/GBASTANDBEVOLKINGTAB", 
                                   pattern = "\\.sav$", recursive = TRUE, 
                                   full.names = TRUE)
    #Relevante jaar in naam GBASTANDBEVOLKINGTAB selecteren
    gbastand_locatie <- gbastand_locatie[str_detect(gbastand_locatie, 
                                                    as.character(year+1))]
    #Error als geen file voor jaar in kwestie
    if(length(gbastand_locatie) == 0){
      stop(paste("Error: Geen GBASTANDBEVOLKING voor", year+1, "gevonden"))
    }
    #Relevante kolommen GBASTANDBEVOLKINGTAB inlezen
    gbastand <- read_sav(gbastand_locatie, col_select = c(RINPERSOONS,
                                                          RINPERSOON,
                                                          GBAGEMEENTEINSCHRIJVING31DECEMBER,
                                                          GBAPOSTCODE4GAAF31DECEMBER))   
    #GBASTANDBEVOLKINGTAB als data table
    gbastand <- as.data.table(gbastand)

    #--------------------
    #DGGBAPERSOONTAB - Doodgeborenen
    #--------------------
    cat("Inlezen DGGBAPERSOONTAB", year, "\n")
    #Lijst files in DGGBAPERSOONTAB
    dggbapersoon_locatie <- list.files("G:/Bevolking/DGGBAPERSOONTAB", 
                                       pattern = "\\.sav$", recursive = TRUE, 
                                       full.names = TRUE)
    #Relevante jaar in naam DGGBAPERSOONTAB selecteren
    dggbapersoon_locatie <- dggbapersoon_locatie[str_detect(dggbapersoon_locatie, 
                                                            as.character(year))]
    #Error als geen file voor jaar in kwestie
    if(length(dggbapersoon_locatie) == 0){
      stop(paste("Error: Geen DGGBAPERSOONTAB voor", year, "gevonden"))
    }
    #Relevante kolommen DGGBAPERSOONTAB inlezen
    dggbapersoon <- read_sav(dggbapersoon_locatie,
                             col_select = c(RINPERSOONS, RINPERSOON,
                                            DGDATUMPARTUS, DGZWANGERSCHAPSDUUR))
    #DGGBAPERSOONTAB als data table
    dggbapersoon <- as.data.table(dggbapersoon)
    #Selectie zwangerschappen vanaf 24 weken
    dggbapersoon <- dggbapersoon[DGZWANGERSCHAPSDUUR >= 24 & DGZWANGERSCHAPSDUUR != 99] 
    #Nieuwe naam geboortedatum kolom in overeenstemming met gbapersoon
    dggbapersoon[, dggeboortedatum := as.IDate(DGDATUMPARTUS, format = "%Y%m%d")] 
    #Onnodige kolommen weghalen
    dggbapersoon[, c("DGDATUMPARTUS", "DGZWANGERSCHAPSDUUR") := NULL]

    #--------------------
    #PERSONEN
    #--------------------
    cat("Combineren GBASTANDBEVOLKING en DGGBAPERSOONTAB", year, "\n")
    #Combineren gbastand en dggbapersoon in personen
    personen <- rbindlist(list(gbastand, dggbapersoon),
                          use.names = TRUE, fill = TRUE)

    #--------------------
    #OUDERS TOEVOEGEN 
    #--------------------
    cat("Ouders toevoegen", year, "\n")
    #Ouders toevoegen in nieuwe data table bevolking
    bevolking <- merge(personen, kindouder, all.x = TRUE,
                       by = c("RINPERSOONS", "RINPERSOON"))
    
    #--------------------
    #NA'S FORMATTEN
    #--------------------
    cat("NA's in gelijk format zetten", year, "\n")
    bevolking[, (names(bevolking)) := lapply(.SD, function(x) fifelse(x %in% c("", "---------"),
                                                                      NA, x))]
    #--------------------
    #GEBRUIKTE FILES EN JAAR TOEVOEGEN
    #--------------------
    cat("Gebruikte files en jaar kolom toevoegen", year, "\n")
    #Jaar kolom toevoegen
    bevolking[, ":="(jaar = year)]
    
    #Gebruikte files als comment definieren
    gebruikte_files <- paste0("# Gebruikte files: ", basename(gbastand_locatie),
                              ", ", basename(dggbapersoon_locatie), ", ",
                              basename(kindouder_locatie), "\n")
    
    #--------------------
    #EXPORT
    #--------------------
    cat("Export uitdraaien", year, "\n")
    #Naam en locatie export csv definieren
    output_bevolking_csv <- file.path(folder_path, paste0("Bevolkingsopbouw_",
                                                       year, ".csv"))
    #Gebruikte files comment toevoegen
    writeLines(gebruikte_files, output_bevolking_csv)
    
    #Als csv exporteren
    bevolking[, c("RINPERSOON", "RINPERSOONMa",
                 "RINPERSOONpa") := lapply(.SD, as.character),
              .SDcols = c("RINPERSOON", "RINPERSOONMa",
                             "RINPERSOONpa")]
    fwrite(bevolking, output_bevolking_csv, append = TRUE, 
           sep = ";", col.names = TRUE)
    
    
    cat("Einde verwerking", year, "\n")
  }
cat("Alle relevante jaren verwerkt", "\n")
}

#Bevolkings_opbouw runnen
bevolkings_opbouw(jaren, folder_path)

# -----------------------------------------------------------------------------
#GEMEENTE-INDELING OMZETTEN TM 2025
# -----------------------------------------------------------------------------
cat("Gemeente-indeling omzetten", "\n")
#Niet in bovenstaande functie verwerkt omdat jaren met elkaar vergelijkt moeten worden
#En anders met CBS datasets in environment: te groot

#--------------------
#FILES LADEN
#--------------------
#Mapping verandering gemeente-codes inladen
cat("Mapping inladen", "\n")
mapping <- fread(file.path(folder_path, "Mapping_gemeente-indeling.csv"),
                     colClasses = "character")

cat("Alle relevante leeftijdfiles inladen", "\n")
bevolkingsopbouw_list <- list()
for(year in jaren){
  file_name <- paste0("Bevolkingsopbouw_", year, ".csv")
  file_path <- file.path(folder_path, file_name)
  
  if(file.exists(file_path)) {
    bevolkingsopbouw <-  fread(file_path, 
                          colClasses = list(character = c("RINPERSOONS", 
                                                          "RINPERSOON",
                                                          "GBAGEMEENTEINSCHRIJVING31DECEMBER",
                                                          "GBAPOSTCODE4GAAF31DECEMBER",
                                                          "RINPERSOONSpa", 
                                                          "RINPERSOONpa",
                                                          "RINPERSOONSMa", 
                                                          "RINPERSOONMa")))
    
    bevolkingsopbouw[, GBAGEMEENTEINSCHRIJVING31DECEMBER := mapping$gem2023[match(GBAGEMEENTEINSCHRIJVING31DECEMBER, mapping$gem)]]
    
    setnames(bevolkingsopbouw, old = c("GBAGEMEENTEINSCHRIJVING31DECEMBER",
                                       "GBAPOSTCODE4GAAF31DECEMBER"),
             new = c(paste0("gem", year), paste0("PC4", year)))

    bevolkingsopbouw_list[[length(bevolkingsopbouw_list)+1]] <- bevolkingsopbouw
    
    cat("Geladen voor:", year, "\n")
  } else {
    cat("File not found:", file_name, "\n")
  }
}

bevolkingsopbouw_jaren <- rbindlist(bevolkingsopbouw_list, use.names = TRUE,
                                    fill = TRUE)
#--------------------
#GESPLITSTE VERANDERINGEN
#--------------------
#veranderingen voor gemeenten die gesplitst zijn
cat("Gesplitste veranderingen doorvoeren")

#--------------------
#VERANDERINGEN 2018
#--------------------
if (all(c(2017,2018) %in% jaren)){
  cat("Gesplitste veranderingen 1 jan 2018 doorvoeren")
  #GM: 1949
  bevolkingsopbouw_jaren[gem2017 == "0140" & gem2018 == "1949", gem2017 := "1949"]
  #GM: 0080
  bevolkingsopbouw_jaren[gem2017 == "0140" & gem2018 == "0080", gem2017 := "0080"]
  #GM: 1900
  bevolkingsopbouw_jaren[gem2017 == "0140" & gem2018 == "1900", gem2017 := "1900"]
  #PC4: 8635
  bevolkingsopbouw_jaren[PC42017 == "8635", gem2017 := "1900"]
  #PC4: 8637
  bevolkingsopbouw_jaren[PC42017 == "8637", gem2017 := "1900"]
  #PC4: 8641
  bevolkingsopbouw_jaren[PC42017 == "8641", gem2017 := "1900"]
  #PC4: 8731
  bevolkingsopbouw_jaren[PC42017 == "8731", gem2017 := "1900"]
  #PC4: 8732
  bevolkingsopbouw_jaren[PC42017 == "8732", gem2017 := "1900"]
  #PC4: 8733
  bevolkingsopbouw_jaren[PC42017 == "8733", gem2017 := "1900"]
  #PC4: 8734
  bevolkingsopbouw_jaren[PC42017 == "8734", gem2017 := "1900"]
  #PC4: 8735
  bevolkingsopbouw_jaren[PC42017 == "8735", gem2017 := "1900"]
  #PC4: 8736
  bevolkingsopbouw_jaren[PC42017 == "8736", gem2017 := "1900"]
  #PC4: 8737
  bevolkingsopbouw_jaren[PC42017 == "8737", gem2017 := "1900"]
  #PC4: 8831
  bevolkingsopbouw_jaren[PC42017 == "8831", gem2017 := "1949"]
  #PC4: 8832
  bevolkingsopbouw_jaren[PC42017 == "8832", gem2017 := "0080"]
  #PC4: 8834
  bevolkingsopbouw_jaren[PC42017 == "8834", gem2017 := "0080"]
  #PC4: 8835
  bevolkingsopbouw_jaren[PC42017 == "8835", gem2017 := "0080"]
  #PC4: 8841
  bevolkingsopbouw_jaren[PC42017 == "8841", gem2017 := "1949"]
  #PC4: 8842
  bevolkingsopbouw_jaren[PC42017 == "8842", gem2017 := "1949"]
  #PC4: 8843
  bevolkingsopbouw_jaren[PC42017 == "8843", gem2017 := "1949"]
  #PC4: 8844
  bevolkingsopbouw_jaren[PC42017 == "8844", gem2017 := "1900"]
  #PC4: 8845
  bevolkingsopbouw_jaren[PC42017 == "8845", gem2017 := "1900"]
  #PC4: 9021
  bevolkingsopbouw_jaren[PC42017 == "9021", gem2017 := "1900"]
  #PC4: 9022
  bevolkingsopbouw_jaren[PC42017 == "9022", gem2017 := "0080"]
  #PC4: 9023
  bevolkingsopbouw_jaren[PC42017 == "9023", gem2017 := "0080"]
  #PC4: 9024
  bevolkingsopbouw_jaren[PC42017 == "9024", gem2017 := "0080"]
  #PC4: 9025
  bevolkingsopbouw_jaren[PC42017 == "9025", gem2017 := "0080"]
  #PC4: 9026
  bevolkingsopbouw_jaren[PC42017 == "9026", gem2017 := "0080"]
  #PC4: 9027
  bevolkingsopbouw_jaren[PC42017 == "9027", gem2017 := "0080"]
  
  #--------------------
  #CHECK
  #--------------------
  if(any(sapply(bevolkingsopbouw_jaren[, grep("^gem", 
                                              names(bevolkingsopbouw_jaren),
                                              value = TRUE),
                                       with = FALSE],
                function(col) any(col == "0140", na.rm = TRUE)))){
    stop("Niet alle GM0140 verwijderd")
  }
}

#--------------------
#VERANDERINGEN 2019
#--------------------
if (2019 %in% jaren && any(c(2017,2018) %in% jaren)){
  cat("Gesplitste veranderingen 1 jan 2019 doorvoeren")
  #voor jaren die we hier hebben geselecteerd en in jaren hebben
  pre2019_jaren <- intersect(jaren, c(2017,2018))
  
  #for loop voor alle gesplitste veranderingen voor deze jaren
  for(year in pre2019_jaren){
    gem_jaar <- paste0("gem", year)
    PC4_jaar <- paste0("PC4", year)
    
    #GM: 1966
    bevolkingsopbouw_jaren[get("gem_jaar") == "0053" & get("gem2019") == "1966", 
                           get("gem_jaar") := "1966"]
    #GM: 1969
    bevolkingsopbouw_jaren[get("gem_jaar") == "0053" & get("gem2019") == "1969", 
                           get("gem_jaar") := "1969"]
    #PC4: 9771
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9771", get("gem_jaar") := "1966"]
    #PC4: 9773
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9773", get("gem_jaar") := "1966"]
    #PC4: 9774
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9774", get("gem_jaar") := "1966"]
    #PC4: 9891
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9891", get("gem_jaar") := "1969"]
    #PC4: 9892
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9892", get("gem_jaar") := "1969"]
    #PC4: 9893
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9893", get("gem_jaar") := "1969"]
    #PC4: 9951
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9951", get("gem_jaar") := "1966"]
    #PC4: 9953
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9953", get("gem_jaar") := "1966"]
    #PC4: 9954
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9954", get("gem_jaar") := "1966"]
    #PC4: 9955
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9955", get("gem_jaar") := "1966"]
    #PC4: 9956
    bevolkingsopbouw_jaren[get("PC4_jaar") == "9956", get("gem_jaar") := "1966"]
    
  }
  
    #--------------------
    #CHECK
    #--------------------
    if(any(sapply(bevolkingsopbouw_jaren[, grep("^gem", 
                                                names(bevolkingsopbouw_jaren),
                                                value = TRUE),
                                         with = FALSE],
                  function(col) any(col == "0053", na.rm = TRUE)))) {
      stop("Niet alle GM0053 verwijderd")
    }
  }

#--------------------
#VERANDERINGEN 2020
#--------------------
if (2020 %in% jaren && any(c(2017,2018,2019) %in% jaren)){
  cat("Gesplitste veranderingen 1 jan 2020 doorvoeren")
  #voor jaren die we hier hebben geselecteerd en in jaren hebben
  pre2020_jaren <- intersect(jaren, c(2017,2018,2019))
  
  #for loop voor alle gesplitste veranderingen voor deze jaren
  for(year in pre2020_jaren){
    gem_jaar <- paste0("gem", year)
    PC4_jaar <- paste0("PC4", year)
    
    #GM:0824
    bevolkingsopbouw_jaren[get("gem_jaar") == "0788" & get("gem2020") == "0824", 
                           get("gem_jaar") := "0824"]
    #GM:0865
    bevolkingsopbouw_jaren[get("gem_jaar") == "0788" & get("gem2020") == "0865", 
                           get("gem_jaar") := "0865"]
    #GM:0757
    bevolkingsopbouw_jaren[get("gem_jaar") == "0788" & get("gem2020") == "0757", 
                           get("gem_jaar") := "0757"]
    #GM:0855
    bevolkingsopbouw_jaren[get("gem_jaar") == "0788" & get("gem2020") == "0855", 
                           get("gem_jaar") := "0855"]
    #PC4: 5074
    bevolkingsopbouw_jaren[get("PC4_jaar") == "5074", get("gem_jaar") := "0855"]
    #PC4: 5076
    bevolkingsopbouw_jaren[get("PC4_jaar") == "5076", get("gem_jaar") := "0855"]
    #PC4: 5262
    bevolkingsopbouw_jaren[get("PC4_jaar") == "5262", get("gem_jaar") := "0865"]
    #PC4: 5268
    bevolkingsopbouw_jaren[get("PC4_jaar") == "5268", get("gem_jaar") := "0855"]
    #PC4: 5296
    bevolkingsopbouw_jaren[get("PC4_jaar") == "5296", get("gem_jaar") := "0865"]
    
  }
  
  #--------------------
  #CHECK
  #--------------------
  if(any(sapply(bevolkingsopbouw_jaren[, grep("^gem", 
                                              names(bevolkingsopbouw_jaren),
                                              value = TRUE),
                                       with = FALSE],
                function(col) any(col == "0788", na.rm = TRUE)))) {
    stop("Niet alle GM0788 verwijderd")
  }
}

#--------------------
#EXPORT
#--------------------
#Export per jaar
export_per_jaar <- function(bevolkingsopbouw_jaren, jaren, folder_path){
  #gecombineerde dataset opsplitsen per jaar
  bevolkingsopbouw_split <- split(bevolkingsopbouw_jaren, by = "jaar")
  
  #loop voor elk apart jaar in jaren
  for (year in jaren){
    jaar <- as.character(year)
    
    if(!jaar %in% names(bevolkingsopbouw_split)){
      stop("Geen data voor", jaar)
    }
    
    #opgesplitste dataset voor jaar in kwestie
    bevolkingsopbouw_jaar <- bevolkingsopbouw_split[[jaar]]
    
    #namen terug veranderen
    if(paste0("gem", year) %in% colnames(bevolkingsopbouw_jaar)){
      setnames(bevolkingsopbouw_jaar, 
               old = c(paste0("gem", year), paste0("PC4", year)),
               new = c("GBAGEMEENTEINSCHRIJVING31DECEMBER",
                         "GBAPOSTCODE4GAAF31DECEMBER"))
    }
    
    # #lege kolommen weghalen
    bevolkingsopbouw_jaar <- bevolkingsopbouw_jaar[, which(colSums(!is.na(bevolkingsopbouw_jaar)) > 0),
                                                   with = FALSE]

    #exporteren
    file_name <- paste0("Bevolkingsopbouw_", year, ".csv")
    file_path <- file.path(folder_path, file_name)
    fwrite(bevolkingsopbouw_jaar, file_path, sep = ";")
    cat("Bevolkinsopbouw geëxporteerd voor", year)
  }
}

#Export runnen
export_per_jaar(bevolkingsopbouw_jaren, jaren, folder_path)

# -----------------------------------------------------------------------------
#SET UP EINDE
# -----------------------------------------------------------------------------
#Log stoppen
sink()
