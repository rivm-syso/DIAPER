#Hoogst behaalde opleiding per jaar en opleidingsniveau
#Dataset: HOOGSTEOPLTAB

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#HOOGSTEOPLTAB
#------------------------------------------------------------------------------
hoogstopl_data <- inlezen_data("G:/Onderwijs/HOOGSTEOPLTAB/", c("RINPERSOONS", "RINPERSOON", "OPLNRHB"),
                               specific = "opleiding")

#------------------------------------------------------------------------------
#Opleidingnr omzetten naar SOI2016
#------------------------------------------------------------------------------
oplnr_cto <- read_spss("K:/Utilities/Code_Listings/SSBreferentiebestanden/OPLEIDINGSNRREFV34.SAV",
                       col_select = c(OPLNR, CTO))
cto_SOI2016 <- read_spss("K:/Utilities/Code_Listings/SSBreferentiebestanden/CTOREFV12.SAV",
                         col_select = c(CTO, OPLNIVSOI2016AGG4HB))

koppel_bestand <- as.data.table(merge(oplnr_cto, cto_SOI2016))
koppel_bestand[, CTO := NULL]

hoogstopl_data <- merge(hoogstopl_data, koppel_bestand, 
                        by.x = "OPLNRHB",
                        by.y = "OPLNR")

hoogstopl_data[, OPLNRHB := NULL]

#------------------------------------------------------------------------------
#Categorien toevoegen
#------------------------------------------------------------------------------

hoogstopl_data[, "opleidingsniveau" := fcase(
  startsWith(OPLNIVSOI2016AGG4HB, "1"), "laag",
  startsWith(OPLNIVSOI2016AGG4HB, "2"), "midden",
  startsWith(OPLNIVSOI2016AGG4HB, "3"), "hoog",
  OPLNIVSOI2016AGG4HBMETNIRWO %in% c("----", "9999"), "onbekend"
)]

#------------------------------------------------------------------------------
#Labels omzetten
#------------------------------------------------------------------------------

hoogstopl_data[, "hoogste_opleiding" := unlabelled(OPLNIVSOI2016AGG4HB)]

hoogstopl_data[, OPLNIVSOI2016AGG4HB := NULL]

hoogstopl_data[, jaar := as.numeric(as.character(jaar))]

#------------------------------------------------------------------------------
#gegevens opslaan in een temp map
#------------------------------------------------------------------------------
write.csv2(hoogstopl_data, "temp/hoogsteopleiding.csv")

#------------------------------------------------------------------------------
#Opruimen
#------------------------------------------------------------------------------
rm(oplnr_cto, cto_SOI2016, koppel_bestand)
