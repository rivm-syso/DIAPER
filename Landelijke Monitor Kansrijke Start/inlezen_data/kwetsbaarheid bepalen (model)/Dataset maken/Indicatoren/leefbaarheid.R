#Indicatoren per gemeenten voor KS basisset

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#jaren
#jaren <- c(2015:2020)

#Gemeente indeling
#gemeente_indeling <- "gem2021"

#------------------------------------------------------------------------------
#Checken of loctatie al gerund is, dan niet nog een keer runnen
#------------------------------------------------------------------------------

if (!"locatie" %in% ls(envir = .GlobalEnv)) {
  source("src/indicatoren/locatie.R")
}

#------------------------------------------------------------------------------
#Inlezen en opruimen leefbaarheid data
#------------------------------------------------------------------------------
lb_data <- read_excel("H:/Data proces/data/Leefbaarometer/Leefbaarometer PC4.xlsx")

#kolommen selecteren
lb_data <- as.data.table(lb_data[, 1:7])

#long format
lb_data <- melt(lb_data, id = "CODE", variable.name = "jaar", value.name = "leefbaarheid")

setnames(lb_data, "CODE", "PC4")

#jaren omzetten
lb_data[, jaar := as.numeric(str_c("20", substr(jaar, 3, 4)))]

#Lage leefbaarheidsscore vaststallen
lb_data[, "lage_leefbaarheid" := ifelse(leefbaarheid <= 4, 1, 0)]

#------------------------------------------------------------------------------
#mergen met locatie
#dichtsbijzijnde waarde voor jaar kiezen
#------------------------------------------------------------------------------

setkey(locatie, PC4, jaar)
setkey(lb_data, PC4, jaar)

leefbaarheid <- lb_data[locatie, roll = "nearest", on = .(PC4, jaar)]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
rm(lb_data)

