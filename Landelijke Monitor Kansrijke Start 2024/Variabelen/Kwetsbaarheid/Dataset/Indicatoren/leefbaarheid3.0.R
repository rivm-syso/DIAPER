#Indicatoren per gemeenten voor KS basisset leefbaarheid 3.0

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
lb_data3 <- read.csv("H:/Data proces/data/Leefbarometer/Leefbaarometer 3.0/Leefbaarometer 3.0 - meting 2020 - scores PC4.csv")
lb_data3 <- as.data.table(lb_data3)


#Lage leefbaarheidsscore vaststallen (score onder 3.81) 
                                                      #LBM3Instrumentontwikkeling p.92
lb_data3[, "lage_leefbaarheid" := ifelse(lbm <= 3.81, 1, 0)]

#kolommen selecteren 
lb_data3 <- lb_data3 %>% select(PC4, jaar, lage_leefbaarheid)
#------------------------------------------------------------------------------
#mergen met locatie
#dichtsbijzijnde waarde voor jaar kiezen
#------------------------------------------------------------------------------

setkey(locatie, PC4, jaar)
setkey(lb_data3, PC4, jaar)

locatie[, jaar := as.numeric(as.character(jaar))]
lb_data3[, jaar := as.numeric(as.character(jaar))]
locatie[, PC4 := as.numeric(as.character(PC4))]
lb_data3[, PC4 := as.numeric(as.character(PC4))]

leefbaarheid <- lb_data3[locatie, roll = "nearest", on = .(PC4, jaar)]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
rm(lb_data3)


