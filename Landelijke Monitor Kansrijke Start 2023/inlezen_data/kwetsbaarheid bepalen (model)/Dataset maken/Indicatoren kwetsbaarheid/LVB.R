#LVB
#Lichtelijke verstandelijke beperking.

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")

#Jaren

years <- jaren

#------------------------------------------------------------------------------
# data ophalen
#------------------------------------------------------------------------------
AO <- readRDS("L:/AO-2015-2021.rds") #2015-2021 data
CIZ <- readRDS("L:/CIZ-2017-2021.rds")#2015-2021 data
WSW <- readRDS("L:/WSW-2015-2021.rds")#2015-2021 data
LVB1415 <- read_spss("H:/LCA kwetsbaarheid/Joyce/Microdata/Data/LVB_Basisbestand 2014 en 2015.sav") #LVB informatie uit 2014 & 2015
  
  
AO <- data.table(AO)
CIZ <- data.table(CIZ)
WSW <- data.table(WSW)
LVB1415 <-  data.table(LVB1415)
# merging sets ------------------------------------------------------------

#Schonen
AO[, LVB := T]
CIZ[, LVB := T]
WSW[, LVB := T]
LVB1415[LVBregistratie == 1, LVB := T]
LVB1415 <-  LVB1415[LVB == T,]

AO <- AO[, c("RINPERSOONS", "RINPERSOON", "LVB")]
CIZ <- CIZ[, c("RINPERSOONS", "RINPERSOON", "LVB")]
WSW <- WSW[, c("RINPERSOONS", "RINPERSOON", "LVB")]
LVB1415 <- LVB1415[, c("RINPERSOONS", "RINPERSOON", "LVB")]

#mergen
LVB <- rbind(AO, CIZ)
LVB <- rbind(LVB, WSW)
LVB <- rbind(LVB, LVB1415)

# 1 rij per persoon
LVB <- unique(LVB)



# schonen -----------------------------------------------------------------

rm(AO, CIZ, WSW, LVB1415)
