#ZVW wanbetaler

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
zvw_wanbetaler <- inlezen_data("G:/GezondheidWelzijn/WANBZVWTAB", 
                           c("RINPERSOONS", "RINPERSOON"))

zvw_wanbetaler[, "zvw_wanbetaler" := TRUE]

zvw_wanbetaler[, jaar := as.numeric(as.character(jaar))]