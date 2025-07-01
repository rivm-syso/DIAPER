#ZVW wanbetaler
#Dataset: WANBZVWTAB

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
zvw_wanbetaler <- inlezen_data("G:/GezondheidWelzijn/WANBZVWTAB", 
                           c("RINPERSOONS", "RINPERSOON"))

zvw_wanbetaler[, "zvw_wanbetaler" := TRUE]

zvw_wanbetaler[, jaar := as.numeric(as.character(jaar))]

#------------------------------------------------------------------------------
#gegevens opslaan in een temp map
#------------------------------------------------------------------------------
write.csv2(zvw_wanbetaler, "temp/zvw_wanbetaler.csv")