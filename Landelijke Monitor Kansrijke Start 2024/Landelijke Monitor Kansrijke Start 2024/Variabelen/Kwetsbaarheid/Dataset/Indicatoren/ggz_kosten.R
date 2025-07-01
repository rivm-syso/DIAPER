#ZVW zorgkosten

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
ggz_kosten <- inlezen_data("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB", 
                           kolommen = c("RINPERSOONS", "RINPERSOON", "ZVWKGENBASGGZ", "ZVWKSPECGGZ"))

#GGZ kosten
setnames(
  ggz_kosten,
  c("ZVWKGENBASGGZ", "ZVWKSPECGGZ"),
  c("kosten_basis_GGZ", "kosten_spec_GGZ")
)

ggz_kosten[, ':=' (
  kosten_basis_GGZ = ifelse(kosten_basis_GGZ > 0, 1, 0),
  kosten_spec_GGZ = ifelse(kosten_spec_GGZ > 0, 1, 0),
  kosten_GGZ = ifelse(kosten_spec_GGZ > 0 | kosten_basis_GGZ > 0, 1, 0)
)]

#overige kolommen verwijderen
ggz_kosten <- ggz_kosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "kosten_basis_GGZ",
                             "kosten_spec_GGZ", "kosten_GGZ")]
