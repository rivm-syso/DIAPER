#Inlezen en opschonen van medicijn data
#Dataset: MEDICIJNTAB
#N05A     Antipsychotica
#N05B     Anxiolytica
#N05C     Hypnotica en sedativa
#N06A     Antidepressiva
#N06B     Psychostimul., midd adhd/nootropica
#N06C     Psycholeptica met psychoanaleptica


#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
med_gebruik <- inlezen_data("G:/GezondheidWelzijn/MEDICIJNTAB", 
                            c("RINPERSOONS", "RINPERSOON", "ATC4"))

#------------------------------------------------------------------------------
#Opschonen
#------------------------------------------------------------------------------
med_gebruik[, ATC4 := as.character(ATC4)]

#alle personen vaststellen die medicatie voor mentale klachten hebben gehad 
psych_gebruik <- med_gebruik[, ':='(
  "med_psych_klachten" = ifelse(ATC4 %in% c("N05A", "N05B", "N05C", "N06A", "N06B", "N06C"), 1, 0),
  "Gebruik_depress_angst_medicatie" = ifelse(ATC4 %in% c("N05B", "N06A"),  1, 0)
)]

#drop irrelevant rows for computational efficiency
psych_gebruik <- psych_gebruik[psych_gebruik$med_psych_klachten == 1 | psych_gebruik$Gebruik_depress_angst_medicatie == 1, ]

#selection ipv unique() to use correct selection ipv first row
# psych_gebruik <- psych_gebruik %>%
#   group_by(jaar, RINPERSOONS, RINPERSOON) %>%
#   summarize(
#     med_psych_klachten = as.integer(any(med_psych_klachten == 1)),
#     Gebruik_depress_angst_medicatie = as.integer(any(Gebruik_depress_angst_medicatie == 1))
#   )

psych_gebruik <- psych_gebruik[, .(
  med_psych_klachten = as.integer(any(med_psych_klachten == 1)),
  Gebruik_depress_angst_medicatie = as.integer(any(Gebruik_depress_angst_medicatie == 1))
), by = .(jaar, RINPERSOONS, RINPERSOON)]

#------------------------------------------------------------------------------
#Check aantallen
#------------------------------------------------------------------------------
# table(psych_gebruik$med_psych_klachten, useNA = "ifany")
# table(psych_gebruik$Gebruik_depress_angst_medicatie, useNA = "ifany")
# #2022, hier: 1251829
# check_N05B_N06A <- med_gebruik %>%
#   filter(ATC4 %in% c("N05B", "N06A")) %>%
#   group_by(jaar, RINPERSOONS, RINPERSOON) %>%
#   summarize(both = all(c("N05B", "N06A") %in% ATC4)) %>%
#   filter(both == TRUE) %>%
#   nrow()
# check_N05B_N06A #2022: 141879
# #2022, check in data: N05B (307199) | N06A (1086509) = 1393708
# #2022: Klopt!

med_gebruik <- psych_gebruik

#------------------------------------------------------------------------------
#gegevens opslaan in een temp map
#------------------------------------------------------------------------------
write.csv2(med_gebruik, "temp/medicijngebruik.csv")

#------------------------------------------------------------------------------
#Opruimen
#------------------------------------------------------------------------------
rm(psych_gebruik)

