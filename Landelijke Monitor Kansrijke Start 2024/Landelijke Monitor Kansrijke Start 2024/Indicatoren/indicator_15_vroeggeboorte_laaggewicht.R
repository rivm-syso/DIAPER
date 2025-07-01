# Indicator 15 (vroeggeboorte en/of laag geboortegewicht): 
# Deze cijfers zijn door het RIVM berekend in DIAPER en gebaseerd op niet-openbare microdata van het CBS 
# en zwangerschap en geboortegegevens van Perined. 
# Voor alle levendgeborenen en doodgeborenen bij een zwangerschap van minstens 24 weken 
# waarvan van wie geboortegegevens beschikbaar zijn in de Perined- registratie is bepaald 
# of zij te vroeg geboren zijn (zwangerschapsduur minder dan 37 weken), 
# een te laag geboortegewicht hadden voor de zwangerschapsduur (percentiel lager dan 10%), 
# of beidenallebei.  

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
library(broom)
library(xlsx)
library(stats)

#jaren
jaren <- c(2021) # laatste jaar in Perined
#------------------------------------------------------------------------------
#inlezen variabelen
#------------------------------------------------------------------------------
#CBS indicatoren
source("variabelen/huishoud_inkomen.R")
source("variabelen/land_van_herkomst.R")
source("variabelen/opleiding.R")

#Perined
perined_variabelen <- c("jaar", "amww", "sterfte", "hoftiezer", "rangnummer", "omv")

source("variabelen/perined.R")

#------------------------------------------------------------------------------
#opschonen
#------------------------------------------------------------------------------
#perined
setnames(perined_data,
         c("amww", "sterfte"),
         c("zwngschp_duur_weken", "perinatale_sterfte"))

#alleen 24 weken of meer
perined_data <- perined_data[zwngschp_duur_weken >= 24]

#Categorische (factor) variabelen aanmaken voor analyses
perined_data[, "perinatale_sterfte_cat" := as.factor(fcase(
  perinatale_sterfte %in% c(1:4), 1,
  perinatale_sterfte == 0, 0
  ))]

perined_data[, ':=' (
  "perinatale_sterfte" = ifelse(perinatale_sterfte %in% c(1:4), 1, 0),
  "neonatale_sterfte" = ifelse(perinatale_sterfte %in% c(3:4), 1, 0),
  "foetale_sterfte" = ifelse(perinatale_sterfte %in% c(1:2), 1, 0),
  "zuigeling_sterfte" = ifelse(perinatale_sterfte %in% c(3:5), 1, 0),
  "vroeggeboorte_37w" = ifelse(zwngschp_duur_weken < 37, 1, 0),
  "vroeggeboorte_32w" = ifelse(zwngschp_duur_weken < 32, 1, 0),
  "laag_gewicht" = as.factor(ifelse(hoftiezer < 10, 1, 0)),
  "big2" = as.factor(ifelse(zwngschp_duur_weken < 37 |  hoftiezer < 10, 1, 0))
)]

#------------------------------------------------------------------------------
#mergen
#------------------------------------------------------------------------------

perined_data <- perined_data %>% rename(perined_jaar = jaar)
perined_data <- merge(perined_data, huishoud_inkomen, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON"))
perined_data <- merge(perined_data, hoogstopl_data, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON"))
perined_data <- merge(perined_data, etnische_achtergrond, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON"))
#inkomenskwintielen moeten als factoren worden behandeld, niet als numeriek
perined_data <- perined_data %>% mutate(ink_kwint = as.factor(ink_kwint))

#voorlopig alleen naar 2021 kijken...
perined_data_2021 <- perined_data %>% filter(perined_jaar == 2021)

#maar wel 2017-2021 voor sterfecijfers
perined_data_5y <- perined_data %>% filter(perined_jaar <= 2021, perined_jaar >= 2017)

#------------------------------------------------------------------------------
#tabellen
#------------------------------------------------------------------------------

#meerlingen wel of niet meenemen ("ja" of "nee")
meerlingen <- "ja"

if (meerlingen == "nee") {
  perined_data_2021 <- perined_data_2021 %>% filter(omv == 1)
  perined_data_5y <- perined_data_5y %>% filter(omv == 1)
}

#als aantallen kleiner dan 10 dan mogen ze niet mee in de export
onder_10 <- function(x){
  x[, N := ifelse(N < 10, NA, N)]
  x[, Percentage_Totaal := ifelse(N < 10, NA, Percentage_Totaal)]
  x[, Percentage_Groep := ifelse(N < 10, NA, Percentage_Groep)]
}

#belangrijk: de variabelen "groep_N", "Percentage Totaal" en "Percentage Groep"
#mogen niet geexporteerd worden omdat <10 gevallen dan teruggerekend kunnen worden, 
#dus deze variabelen zijn er alleen voor inzicht binnen de microdata omgeving!
aantallen_var <- function(output, perined_data) {
  perined_totaal <- dim(perined_data)[1]
  
  migratie <- perined_data[, .N, by = .(eval(parse(text = output)), LANDTIENDELING)]
  migratie_groep <- perined_data[, .(groep_N = .N), by = .(eval(parse(text = output)))]
  migratie <- merge(migratie, migratie_groep)
  migratie$Percentage_Totaal = 100 * migratie$N / perined_totaal
  migratie$Percentage_Groep = 100 * migratie$N / migratie$groep_N
  
  migratie$groep_fisher_95_odds_ratio_lower = NA
  migratie$groep_fisher_95_odds_ratio_upper = NA
  for (i in 1:dim(migratie)[1])
    if (migratie$N[1] >= 10) {
      fisht <- fisher.test(as.table(rbind(c(migratie$groep_N[i], migratie$N[i]), c(migratie$groep_N[i], migratie$N[i]))), 
                           conf.level = 0.95, conf.int = TRUE)
      migratie$groep_fisher_95_odds_ratio_lower[i] <- fisht$conf.int[1]
      migratie$groep_fisher_95_odds_ratio_upper[i] <- fisht$conf.int[2]
    }
  
  inkomen <- perined_data[, .N, by = .(eval(parse(text = output)), ink_kwint)]
  inkomen_groep <- perined_data[, .(groep_N = .N), by = .(eval(parse(text = output)))]
  inkomen <- merge(inkomen, inkomen_groep)
  inkomen$Percentage_Totaal = 100 * inkomen$N / perined_totaal
  inkomen$Percentage_Groep = 100 * inkomen$N / inkomen$groep_N
  
  inkomen$groep_fisher_95_odds_ratio_lower = NA
  inkomen$groep_fisher_95_odds_ratio_upper = NA
  for (i in 1:dim(inkomen)[1])
    if (inkomen$N[i] >= 10) {
      fisht <- fisher.test(as.table(rbind(c(inkomen$groep_N[i], inkomen$N[i]), c(inkomen$groep_N[i], inkomen$N[i]))), 
                         conf.level = 0.95, conf.int = TRUE)
      inkomen$groep_fisher_95_odds_ratio_lower[i] <- fisht$conf.int[1]
      inkomen$groep_fisher_95_odds_ratio_upper[i] <- fisht$conf.int[2]
    }
  
  opleiding <- perined_data[, .N, by = .(eval(parse(text = output)), opleidingsniveau)]
  opleiding_groep <- perined_data[, .(groep_N = .N), by = .(eval(parse(text = output)))]
  opleiding <- merge(opleiding, opleiding_groep)
  opleiding$Percentage_Totaal = 100 * opleiding$N / perined_totaal
  opleiding$Percentage_Groep = 100 * opleiding$N / opleiding$groep_N
  
  opleiding$groep_fisher_95_odds_ratio_lower = NA
  opleiding$groep_fisher_95_odds_ratio_upper = NA
  for (i in 1:dim(opleiding)[1]) 
    if (opleiding$N[i] >= 10) {
      fisht <- fisher.test(as.table(rbind(c(opleiding$groep_N[i], opleiding$N[i]), c(opleiding$groep_N[i], opleiding$N[i]))), 
                         conf.level = 0.95, conf.int = TRUE)
      opleiding$groep_fisher_95_odds_ratio_lower[i] <- fisht$conf.int[1]
      opleiding$groep_fisher_95_odds_ratio_upper[i] <- fisht$conf.int[2]
    }
   
  data_list <- list(migratie, inkomen, opleiding)
  
  names(data_list) <- c("migratie", "inkomen", "opleiding")
  
  lapply(data_list, setnames, "parse", output)
  lapply(data_list, onder_10)
  
  walk2(.x = data_list,
       .y = str_c("mypath", output, "_", names(data_list), ".csv"),
        .f = write.csv2)
}

lapply(list("perinatale_sterfte_cat", "neonatale_sterfte", "foetale_sterfte", "zuigeling_sterfte"), 
       perined_data = perined_data_5y,
       aantallen_var)

lapply(list("vroeggeboorte_37w", "laag_gewicht", "vroeggeboorte_32w", "big2"), perined_data = perined_data_2021,
       aantallen_var)

