#aanvullend KS24 nationaal 5 jaar gemiddelde


#teller: zwangere vrouwen in jaar in kwestie die voldoen aan de variabele
#noemer: zwangere vrouwen in jaar in kwestie

#utilities
setwd("H:/GIT data proces/ks data proces")
source("utils.R")


jaren = 2017:2023
years = jaren

schulden_gem2023_ouders <- data.frame()
kwetsbaar_gem2023 <- data.frame()

for(jaar in jaren){
  #------------------------------------------------------------------------------
  #INLEZEN DATA
  #------------------------------------------------------------------------------
  bevolking_met_leeftijd <- fread(paste0("temp/Leeftijd_met_regiocodes_", jaar, ".csv"), 
                    colClasses = list(character = c("RINPERSOON",
                                                    "gem2023", 
                                                    "PC4",
                                                    "RINPERSOONpa", 
                                                    "RINPERSOONMa")))


  if(!"kwetsbaarheid" %in% ls(envir = .GlobalEnv)) {
    kwetsbaarheid <- read.fst("temp/meervoudig_kwetsbaar24_v2.fst")
  }
  schuldsanering <- fread(paste0("temp/Schuldsanering_", jaar,".csv"))
  zvw_wanbetaler <- fread(paste0("temp/Zvw_wanbetaler_", jaar,".csv"), colClasses = list(character = "RINPERSOON"))


  #------------------------------------------------------------------------------
  #OPBOUWEN POPULATIE ZWANGERE VROUWEN
  #------------------------------------------------------------------------------
  #selectie personenen die zwanger waren in betreffende jaar 
  zwangeren <- bevolking_met_leeftijd %>%
    filter(year(geboortedatum) == jaar) %>%
    rename(RINPERSOON_KIND = RINPERSOON, RINPERSOONS_KIND = RINPERSOONS) %>% #kinderen definieren 
    rename(RINPERSOON = RINPERSOONMa, RINPERSOONS = RINPERSOONSMa) #moeders primaire RIN  


  #Unieke moeders per kalender jaar
  zwangeren <- zwangeren %>% group_by(RINPERSOON) %>% slice_head(n=1) %>% ungroup()

  # Schulden en wanbetaal info van moeder toevoegen
  zwangeren <- Reduce(function(df1, df2) merge(df1 ,df2, all.x = TRUE, 
                                              by = c("jaar", "RINPERSOONS", "RINPERSOON")),
                      list(zwangeren, 
                          schuldsanering, 
                          zvw_wanbetaler
                      ))
  # Schulden en wanbetaal info van vader toevoegen
  zwangeren <- Reduce(function(df1, df2) merge(df1 ,df2, all.x = TRUE, suffixes = c("_Ma", "_Pa"),
                                              by.x = c("jaar", "RINPERSOONSpa", "RINPERSOONpa"),
                                              by.y = c("jaar", "RINPERSOONS", "RINPERSOON")),
                      list(zwangeren, 
                          schuldsanering, 
                          zvw_wanbetaler
                                              ))

  #kwetsbaarheid op RIN kind toevoegen
  zwangeren <- merge(zwangeren, kwetsbaarheid, by.x= c("RINPERSOONS_KIND", "RINPERSOON_KIND") ,by.y= c("Rinpersoons_KIND", "RINPERSOON_KIND"), all.x =T)
  zwangeren <- data.table(zwangeren)

  # ja/nee indiicatoren maken
  # problematische schulden als in schuldsanering en/of wanbetaler
  zwangeren[, ':=' (
    schulden = ifelse(schuldsanering_Ma == 1 | zvw_wanbetaler_Ma == TRUE, 1, 0),
    schulden_Pa = ifelse(schuldsanering_Pa == 1 | zvw_wanbetaler_Pa == TRUE, 1, 0),
    kwetsbaar = ifelse(voorspelling_kwetsbaar == "ja",1,0)
  )]

  #Waar relevant NA's op 0 zetten
  #Specifiek: schulden, wanbetaler, psych_prob, angst_depressie
  zwangeren[, schulden := ifelse(is.na(schulden), 0, schulden)]

  #Waarden en/of vader/moeder positief
  zwangeren[, ':=' (
    schulden_ouders =ifelse(schulden == 1 | schulden_Pa ==1, 1,0),
    schuldsanering_ouders = ifelse(schuldsanering_Ma == 1 | schuldsanering_Pa ==1, 1,0),
    zvwwan_ouders = ifelse(zvw_wanbetaler_Ma == 1 | zvw_wanbetaler_Pa ==1, 1,0)
  )]

  #waarden op 0 zetten wanneer niet 1
  zwangeren[is.na(schulden_ouders), schulden_ouders := 0]
  zwangeren[is.na(schulden), schulden := 0]
  zwangeren[is.na(schulden_Pa), schulden_Pa := 0]
  zwangeren[is.na(schuldsanering_ouders), schuldsanering_ouders := 0]
  zwangeren[is.na(schuldsanering_Ma), schuldsanering_Ma := 0]
  zwangeren[is.na(schuldsanering_Pa), schuldsanering_Pa := 0]
  zwangeren[is.na(zvwwan_ouders), zvwwan_ouders := 0]
  zwangeren[is.na(zvw_wanbetaler_Ma), zvw_wanbetaler_Ma := 0]
  zwangeren[zvw_wanbetaler_Ma == T, zvw_wanbetaler_Ma := 1]
  zwangeren[zvw_wanbetaler_Ma == F, zvw_wanbetaler_Ma := 0]
  zwangeren[zvw_wanbetaler_Ma == T, zvw_wanbetaler_Mam := 1]
  zwangeren[zvw_wanbetaler_Ma == F, zvw_wanbetaler_Mam := 0]
  zwangeren[is.na(zvw_wanbetaler_Mam), zvw_wanbetaler_Mam := 0]
  zwangeren[is.na(zvw_wanbetaler_Pa), zvw_wanbetaler_Pa := 0]
  zwangeren[zvw_wanbetaler_Pa == T, zvw_wanbetaler_Pap := 1]
  zwangeren[zvw_wanbetaler_Pa == F, zvw_wanbetaler_Pap := 0]
  zwangeren[is.na(zvw_wanbetaler_Pap), zvw_wanbetaler_Pap := 0]

  # variabelen op gemeente niveau uitrekenen --------------------------------
  schulden_prob_ouders_gem23_ouders <-  zwangeren %>% group_by(gem2023, schulden_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) #%>% pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{schulden_ouders}" , names_from = "schulden_ouders", values_from = "n") %>%  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0))) %>% mutate(jaar = j)                   
  kwetsbaar_moeder_gem2023 <-  zwangeren %>% group_by(gem2023, kwetsbaar) %>% summarise(n = n()) %>% mutate(jaar = j) #%>% pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{kwetsbaar}" , names_from = "kwetsbaar", values_from = "n") %>%  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0))) %>% mutate(jaar = j)                   

  #Koppelen per jaar
  schulden_gem2023_ouders <- rbind(schulden_gem2023_ouders,schulden_prob_ouders_gem23_ouders)
  kwetsbaar_gem2023 <- rbind(kwetsbaar_gem2023,kwetsbaar_moeder_gem2023)

}

# vijf jaars gemiddelde ---------------------------------------------------
schulden_5J_gem <- schulden_gem2023_ouders %>% filter(jaar > (max(jaren)-5)) %>% group_by(gem2023, schulden_ouders) %>% summarise(n=sum(n)) %>% 
  pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{schulden_ouders}" , names_from = "schulden_ouders", values_from = "n") %>% 
  mutate(percentage = n_1/(n_1+n_0)) %>% select(gem2023, n_1, n_0, percentage) %>% 
  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0)), percentage = ifelse(n_1 <10 | n_0 <10, "<10", as.character(percentage))) %>% 
  mutate(n_1 = ifelse(is.na(n_1)| is.na(n_0), "<10", as.character(n_1)), n_0 = ifelse(is.na(n_1)| is.na(n_0), "<10", as.character(n_0)), percentage = ifelse(is.na(n_1)| is.na(n_0) | is.na(percentage), "<10", as.character(percentage))) 
  

kwetsbaar_5J_gem <- kwetsbaar_gem2023 %>% filter(jaar > (max(jaren)-5)) %>% group_by(gem2023, kwetsbaar) %>% summarise(n=sum(n)) %>% 
  pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{kwetsbaar}" , names_from = "kwetsbaar", values_from = "n") %>% 
  mutate(percentage = n_1/(n_1+n_0)) %>% select(gem2023, n_1, n_0, percentage) %>% 
  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0)), percentage = ifelse(n_1 <10 | n_0 <10, "<10", as.character(percentage))) %>% 
  mutate(n_1 = ifelse(is.na(n_1)| is.na(n_0), "<10", as.character(n_1)), n_0 = ifelse(is.na(n_1)| is.na(n_0), "<10", as.character(n_0)), percentage = ifelse(is.na(n_1)| is.na(n_0) | is.na(percentage), "<10", as.character(percentage))) 


# Data opslaan ------------------------------------------------------------
write.csv2(schulden_5J_gem, "Output/KS24/schulden_gem23_ouders_5jaarsGEm.csv")
write.csv2(schulden_5J_gem_nietclean, "Output/KS24/schulden_gem23_ouders_5jaarsGEm_ongeschoond.csv")
write.csv2(kwetsbaar_5J_gem, "Output/KS24/kwetsbaarheid_gem23_ouders_5jaarsGEm.csv")
write.csv2(kwetsbaar_5J_gem_nietclean, "Output/KS24/kwetsbaarheid_gem23_ouders_5jaarsGEm_ongeschoond.csv")

