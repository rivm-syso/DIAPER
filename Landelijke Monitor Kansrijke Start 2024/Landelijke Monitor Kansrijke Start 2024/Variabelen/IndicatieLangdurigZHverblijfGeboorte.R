#Kraamzorg verdiepende verkenning

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
library(fst)
library(xlsx)

#Jaren
jaren <- c(2015:2023)

#locatie indeling
gemeente_indeling <- "gem2022"


# Vrouwen/kinderen selecteren met indicatie landurig verblijf -------------
#ivm met grote bestanden opdelen over seperate jaren

for( j in years){
  jaren <- j
  dat <- inlezen_data("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB", kolommen = c("RINPERSOONS", "RINPERSOON", "VEKTMSZDBCZorgproduct"))
  
  #Bepalen wie in aanmerkign komt voor indicatie langdurig verblijf
  datx <- dat %>% mutate(langdurig_verblijf = ifelse(VEKTMSZDBCZorgproduct == "159899011" | VEKTMSZDBCZorgproduct == "159899012" | VEKTMSZDBCZorgproduct == "990016382" | VEKTMSZDBCZorgproduct == "990016383" | VEKTMSZDBCZorgproduct == "990017011" | VEKTMSZDBCZorgproduct == "990017012" |  VEKTMSZDBCZorgproduct == "990017015" | VEKTMSZDBCZorgproduct == "990017016" | VEKTMSZDBCZorgproduct == "990017021" | VEKTMSZDBCZorgproduct == "990017022" |  VEKTMSZDBCZorgproduct == "990017023" | VEKTMSZDBCZorgproduct == "990017024" |       VEKTMSZDBCZorgproduct == "990017025" | VEKTMSZDBCZorgproduct == "990017026" | VEKTMSZDBCZorgproduct == "990017029" | VEKTMSZDBCZorgproduct == "990017030" |  VEKTMSZDBCZorgproduct == "990017033" | VEKTMSZDBCZorgproduct == "990017034" |  VEKTMSZDBCZorgproduct == "990017037" | VEKTMSZDBCZorgproduct == "990017038" | VEKTMSZDBCZorgproduct == "990017040" | VEKTMSZDBCZorgproduct == "990017041" |  VEKTMSZDBCZorgproduct == "990017042" | VEKTMSZDBCZorgproduct == "990017043" |    VEKTMSZDBCZorgproduct == "990017048" | VEKTMSZDBCZorgproduct == "990017049", 1,0)) %>% 
    mutate(langdurig_verblijf_alt = ifelse(VEKTMSZDBCZorgproduct == "159899011" | VEKTMSZDBCZorgproduct == "159899012" | VEKTMSZDBCZorgproduct == "990016382" | VEKTMSZDBCZorgproduct == "990016383" | VEKTMSZDBCZorgproduct == "990017011" | VEKTMSZDBCZorgproduct == "990017012" |  VEKTMSZDBCZorgproduct == "990017015" | VEKTMSZDBCZorgproduct == "990017016" | VEKTMSZDBCZorgproduct == "990017021" | VEKTMSZDBCZorgproduct == "990017022" |  VEKTMSZDBCZorgproduct == "990017023" | VEKTMSZDBCZorgproduct == "990017024" |       VEKTMSZDBCZorgproduct == "990017025" | VEKTMSZDBCZorgproduct == "990017026" | VEKTMSZDBCZorgproduct == "990017029" | VEKTMSZDBCZorgproduct == "990017030" |  VEKTMSZDBCZorgproduct == "990017033" | VEKTMSZDBCZorgproduct == "990017034" |  VEKTMSZDBCZorgproduct == "990017037" | VEKTMSZDBCZorgproduct == "990017038" | VEKTMSZDBCZorgproduct == "990017040" | VEKTMSZDBCZorgproduct == "990017041" |  VEKTMSZDBCZorgproduct == "990017042" | VEKTMSZDBCZorgproduct == "990017043" |    VEKTMSZDBCZorgproduct == "990017048" | VEKTMSZDBCZorgproduct == "990017049" |
                                             VEKTMSZDBCZorgproduct == "990017046" | VEKTMSZDBCZorgproduct == "990017047" , 1,0)) %>% 
    mutate(langdurig_verblijf_evt = ifelse(VEKTMSZDBCZorgproduct == "159899011" | VEKTMSZDBCZorgproduct == "159899012" | VEKTMSZDBCZorgproduct == "990016382" | VEKTMSZDBCZorgproduct == "990016383" | VEKTMSZDBCZorgproduct == "990017011" | VEKTMSZDBCZorgproduct == "990017012" |  VEKTMSZDBCZorgproduct == "990017015" | VEKTMSZDBCZorgproduct == "990017016" | VEKTMSZDBCZorgproduct == "990017021" | VEKTMSZDBCZorgproduct == "990017022" |  VEKTMSZDBCZorgproduct == "990017023" | VEKTMSZDBCZorgproduct == "990017024" |       VEKTMSZDBCZorgproduct == "990017025" | VEKTMSZDBCZorgproduct == "990017026" | VEKTMSZDBCZorgproduct == "990017029" | VEKTMSZDBCZorgproduct == "990017030" |  VEKTMSZDBCZorgproduct == "990017033" | VEKTMSZDBCZorgproduct == "990017034" |  VEKTMSZDBCZorgproduct == "990017037" | VEKTMSZDBCZorgproduct == "990017038" | VEKTMSZDBCZorgproduct == "990017040" | VEKTMSZDBCZorgproduct == "990017041" |  VEKTMSZDBCZorgproduct == "990017042" | VEKTMSZDBCZorgproduct == "990017043" |    VEKTMSZDBCZorgproduct == "990017048" | VEKTMSZDBCZorgproduct == "990017049"| 
                                             VEKTMSZDBCZorgproduct == "990416016" | VEKTMSZDBCZorgproduct == "990616046" | VEKTMSZDBCZorgproduct == "990516042" | VEKTMSZDBCZorgproduct == "990316017" | VEKTMSZDBCZorgproduct == "990316018" | VEKTMSZDBCZorgproduct == "990616040" |  VEKTMSZDBCZorgproduct == "991116014 " | VEKTMSZDBCZorgproduct == "990516043" | VEKTMSZDBCZorgproduct == "990216031" | VEKTMSZDBCZorgproduct == "991316028" |  VEKTMSZDBCZorgproduct == "990416063" | VEKTMSZDBCZorgproduct == "990416005" |       VEKTMSZDBCZorgproduct == "991630035" | VEKTMSZDBCZorgproduct == "990616005" | VEKTMSZDBCZorgproduct == "990356068" | VEKTMSZDBCZorgproduct == "990416053" |  VEKTMSZDBCZorgproduct == "990017033" | VEKTMSZDBCZorgproduct == "990516051" |  VEKTMSZDBCZorgproduct == "991516009" | VEKTMSZDBCZorgproduct == "972802111" | VEKTMSZDBCZorgproduct == "119499064" | VEKTMSZDBCZorgproduct == "990516050" |  VEKTMSZDBCZorgproduct == "991630034" | VEKTMSZDBCZorgproduct == "990616029" |    VEKTMSZDBCZorgproduct == "991316008" | VEKTMSZDBCZorgproduct == "990316065" |
                                             VEKTMSZDBCZorgproduct == "159999021" | VEKTMSZDBCZorgproduct == "159899012" | VEKTMSZDBCZorgproduct == "159999020"  | VEKTMSZDBCZorgproduct == "990017046" | VEKTMSZDBCZorgproduct == "990017047" ,1,0)) %>% 
    filter(langdurig_verblijf_evt == 1)
  
  
  datx <- datx %>% mutate(jaar = as.numeric(as.character(jaar)))
  write.csv(datx, paste0("H:/PP/langdurigVerblijf", j, ".csv"))
}

rm(dat)


# 1 langdurig verblijf bestand ------------------------------------------------------

for( j in years){
  pasteo("dat", j) <-  read.csv("H:/PP/langdurigVerblijf", j, ".csv")
}
  
datx <- rbind(dat2016, dat2017, dat2018, dat2019, dat2020, dat2021, dat2022, dat2023)
write.csv2(datx,"H:/Data proces/src/projecten/KS kraamzorg indicator/langdurigVerblijf1623.csv")

