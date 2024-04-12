#Inkomensbron 

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Relevant jaren
#jaren <- c(2018:2019)

#------------------------------------------------------------------------------
# persoon data
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/InkomenBestedingen/SECMBUS", pattern = ".SAV", recursive = TRUE, full.names = TRUE)

data_locatie <- data_locatie[length(data_locatie)]

#Inlezen datasets met alleen relevante kolommen
data <- read_spss(data_locatie,  col_select = c(RINPERSOONS, RINPERSOON, AANVSECM, EINDSECM, SECM))

data <- data[ !data$EINDSECM < as.numeric(paste0(jaren[1], "0101")),]
SECM <- data.table(data)
rm(data)


#------------------------------------------------------------------------------
# Omzetten data
#------------------------------------------------------------------------------
# 1= geen inkomen; 2= ontvanger uitkering/sociale voorziening; 3= scholier/student, 4= Inkomen uit arbeid
SECM[SECM == "11", SECM4cat := 4]
SECM[SECM == "12", SECM4cat := 4]
SECM[SECM == "13", SECM4cat := 4]
SECM[SECM == "14", SECM4cat := 4]
SECM[SECM == "15", SECM4cat := 4]
SECM[SECM == "21", SECM4cat := 2]
SECM[SECM == "22", SECM4cat := 2]
SECM[SECM == "23", SECM4cat := 2]
SECM[SECM == "24", SECM4cat := 2]
SECM[SECM == "25", SECM4cat := 2]
SECM[SECM == "26", SECM4cat := 3]
SECM[SECM == "31", SECM4cat := 3]
SECM[SECM == "32", SECM4cat := 1]

#------------------------------------------------------------------------------
# Meest voortkomende bron van inkomen meest per jaar
#------------------------------------------------------------------------------

SECMBUS <- data.table()

## Per jaar: 2014, 2015, 2016, 2017, ....  kijken welke sociaaleconomische categorie een 
## persoon het meeste had.

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

for(year in jaren){
SECM$SECM4cat <- as.character(SECM$SECM4cat)
SECMBUS2 <- SECM %>% mutate(jan = ifelse(AANVSECM <= as.numeric(paste0(year, "0101")) & EINDSECM >= as.numeric(paste0(year, "0102")), SECM4cat, NA),
                               feb = ifelse(AANVSECM <= as.numeric(paste0(year, "0201")) & EINDSECM >= as.numeric(paste0(year, "0202")), SECM4cat, NA),
                               mrt = ifelse(AANVSECM <= as.numeric(paste0(year, "0301")) & EINDSECM >= as.numeric(paste0(year, "0302")), SECM4cat, NA),
                               apr = ifelse(AANVSECM <= as.numeric(paste0(year, "0401")) & EINDSECM >= as.numeric(paste0(year, "0402")), SECM4cat, NA),
                               mei = ifelse(AANVSECM <= as.numeric(paste0(year, "0501")) & EINDSECM >= as.numeric(paste0(year, "0502")), SECM4cat, NA),
                               jun = ifelse(AANVSECM <= as.numeric(paste0(year, "0601")) & EINDSECM >= as.numeric(paste0(year, "0602")), SECM4cat, NA),
                               jul = ifelse(AANVSECM <= as.numeric(paste0(year, "0701")) & EINDSECM >= as.numeric(paste0(year, "0702")), SECM4cat, NA),
                               aug = ifelse(AANVSECM <= as.numeric(paste0(year, "0801")) & EINDSECM >= as.numeric(paste0(year, "0802")), SECM4cat, NA),
                               sep = ifelse(AANVSECM <= as.numeric(paste0(year, "0901")) & EINDSECM >= as.numeric(paste0(year, "0902")), SECM4cat, NA),
                               okt = ifelse(AANVSECM <= as.numeric(paste0(year, "1001")) & EINDSECM >= as.numeric(paste0(year, "1002")), SECM4cat, NA),
                               nov = ifelse(AANVSECM <= as.numeric(paste0(year, "1101")) & EINDSECM >= as.numeric(paste0(year, "1102")), SECM4cat, NA),
                               dec = ifelse(AANVSECM <= as.numeric(paste0(year, "1201")) & EINDSECM >= as.numeric(paste0(year, "1202")), SECM4cat, NA))

SECMBUS2 <- SECMBUS2 %>% mutate(SECM = ifelse(!is.na(jan) | !is.na(feb) | !is.na(mrt) | !is.na(apr) | !is.na(mei) | !is.na(jun) | !is.na(jul)| !is.na(aug) | !is.na(sep) | !is.na(nov) | !is.na(dec), '1', NA))
SECMBUS2 <- SECMBUS2 %>% filter(SECM == '1')
SECMBUS2 <- SECMBUS2 %>% select(-AANVSECM, -EINDSECM, -SECM4cat)
SECMBUS2 <- SECMBUS2 %>% mutate(jan = as.numeric(jan), feb = as.numeric(feb), mrt = as.numeric(mrt), apr = as.numeric(apr), mei = as.numeric(mei), jun = as.numeric(jun), jul = as.numeric(jul), aug = as.numeric(aug), sep = as.numeric(sep), okt = as.numeric(okt), nov = as.numeric(nov), dec = as.numeric(dec))
SECMBUS2[is.na(SECMBUS2)] = 0
SECMBUS2 <- SECMBUS2 %>% group_by(RINPERSOONS, RINPERSOON) %>% summarise(jan = max(jan), feb = max(feb), mrt = max(mrt), apr = max(apr), mei = max(mei), jun = max(jun), jul = max(jul), aug = max(aug), sep = max(sep), okt = max(okt), nov= max(nov), dec = max(dec)) %>% ungroup()

SECMBUS2$SECM <- apply(SECMBUS2[3:14], 1, Mode)
SECMBUS2 <- data.table(SECMBUS2)
SECMBUS2[, jaar := year]


SECMBUS2[,3:15][SECMBUS2[,3:15] == 0] <- NA


SECMBUS <- rbind(SECMBUS, SECMBUS2)


}


SECMBUS_filt <- SECMBUS %>% select(RINPERSOONS, RINPERSOON, jaar, SECM)
SECMBUS_filt <- unique(SECMBUS_filt)

#------------------------------------------------------------------------------
# Wijzigen van bron/baan per jaar
#------------------------------------------------------------------------------

#jaar einde contract/wisseling
SECM <- data.table(SECM)
SECM[, jaarEind := substr(EINDSECM, 1,4)]
SECM2 <- SECM %>% group_by(RINPERSOONS, RINPERSOON) %>% count(jaarEind)
SECM2 <- plyr::rename(SECM2, c("n" = "nBaanwissel"))
SECM2 <- plyr::rename(SECM2, c("jaarEind" = "jaar"))
SECM2 <- data.table(SECM2)
SECM2[, jaar := as.double(jaar)]



rm(SECM,  SECMBUS2)

#jaar terugkijken (2020 wordt 2021 in finale set)
SECM2[, jaar := jaar+1]
SECMBUS_filt[, jaar := jaar+1]


