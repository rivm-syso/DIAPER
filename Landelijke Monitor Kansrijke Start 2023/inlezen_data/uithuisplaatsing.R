#uituisplaatsing vaststellen

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#jeugd data inlezen vanuit hulp en bescherming
#------------------------------------------------------------------------------
jeugd_bescherming <- inlezen_data("G:/VeiligheidRecht/JGDBESCHERMBUS", 
                                  c("RINPERSOONS", "RINPERSOON", "JGDBMAATREGEL",
                                    "JGDBGELDIGHEIDBEGINDATUM", "JGDBGELDIGHEIDEINDDATUM"))

jeugd_hulp <- inlezen_data("G:/GezondheidWelzijn/JGDHULPBUS", 
                           c("RINPERSOONS", "RINPERSOON", "JGDHHULPVORM",
                             "JGDHGELDIGHEIDBEGINDATUM", "JGDHGELDIGHEIDEINDDATUM"))

#------------------------------------------------------------------------------
#datasets combineren
#------------------------------------------------------------------------------
#alle mogelijke combinaties meenemen, sommige personen komen meerdere keren 
#voor in ??n of beide bestanden
uithuis <- merge(jeugd_bescherming, jeugd_hulp, all = TRUE, 
                 by = c("RINPERSOONS", "RINPERSOON", "jaar"))

#------------------------------------------------------------------------------
#opschonen
#------------------------------------------------------------------------------
uithuis[, ':=' (
  JGDBGELDIGHEIDBEGINDATUM = as.numeric(as.character(JGDBGELDIGHEIDBEGINDATUM)),
  JGDBGELDIGHEIDEINDDATUM = as.numeric(as.character(JGDBGELDIGHEIDEINDDATUM)),
  JGDHGELDIGHEIDBEGINDATUM = as.numeric(as.character(JGDHGELDIGHEIDBEGINDATUM)),
  JGDHGELDIGHEIDEINDDATUM = as.numeric(as.character(JGDHGELDIGHEIDEINDDATUM))
)]

#------------------------------------------------------------------------------
#variabele aanmaken
#------------------------------------------------------------------------------
#proxy voor uithuisplaatsing: gelijktijdig van jeugdbescherming (OTS of voogdij) en jeugdhulp met verblijf
uithuis[, "uithuis" := ifelse(JGDHHULPVORM %in% c(11:14) & JGDBMAATREGEL %in% c(11, 12, 21, 22, 23) & #relevante rijen in beide datasets
                                (JGDBGELDIGHEIDBEGINDATUM <= JGDHGELDIGHEIDEINDDATUM | 
                                   JGDHGELDIGHEIDBEGINDATUM <= JGDBGELDIGHEIDEINDDATUM) #als een van de twee begint door de andere eindigt, dan overlappen ze
                              , 1, 0)]

#dubbele rijen per jaar opruimen
uithuis <- unique(uithuis[uithuis == 1], by = c("RINPERSOONS", "RINPERSOON", "jaar"))

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
uithuis[, c("JGDBGELDIGHEIDBEGINDATUM", "JGDBGELDIGHEIDEINDDATUM", 
            "JGDHGELDIGHEIDBEGINDATUM", "JGDHGELDIGHEIDEINDDATUM",
            "JGDBMAATREGEL", "JGDHHULPVORM") := NULL]

rm(jeugd_bescherming, jeugd_hulp)
