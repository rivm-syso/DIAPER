#ZVW zorgkosten, GGZ
#Dataset: ZVWZORGKOSTENTAB

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
#vanaf 2022 zijn de variabelen basis en specialistische ggz niet meer gevuld, en vervangen door algemene ggz variabele (deze is echter pas beschikbaar vanaf '22)

kostenGGZ <- data.frame()

jarenPre22 <- lapply(jaren, function(x) if ( x < 2022) x )
jarenPost22 <- lapply(jaren, function(x) if ( x >= 2022) x )
jarenPre22 <- Filter(Negate(is.null), jarenPre22)
jarenPost22 <- Filter(Negate(is.null), jarenPost22)



  if(!is.null(jarenPre22)){
    jaren = jarenPre22
    years = jarenPre22
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
ggz_kosten <- ggz_kosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "kosten_GGZ")]
kostenGGZ <- rbind(kostenGGZ, ggz_kosten)
}  

if(!is.null(jarenPost22)){
          jaren = jarenPost22        
          years = jarenPost22
    ggz_kosten <- inlezen_data("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB", 
                               kolommen = c("RINPERSOONS", "RINPERSOON", "ZVWKGGZZPMTOTAAL"))
    
    #GGZ kosten
    setnames(
      ggz_kosten,
      c("ZVWKGGZZPMTOTAAL"),
      c("kosten_GGZ")
    )
    
    ggz_kosten[, ':=' (
      kosten_GGZ = ifelse(kosten_GGZ > 0, 1, 0)
    )]
    
    #overige kolommen verwijderen
    ggz_kosten <- ggz_kosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "kosten_GGZ")]
    kostenGGZ <- rbind(kostenGGZ, ggz_kosten)
    
                             }
#weer  juiste list met jaren
jaren <- c(jarenPre22, jarenPost22)

rm(jarenPre22, jarenPost22, ggz_kosten)

#------------------------------------------------------------------------------
#gegevens opslaan in een temp map
#------------------------------------------------------------------------------
write.csv2(kostenGGZ, "temp/ggzkostenKS.csv")
