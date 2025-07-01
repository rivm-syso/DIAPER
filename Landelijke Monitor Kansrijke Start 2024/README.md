# Landelijke Monitor Kansrijke Start 2024
Zeven van de achttien indicatoren van de monitor Kansrijke Start 2024 zijn bepaald in DIAPER. 
In deze map staan de R-scripts die hiervoor zijn gebruikt.

## Running the code
Het is niet mogelijk om lokaal de scripts te runnen.

De R-scripts om de indicatoren te berekenen runnen wij in de omgeving waar de data staat, de Remote Access omgeving van CBS Microdata. Zie de [README in de hoofdmap DIAPER](./../README.md) voor meer informatie en hoe je toegang kunt aanvragen.

## Indeling code
De code is verdeeld in mappen:
- Indicatoren: hoofdscripts
- Bevolking opbouw: scripts voor het opbouwen van de onderzoekspopulatie
- Variabelen: scripts die gebruikt worden om de data/variabelen in te lezen
- Functies: handige functies
- utils.R: script met utilities

## Leeswijzer - berekening indicator
Open het R-script van de indicator die je wilt bestuderen. Deze staan in de map Indicatoren.
 
Het indicator script heeft de regie over het verzamelen van de onderliggende gegevens (Variabelen), het uitvoeren van aanvullende berekeningen en het bepalen van de teller en de noemer van de indicator. Het indicator script gebruikt hiervoor de scripts in de andere mappen.

## Data
De indicatoren van de Landelijke Monitor Kansrijke Start 2024 zijn bepaald op basis van de volgende data tabellen.

### CBS microdata 
- GBASTANDBEVOLKINGTAB
- DGGBAPERSOONTAB
- GBALEVENDGEBORENENMASSATAB
- GBAPERSOONTAB
- KINDOUDERTAB
- GBAADRESOBJECTBUS
- INHATAB
- HOOGSTEOPLTAB
- WSNPPERSBUS
- WANBZVWTAB
- MEDICIJNTAB
- ZVWZORGKOSTENTAB
- MSZPRESTATIESVEKTTAB

### Maatwerk tabellen
- Perined 
- Vektis



