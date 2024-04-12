# Landelijke Monitor Kansrijke Start 2023
In deze map staan de R-scripts die zijn gebruikt voor het berekenen van de indicatoren van de Landelijke Monitor Kansrijke Start 2023.

## Running the code
Het is niet mogelijk om lokaal de scripts te runnen.

De R-scripts om de indicatoren te berekenen runnen wij in de omgeving waar de data staat, de Remote Access omgeving van CBS Microdata. Zie de [README in de hoofdmap DIAPER](./../README.md) voor meer informatie en hoe je toegang kunt aanvragen.

## Indeling code
De code is verdeeld in de mappen:
- indicatoren: map met de hoofdscripts
- inlezen_data: map met scripts de gebruikt worden om de data in te lezen
- functies: map met gebruikte functies
- utils.R: script met utilities, zoals packages en een handige algemene inlees functie

## Leeswijzer - berekening indicator
Open het R-script van de indicator die je wilt bestuderen. Deze staan in de map indicatoren.
 
Het indicator script heeft de regie over het verzamelen van de onderliggende gegevens (inlezen_data), het uitvoeren van aanvullende berekeningen en het bepalen van de teller en de noemer van de indicator. Hiervoor worden de scripts in de andere mappen gebruikt.

## Data
De indicatoren van de Landelijke Monitor Kansrijke Start 2023 zijn bepaald op basis van de volgende data tabellen.

### CBS microdata 
- ZVWZORGKOSTENTAB
- GBALEVENDGEBORENENMASSATAB
- GBAPERSOONTAB
- KINDOUDERTAB
- GBAADRESOBJECTBUS
- VSLPOSTCODEBUS
- VSLGWBTAB
- DGGBAPERSOONTAB
- INHATAB
- HOOGSTEOPLTAB
- OPLEIDINGSNRREFV30
- WSNPPERSBUS
- WANBZVWTAB
- JGDBESCHERMBUS
- JGDHULPBUS

### Maatwerk tabellen
- Perined 
- Vektis
