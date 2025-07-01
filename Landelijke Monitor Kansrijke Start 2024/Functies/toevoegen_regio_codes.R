#functie voor het koppelen van regio's
toevoegen_regio_codes <- function(x){
  regio_mapping <- fread("H:/Data proces/data/20230608_map_gem_regio_gem2022.csv") %>% 
    rename("gem2022" = "geo_id")
  
  nl_data <- x %>% #Nederland apart, anders worden mensen zonder gemeente geexcludeerd
    select(-gem2022) %>% 
    group_by(across(-aantal)) %>% 
    summarise(aantal = sum(aantal, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(gem2022 = "NL01")
  
  regio_data <- x %>% 
    inner_join(regio_mapping, by = "gem2022", relationship = "many-to-many") %>% 
    select(-gem2022) %>% 
    rename("gem2022" = "geo_id_map") %>% 
    group_by(across(-aantal)) %>% 
    summarise(aantal = sum(aantal, na.rm = TRUE)) %>%
    ungroup() %>% 
    filter(gem2022 != "NL01")
  
  x <- rbindlist(list(x, regio_data, nl_data), use.names = TRUE)
  
  return(x)
}
