save_data <- function(data, naam) {
  datum <- str_remove_all(Sys.Date(), "-")
  
  write.csv2(data, str_c(output_map, "/", datum, "_", naam, ".csv"))
}