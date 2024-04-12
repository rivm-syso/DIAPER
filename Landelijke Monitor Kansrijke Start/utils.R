#jaren aangeven
jaren <- c(2015:2022)

#gemeente indeling aangeven
gemeente_indeling <- "gem2022"

#output map
output_map <- "projecten/ks factsheet landelijk/output"

#Packages
library(haven)
library(data.table)
library(stringr)
library(tidyverse)
library(labelled)
library(readxl)
library(purrr)

#inlezen functies
source("functies/inlezen_data.R")
source("functies/toevoegen_regios.R")
source("functies/save_data.R")

