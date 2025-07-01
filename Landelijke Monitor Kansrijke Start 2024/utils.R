#jaren aangeven
jaren <- c(2017:2023)

#gemeente indeling aangeven
gemeente_indeling <- "gem2025"

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
