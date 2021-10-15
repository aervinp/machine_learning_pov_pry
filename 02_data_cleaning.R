library(tidyverse)
library(janitor)

# load data created in 01_data_conversion.R ----------------------------------------------------------------------

full_file <- read_csv("data/merged_allyears.csv")

# select data for rural and household members --------------------------------------------------------------------

full_file <- 
  full_file %>% 
  filter(area == 6 & relacion_parentesco<=10)

# check versus national poverty rates ----------------------------------------------------------------------------
# https://www.ine.gov.py/Publicaciones/Biblioteca/documento/8613_Presentaci%C3%B3n%20_pobreza%20monetaria_EPHC%202020.pdf

full_file %>% 
  group_by(year) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = fex))

# data cleaning ----------------------------------------------------------------------------------

full_file %>% tabyl(edad)
full_file %>% tabyl(es_miembro_hogar, relacion_parentesco)