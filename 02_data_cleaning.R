library(tidyverse)
library(janitor)
library(skimr)

# load data created in 01_data_conversion.R ----------------------------------------------------------------------

full_file <- read_csv("data/merged_allyears.csv")

# select data for rural and household members --------------------------------------------------------------------

# first identify households with personal domestico
full_file <- 
  full_file %>% 
  group_by(upm, nvivi, nhoga, year) %>% 
  mutate(hh_personal_domestico = max(as.numeric(relacion_parentesco==11))) %>% 
  ungroup

# drop urban areas and non-household members
full_file <- 
  full_file %>% 
  filter(area == 6 & relacion_parentesco<=10)

# national poverty rate check @ ----------------------------------------------------------------------------
# https://www.ine.gov.py/Publicaciones/Biblioteca/documento/8613_Presentaci%C3%B3n%20_pobreza%20monetaria_EPHC%202020.pdf

povrate_rural <- 
  full_file %>% 
  group_by(year) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = fex))

# descriptive statistics --------------------------------------------------------------------------

skim(full_file)

# data cleaning ----------------------------------------------------------------------------------

full_file <-   
  full_file %>% 
  mutate(jefe = as.numeric(relacion_parentesco==1),
         female = as.numeric(sexo==6),
         tiene_trabajo = as.numeric(trabajo_7dias==1 | trabajo_1hr==1 | trabajo_no_realizado==1),
         quiere_trabajo = as.numeric(trabajo_si_ofrecido==1),
         tipo_empleo = replace(tipo_empleo, is.na(tipo_empleo), 99),
         tiene_trabajo = replace(tiene_trabajo, edad<10, 0),
         quiere_trabajo = replace(quiere_trabajo, edad<10 | tiene_trabajo==1, 0),
         inactive = as.numeric(tiene_trabajo==0 & quiere_trabajo==0),
         aporta_caja_jubilacion1 = replace(aporta_caja_jubilacion1, is.na(aporta_caja_jubilacion1), 0))

## Jefe ------------------------------------------------------------------------------------------

full_file <- 
  full_file %>% 
  group_by(upm, nvivi, nhoga, year) %>% 
  mutate(jefe_female = max(as.numeric(jefe==1 & female==1)),
         jefe_edad = max(if_else(jefe==1, edad, NA_real_), na.rm=TRUE),
         jefe_idioma = max(if_else(jefe==1, idioma, NA_real_), na.rm=TRUE),
         jefe_aniosestudio = max(if_else(jefe==1, anios_estudio, NA_real_), na.rm = TRUE),
         jefe_estado_civil = max(if_else(jefe==1, estado_civil, NA_real_), na.rm = TRUE),
         jefe_seguro_medico = max(if_else(jefe==1, tiene_seguro, NA_real_), na.rm = TRUE),
         jefe_sabe_leer_escribir = max(if_else(jefe==1, sabe_leer_escribir, NA_real_), na.rm = TRUE),
         jefe_tipo_empleo = max(if_else(jefe==1, tipo_empleo, NA_real_), na.rm = TRUE),
         jefe_tiene_trabajo = max(as.numeric(jefe==1 & tiene_trabajo==1)),
         jefe_quiere_trabajo = max(as.numeric(jefe==1 & quiere_trabajo==1)),
         jefe_caja_jubilacion1 = max(if_else(jefe==1, aporta_caja_jubilacion1, NA_real_), na.rm = TRUE))  %>%
  ungroup

## hh composition ----------------------------------------------------------------------------------
full_file <- 
  full_file %>% 
  group_by(upm, nvivi, nhoga, year) %>% 
  mutate(hh_conyuge_present = max(as.numeric(relacion_parentesco==2)),
         hh_tipo_hogar = tipo_hogar,
         hh_miembros_5ymenos = sum(as.numeric(edad<6)),
         hh_miembros_6a14 = sum(as.numeric(edad>=6 & edad<15)),
         hh_miembros_15a64 = sum(as.numeric(edad>=15 & edad<65)),
         hh_miembros_65ymas = sum(as.numeric(edad>=65)),
         hh_dependents = hh_miembros_5ymenos + hh_miembros_6a14 + hh_miembros_65ymas,
         hh_youth_dependents = hh_miembros_5ymenos + hh_miembros_6a14,
         hh_old_dependents = hh_miembros_65ymas,
         hh_females = sum(as.numeric(female==1)),
         hh_males = sum(as.numeric(female==0)),
         hh_tekopora = max(as.numeric(ing_tekopora>0)),
         hh_totpers = totpers) %>%
  ungroup

## vivienda características --------------------------------------------------------------------------------
vivi_vars <- Hmisc::Cs(tipo_combustible, tipo_agua_fuente, tipo_agua_fuente_beber, 
                       tipo_banho_desague, tiene_pieza_cocinar, tiene_tv_cable,
                       tiene_horno_microondas, tiene_horno_electrico, tiene_auto_camion,
                       tiene_videoDVD, tiene_termocalefon, tiene_acondicionador_aire,
                       tiene_motocicleta, tiene_antena_parabolica, tipo_vivienda_propiedad, 
                       tipo_agua_proveedor_beber, tipo_basura, tipo_pared, tipo_piso, tipo_techo,
                       tipo_vivienda, tipo_agua_proveedor, tiene_luz_electrica, tiene_linea_fija,
                       tiene_celular, tiene_banho, tipo_lote_propiedad, tiene_computadora,
                       tiene_tableta, tiene_internet, tiene_radio, tiene_televisor, tiene_heladera,
                       tiene_cocina_gas, tiene_cocina_elec, tiene_lavarropas, nro_piezas, nro_dormitorios)

full_file <- 
  full_file %>% 
  rename_at(vars(vivi_vars), ~gsub("tipo|tiene|nro", "vivi", .))

full_file <- 
  full_file %>% 
  select(upm, nvivi, nhoga, year, fex, facpob, area, dptorep, ipcm, linea_pobreza_total, 
         linea_pobreza_extrema, starts_with("vivi_"), starts_with("hh_"), starts_with("jefe_")) %>% 
  distinct(upm, nvivi, nhoga, year, .keep_all = TRUE)

# check and skim again ---------------------------------------------------------------------------------------------------------

full_file %>% 
  group_by(year) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = facpob))

skim(full_file)

# save household file --------------------------------------------------------------------------------------------------

write_csv(full_file, "data/hh_merged_allyears.csv")

# Session Info -----------------------------------------------------------------------------------------------------------

renv::snapshot()
