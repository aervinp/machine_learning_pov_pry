library(tidyverse)

survey_years <- c(2018:2020)

for (i in 1:length(survey_years)) {

# Read in the dta files ------------------------------------------------------------
  
assign(paste0("reg01_", survey_years[i]), 
       haven::read_dta(paste0("data/EPH_", survey_years[i], "/reg01_ephc", survey_years[i], ".dta"),
                       encoding = "UTF-8"))

assign(paste0("reg02_", survey_years[i]), 
       haven::read_dta(paste0("data/EPH_", survey_years[i], "/reg02_ephc", survey_years[i], ".dta"),
                       encoding = "UTF-8"))

assign(paste0("ingrefam_", survey_years[i]), 
       haven::read_dta(paste0("data/EPH_", survey_years[i], "/ingrefam_ephc", survey_years[i], ".dta"),
                       encoding = "UTF-8"))

}

# 2018 Harmonize --------------------------------------------------------------------

reg02_2018_select <- 
  reg02_2018 %>%
  rename(edad = p02,
         relacion_parentesco = p03,
         es_miembro_hogar = p04,
         sexo = p06,
         estado_civil = p09,
         tiene_seguro = s01a,
         trabajo_7dias = a02,
         trabajo_1hr = a03,
         trabajo_no_realizado = a04,
         trabajo_si_ofrecido = a05,
         busco_trabajo_7dias = a07,
         busco_trabajo_30dias = a08,
         razon_dejo_ocupacion = a18,
         tipo_empleo = b12,
         rama_ocupacion = b02rec,
         aporta_caja_jubilacion1 = b10,
         aporta_caja_jubilacion2 = c07,
         idioma = ed01,
         sabe_leer_escribir = ed02,
         anios_estudio = añoest,
         razon_inactividad = ra06ya09) %>%
  select(upm,nvivi,nhoga,edad,relacion_parentesco,es_miembro_hogar,sexo,
         estado_civil,tiene_seguro,trabajo_7dias,trabajo_1hr,
         trabajo_no_realizado,trabajo_si_ofrecido,busco_trabajo_7dias,
         busco_trabajo_30dias,razon_dejo_ocupacion,tipo_empleo,rama_ocupacion,
         aporta_caja_jubilacion1,aporta_caja_jubilacion2,idioma,
         sabe_leer_escribir,anios_estudio,razon_inactividad) 

reg01_2018_select <- 
  reg01_2018 %>% 
  rename(tipo_vivienda = v01,
         nro_piezas = v02a,
         nro_dormitorios = v02b,
         tipo_pared = v03,
         tipo_piso = v04,
         tipo_techo = v05,
         tipo_agua_proveedor = v06,
         tipo_agua_fuente = v07a,
         tipo_agua_proveedor_beber = v08,
         tipo_agua_fuente_beber = v09,
         tiene_luz_electrica = v10,
         tiene_linea_fija = v11a,
         tiene_celular = v11b,
         tiene_banho = v12,
         tipo_banho_desague = v13,
         tiene_pieza_cocinar = v14a,
         tipo_combustible = v14b,
         tipo_basura = v15,
         tipo_vivienda_propiedad = v16,
         tipo_lote_propiedad = v17,
         tiene_computadora = v23a1,
         tiene_tableta = v23a2,
         tiene_internet = v23b,
         tiene_radio = v2401,
         tiene_televisor = v2402,
         tiene_heladera = v2403,
         tiene_cocina_gas = v2404,
         tiene_cocina_elec = v2404a,
         tiene_lavarropas = v2405,
         tiene_videoDVD = v2406,
         tiene_termocalefon = v2407,
         tiene_acondicionador_aire = v2408,
         tiene_antena_parabolica = v2409,
         tiene_tv_cable = v2410,
         tiene_horno_microondas = v2411,
         tiene_horno_electrico = v2412,
         tiene_auto_camion = v2413,
         tiene_motocicleta = v2414,
         tipo_hogar = thogav) %>%
  select(upm,nvivi,nhoga, area, tipo_vivienda,nro_piezas,nro_dormitorios,
         tipo_pared,tipo_piso,tipo_techo,tipo_agua_proveedor,tipo_agua_fuente,
         tipo_agua_proveedor_beber,tipo_agua_fuente_beber,tiene_luz_electrica,
         tiene_linea_fija,tiene_celular,tiene_banho,tipo_banho_desague,
         tiene_pieza_cocinar,tipo_combustible,tipo_basura,tipo_vivienda_propiedad,
         tipo_lote_propiedad,tiene_computadora,tiene_tableta,tiene_internet,
         tiene_radio,tiene_televisor,tiene_heladera,tiene_cocina_gas,tiene_cocina_elec,
         tiene_lavarropas,tiene_videoDVD,tiene_termocalefon,tiene_acondicionador_aire,
         tiene_antena_parabolica,tiene_tv_cable,tiene_horno_microondas,tiene_horno_electrico,
         tiene_auto_camion,tiene_motocicleta,tipo_hogar)
  
ingrefam_2018_select <- 
  ingrefam_2018 %>%
  rename(ing_tekopora = e01hde,
         linea_pobreza_total = linpobto,
         linea_pobreza_extrema = linpobex) %>%
  select(upm,nvivi,nhoga,fex,facpob, dptorep, totpers,ipcm,ing_tekopora,
         linea_pobreza_total,linea_pobreza_extrema)

file_2018 <- 
  reg02_2018_select %>%
  left_join(reg01_2018_select, by = c("upm", "nvivi", "nhoga")) %>%
  left_join(ingrefam_2018_select, by = c("upm", "nvivi", "nhoga"))  %>%
  mutate(year = 2018)

# 2019 Harmonize -------------------------------------------------------------------------

reg02_2019_select <- 
  reg02_2019 %>%
  rename(edad = p02,
         relacion_parentesco = p03,
         es_miembro_hogar = p04,
         sexo = p06,
         estado_civil = p09,
         tiene_seguro = s01a,
         trabajo_7dias = a02,
         trabajo_1hr = a03,
         trabajo_no_realizado = a04,
         trabajo_si_ofrecido = a05,
         busco_trabajo_7dias = a07,
         busco_trabajo_30dias = a08,
         razon_dejo_ocupacion = a18,
         tipo_empleo = b12,
         rama_ocupacion = b02rec,
         aporta_caja_jubilacion1 = b10,
         aporta_caja_jubilacion2 = c07,
         idioma = ed01,
         sabe_leer_escribir = ed02,
         anios_estudio = añoest,
         razon_inactividad = ra06ya09) %>%
  select(upm,nvivi,nhoga,edad,relacion_parentesco,es_miembro_hogar,sexo,
         estado_civil,tiene_seguro,trabajo_7dias,trabajo_1hr,
         trabajo_no_realizado,trabajo_si_ofrecido,busco_trabajo_7dias,
         busco_trabajo_30dias,razon_dejo_ocupacion,tipo_empleo,rama_ocupacion,
         aporta_caja_jubilacion1,aporta_caja_jubilacion2,idioma,
         sabe_leer_escribir,anios_estudio,razon_inactividad) 

reg01_2019_select <- 
  reg01_2019 %>% 
  rename(tipo_vivienda = v01,
         nro_piezas = v02a,
         nro_dormitorios = v02b,
         tipo_pared = v03,
         tipo_piso = v04,
         tipo_techo = v05,
         tipo_agua_proveedor = v06,
         tipo_agua_fuente = v07a,
         tipo_agua_proveedor_beber = v08,
         tipo_agua_fuente_beber = v09,
         tiene_luz_electrica = v10,
         tiene_linea_fija = v11a,
         tiene_celular = v11b,
         tiene_banho = v12,
         tipo_banho_desague = v13,
         tiene_pieza_cocinar = v14a,
         tipo_combustible = v14b,
         tipo_basura = v15,
         tipo_vivienda_propiedad = v16,
         tipo_lote_propiedad = v17,
         tiene_computadora = v23a1,
         tiene_tableta = v23a2,
         tiene_internet = v23b,
         tiene_radio = v2401,
         tiene_televisor = v2402,
         tiene_heladera = v2403,
         tiene_cocina_gas = v2404,
         tiene_cocina_elec = v2404a,
         tiene_lavarropas = v2405,
         tiene_videoDVD = v2406,
         tiene_termocalefon = v2407,
         tiene_acondicionador_aire = v2408,
         tiene_antena_parabolica = v2409,
         tiene_tv_cable = v2410,
         tiene_horno_microondas = v2411,
         tiene_horno_electrico = v2412,
         tiene_auto_camion = v2413,
         tiene_motocicleta = v2414,
         tipo_hogar = thogav) %>%
  select(upm,nvivi,nhoga, area, tipo_vivienda,nro_piezas,nro_dormitorios,
         tipo_pared,tipo_piso,tipo_techo,tipo_agua_proveedor,tipo_agua_fuente,
         tipo_agua_proveedor_beber,tipo_agua_fuente_beber,tiene_luz_electrica,
         tiene_linea_fija,tiene_celular,tiene_banho,tipo_banho_desague,
         tiene_pieza_cocinar,tipo_combustible,tipo_basura,tipo_vivienda_propiedad,
         tipo_lote_propiedad,tiene_computadora,tiene_tableta,tiene_internet,
         tiene_radio,tiene_televisor,tiene_heladera,tiene_cocina_gas,tiene_cocina_elec,
         tiene_lavarropas,tiene_videoDVD,tiene_termocalefon,tiene_acondicionador_aire,
         tiene_antena_parabolica,tiene_tv_cable,tiene_horno_microondas,tiene_horno_electrico,
         tiene_auto_camion,tiene_motocicleta,tipo_hogar)

ingrefam_2019_select <- 
  ingrefam_2019 %>%
  rename(ing_tekopora = e01hde,
         linea_pobreza_total = linpobto,
         linea_pobreza_extrema = linpobex) %>%
  select(upm,nvivi,nhoga,fex,facpob, dptorep, totpers,ipcm,ing_tekopora,
         linea_pobreza_total,linea_pobreza_extrema)

file_2019 <- 
  reg02_2019_select %>%
  left_join(reg01_2019_select, by = c("upm", "nvivi", "nhoga")) %>%
  left_join(ingrefam_2019_select, by = c("upm", "nvivi", "nhoga")) %>%
  mutate(year = 2019)

# 2020 Harmonize -------------------------------------------------------------------------

reg02_2020_select <- 
  reg02_2020 %>%
  rename(edad = p02,
         relacion_parentesco = p03,
         es_miembro_hogar = p04,
         sexo = p06,
         estado_civil = p09,
         tiene_seguro = s01a,
         trabajo_7dias = a02,
         trabajo_1hr = a03,
         trabajo_no_realizado = a04,
         trabajo_si_ofrecido = a05,
         busco_trabajo_7dias = a07,
         busco_trabajo_30dias = a08,
         razon_dejo_ocupacion = a18,
         tipo_empleo = b12,
         rama_ocupacion = b02rec,
         aporta_caja_jubilacion1 = b10,
         aporta_caja_jubilacion2 = c07,
         idioma = ed01,
         sabe_leer_escribir = ed02,
         anios_estudio = añoest,
         razon_inactividad = ra06ya09) %>%
  select(upm,nvivi,nhoga,edad,relacion_parentesco,es_miembro_hogar,sexo,
         estado_civil,tiene_seguro,trabajo_7dias,trabajo_1hr,
         trabajo_no_realizado,trabajo_si_ofrecido,busco_trabajo_7dias,
         busco_trabajo_30dias,razon_dejo_ocupacion,tipo_empleo,rama_ocupacion,
         aporta_caja_jubilacion1,aporta_caja_jubilacion2,idioma,
         sabe_leer_escribir,anios_estudio,razon_inactividad) 

reg01_2020_select <- 
  reg01_2020 %>% 
  rename(tipo_vivienda = v01,
         nro_piezas = v02a,
         nro_dormitorios = v02b,
         tipo_pared = v03,
         tipo_piso = v04,
         tipo_techo = v05,
         tipo_agua_proveedor = v06,
         tipo_agua_fuente = v07a,
         tipo_agua_proveedor_beber = v08,
         tipo_agua_fuente_beber = v09,
         tiene_luz_electrica = v10,
         tiene_linea_fija = v11a,
         tiene_celular = v11b,
         tiene_banho = v12,
         tipo_banho_desague = v13,
         tiene_pieza_cocinar = v14a,
         tipo_combustible = v14b,
         tipo_basura = v15,
         tipo_vivienda_propiedad = v16,
         tipo_lote_propiedad = v17,
         tiene_computadora = v23a1,
         tiene_tableta = v23a2,
         tiene_internet = v23b,
         tiene_radio = v2401,
         tiene_televisor = v2402,
         tiene_heladera = v2403,
         tiene_cocina_gas = v2404,
         tiene_cocina_elec = v2404a,
         tiene_lavarropas = v2405,
         tiene_videoDVD = v2406,
         tiene_termocalefon = v2407,
         tiene_acondicionador_aire = v2408,
         tiene_antena_parabolica = v2409,
         tiene_tv_cable = v2410,
         tiene_horno_microondas = v2411,
         tiene_horno_electrico = v2412,
         tiene_auto_camion = v2413,
         tiene_motocicleta = v2414,
         tipo_hogar = thogav) %>%
  select(upm,nvivi,nhoga, area, tipo_vivienda,nro_piezas,nro_dormitorios,
         tipo_pared,tipo_piso,tipo_techo,tipo_agua_proveedor,tipo_agua_fuente,
         tipo_agua_proveedor_beber,tipo_agua_fuente_beber,tiene_luz_electrica,
         tiene_linea_fija,tiene_celular,tiene_banho,tipo_banho_desague,
         tiene_pieza_cocinar,tipo_combustible,tipo_basura,tipo_vivienda_propiedad,
         tipo_lote_propiedad,tiene_computadora,tiene_tableta,tiene_internet,
         tiene_radio,tiene_televisor,tiene_heladera,tiene_cocina_gas,tiene_cocina_elec,
         tiene_lavarropas,tiene_videoDVD,tiene_termocalefon,tiene_acondicionador_aire,
         tiene_antena_parabolica,tiene_tv_cable,tiene_horno_microondas,tiene_horno_electrico,
         tiene_auto_camion,tiene_motocicleta,tipo_hogar)

ingrefam_2020_select <- 
  ingrefam_2020 %>%
  rename(ing_tekopora = e01hde,
         linea_pobreza_total = linpobto,
         linea_pobreza_extrema = linpobex) %>%
  select(upm,nvivi,nhoga,fex,facpob, dptorep, totpers,ipcm,ing_tekopora,
         linea_pobreza_total,linea_pobreza_extrema)

file_2020 <- 
  reg02_2020_select %>%
  left_join(reg01_2020_select, by = c("upm", "nvivi", "nhoga")) %>%
  left_join(ingrefam_2020_select, by = c("upm", "nvivi", "nhoga")) %>%
  mutate(year = 2020)

# remove unneeded files

rm(list = objects()[str_starts(objects(), pattern = "file_", negate = TRUE)])

# Name check --------------------------------------------------------------------

setdiff(names(file_2018), names(file_2019))
setdiff(names(file_2018), names(file_2020))
setdiff(names(file_2019), names(file_2020))


# merge full file ----------------------------------------------------------------
full_file <- 
  file_2018 %>%
  bind_rows(file_2019) %>%
  bind_rows(file_2020)

rm(list = objects()[str_starts(objects(), pattern = "file_")])

write_csv(full_file, "data/merged_allyears.csv")

# snapshot packages --------------------------------------------------------------
renv::snapshot()

