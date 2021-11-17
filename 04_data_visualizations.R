library(tidyverse)
library(janitor)
library(tidymodels)

library(heatmaply)
library(gridExtra)

# load household data created in 03_encoding_categorical_numerical.R ----------------------------------------------------------------------

hhfile <- read_rds("data/hh_merged_allyears_cleaned.rds")

# variables to investigate:
# income sources -------------------------------------------------------------------------------------------------------------------------

# demographics ---------------------------------------------------------------------------------------------------------------------------
# people in the house and age, gender
# jefe age and gender (jefe_female_Yes)
# hh_tipo_hogar_Nuclear_Completo

# jefe -----------------------------------------------------------------------------------------------------------------------------------
# jefe_idioma_Castellano
# jefe_estado_civil_Viudo
# Jefe_aniosestudio

# location -------------------------------------------------------------------------------------------------------------------------------
# departments

# housing conditions ---------------------------------------------------------------------------------------------------------------------
# pared
# agua proveedor
# agua proveedor beber
# combustible
# vivi_lote_propiedad
# vivi_piso1_high
# vivi_agua_fuente1_low
# vivi_basura1_high

# assets ---------------------------------------------------------------------------------------------------------------------------------
# celular
# vivi_computadora_No
# vivi_tableta_No
# vivi_internet_Yes
# vivi_radio_No
# vivi_televisor_No
# vivi_cocina_gas_No
# vivi_cocina_elec_No
# vivi_lavarropas_No
# vivi_videoDVD_No
# vivi_termocalefon_Yes
# vivi_acondicionador_aire_No
# vivi_antena_parabolica_Yes
# vivi_tv_cable_No
# vivi_horno_microondas_Yes
# vivi_horno_electrico_No
# vivi_auto_camion_No
# vivi_motocicleta_No

hhfile <- 
  hhfile %>% 
  mutate(jefe_tipo_empleo1 = as_factor(case_when(jefe_tipo_empleo %in% c("Domestico", "Cuenta_propia", "No_trabajo", "No_remunerado") ~ "Low",
                                                 TRUE ~ "High")),
         perc_tiene_trabajo_remunerado = hh_tiene_trabajo_remunerado/(hh_totpers),
         hh_tipo_hogar1 = as_factor(case_when(hh_tipo_hogar=="Unipersonal" ~ "Unipersonal",
                                              TRUE ~ "MoreThanOne")),
         jefe_idioma1 = as_factor(case_when(jefe_idioma %in% c("No_habla", "Guarani") ~ "LowInc",
                                            TRUE ~ "HigherInc")),
         jefe_aniosestudio1 = as_factor(case_when(jefe_aniosestudio %in% c(0:6) ~ "Low",
                                                  jefe_aniosestudio %in% c(6:12) ~ "Medium",
                                                  jefe_aniosestudio>12 ~ "High")),
         jefe_estado_civil1 = as_factor(case_when(jefe_estado_civil %in% c("Casado", "Unido") ~ "Together",
                                                 TRUE ~ "Alone")),
         dptorep1 = as_factor(case_when(dptorep %in% c("Central", "Alto_Parana") ~ "High",
                                        dptorep %in% c("Resto", "San_Pedro", "Itapua") ~ "Medium",
                                        TRUE ~ "Low")),
         vivi_pared1 = as_factor(case_when(vivi_pared %in% c("Madera", "Adobe", "No_tiene") ~ "Low",
                                          TRUE ~ "High")),
         vivi_agua_proveedor1 = as_factor(case_when(vivi_agua_proveedor %in% c("Artesiano", "Lluvia", "ESSAP", 
                                                                              "Privada", "Pozo_bomba") ~ "High",
                                                   TRUE ~ "Low")),
         vivi_agua_proveedor_beber1 = as_factor(case_when(vivi_agua_proveedor_beber %in% c("Artesiano", "Embotellada", "Lluvia", "ESSAP", 
                                                                                           "Privada", "Manantial_protegido", "Pozo_protegido") ~ "High",
                                                    TRUE ~ "Low")),
         vivi_combustible1 = as_factor(case_when(vivi_combustible %in% c("Ninguno", "Gas", "Electricidad") ~ "High",
                                                 TRUE ~ "Low")),
         vivi_lote_propiedad1 = as_factor(case_when(vivi_lote_propiedad=="Propio" ~ "Propio",
                                                    TRUE ~ "Other")),
         vivi_piso1 = as_factor(case_when(vivi_piso %in% c("Porcelanato", "Parquet", "Otro", "Baldosa", "Madera") ~ "high",
                                          vivi_piso %in% c("Ladrillo", "Lecherada") ~ "mid",
                                          vivi_piso %in% c("Tierra") ~ "low")),
         vivi_agua_fuente1 = as_factor(case_when(vivi_agua_fuente %in% c("Caneria_vivienda") ~ "high",
                                                 vivi_agua_fuente %in% c("Otros", "Vecino", "Caneria_terreno", "Pozo_terreno") ~ "low")),
         vivi_agua_fuente_beber1 = as_factor(case_when(vivi_agua_fuente_beber %in% c("Embotellada", "Canaeria_vivienda") ~ "high",
                                                       vivi_agua_fuente_beber %in% c("Caneria_terreno", "Pozo_terreno", "Vecino", "Otros", "Canilla_publica") ~ "low")),
         vivi_basura1 = as_factor(case_when(vivi_basura %in% c("Vertedero_municipal", "Recoleccion_publica", "Recoleccion_privada") ~ "high",
                                            vivi_basura %in% c("Otro", "Arroyo", "Hoyo", "Chacra", 
                                                               "Patio", "Quema") ~ "low")),
         vivi_techo1 = as_factor(case_when(vivi_techo %in% c("Palma", "Hormigon", "Teja", "Otro") ~ "high",
                                           vivi_techo %in% c("Zinc") ~ "mid",
                                           vivi_techo %in% c("Paja", "Fibrocemento", "Madera", "Carton") ~ "low")),
         vivi_banho_desague1 = as_factor(case_when(vivi_banho_desague %in% c("Camara_septica") ~ "high",
                                                   vivi_banho_desague %in% c("Pozo_ciego", "Letrina_ventilada", "Letrina_comun", "Letrina_comun_sin_techo", "Red_sanitario", "Hoyo_abierto", "Otro", "No_banho") ~ "low")),
         piezas_por_miembro = case_when(vivi_piezas == 0 ~ 1/hh_totpers,
                                        vivi_piezas > 0 ~ vivi_piezas/hh_totpers),
         vivi_vivienda_propiedad1 = as_factor(case_when(vivi_vivienda_propiedad=="Propia" ~ "Own",
                                                       TRUE ~ "Other"))) %>% 
  select(-c(jefe_tipo_empleo, hh_tiene_trabajo_remunerado, hh_tiene_trabajo_noremunerado, hh_tipo_hogar,
            jefe_idioma, jefe_estado_civil, dptorep, vivi_pared, vivi_agua_proveedor, vivi_agua_proveedor, vivi_agua_proveedor_beber1,
            vivi_combustible, vivi_lote_propiedad, vivi_piso, vivi_agua_fuente, vivi_agua_fuente_beber, vivi_agua_fuente_beber1,
            vivi_basura, vivi_techo, vivi_banho_desague, vivi_piezas, hh_dependents, hh_youth_dependents, hh_old_dependents, hh_males,
            hh_miembros_5ymenos, hh_miembros_6a14, hh_miembros_15a64, hh_miembros_65ymas, hh_females, vivi_celular, vivi_tableta, vivi_termocalefon, vivi_antena_parabolica))

# data year
hh2018 <- 
  hhfile %>% 
  filter(year == 2018)

# training and testing set ----------------------------------------------------------------------------------------------------------------
set.seed(123)

hhsplit <- 
  hh2018 %>% 
  initial_split(prop = 0.75, strata = ipcm)

hhtraining <- 
  training(hhsplit)

hhtesting <- 
  testing(hhsplit)

# nominal indicator tables and charts -------------------------------------------------------------------------------

hhtraining %>% tabyl(vivi_vivienda_propiedad) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_vivienda_propiedad) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_vivienda_propiedad, ref="Propia"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhfile, mapping = aes(x = reorder(vivi_vivienda_propiedad, lnipcm, median), 
                             y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_vivienda_propiedad")

## Cross tabs -------------------------------------------------------------------------------------------------------

sjPlot::tab_xtab(hhtraining$vivi_banho_desague1, hhtraining$vivi_agua_fuente1)

# continuous indicators ---------------------------------------------------------------------------------------------

ggplot(hhtraining, aes(x =  log(piezas_por_miembro), y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm") +
  geom_smooth(method="loess")

# checks 
summary(lm(lnipcm ~  vivi_vivienda_propiedad1 + log(piezas_por_miembro)  + hh_tipo_hogar1 + vivi_banho_desague1 + vivi_techo1 + vivi_basura1 + vivi_agua_fuente1  + vivi_piso1 + vivi_lote_propiedad1+ vivi_combustible1 + vivi_agua_proveedor1 + vivi_pared1 + dptorep1  +  jefe_estado_civil1 + jefe_aniosestudio1 + jefe_idioma1 + log(hh_totpers) + jefe_female + jefe_edad + jefe_tipo_empleo1 + perc_tiene_trabajo_remunerado, data = hhtraining))


# predictors -------------------------------------------------------------------------------------------
## continuous ------------------------------------------------------------------------------------------

cor_mat <- 
  hhtraining %>% 
  select(-c("upm", "nvivi", "nhoga", "year", "fex", "facpob", "area", "ipcm", "lnipcm", 
            "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov")) %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  cor()

cor_map <- 
  heatmaply_cor(
    cor_mat, 
    symm = TRUE, 
    cexRow = .0001, 
    cexCol = .0001, 
    branches_lwd = .1
  ) 


ggplot(hhtraining, aes(x = log(hh_totpers), y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm") +
  geom_smooth(method="loess")
  
## nominal -----------------------------------------------------------------------------------------------------------
# removals based on zero variance in recipe
# [1] "vivi_piso_Otro"                         "vivi_piso_Porcelanato"                  "vivi_techo_Palma"                      
# [4] "vivi_agua_fuente_beber_Canilla_publica" "vivi_banho_desague_Otro"                "vivi_banho_desague_Hoyo_abierto"       
# [7] "vivi_basura_Arroyo"                     "vivi_vivienda_propiedad_Ocupada_Hecho" 

hhtraining %>% tabyl(vivi_piso) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_piso) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_piso, ref="Tierra"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(vivi_piso, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_piso")

hhtraining %>% tabyl(vivi_techo) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_techo) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_techo, ref="Fibrocemento"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(vivi_techo, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_techo")

hhtraining %>% tabyl(vivi_agua_fuente) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_agua_fuente) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_agua_fuente, ref="Vecino"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(vivi_agua_fuente, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_agua_fuente")

hhtraining %>% tabyl(vivi_banho_desague) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_banho_desague) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_banho_desague, ref="Letrina_ventilada"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(vivi_banho_desague, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_banho_desague")

hhtraining %>% tabyl(vivi_basura) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_basura) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_basura, ref="Vertedero_municipal"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhfile, mapping = aes(x = reorder(vivi_basura, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_basura")

vivi_agua_fuente_beber_Canilla_publica

hhtraining %>% tabyl(vivi_agua_fuente_beber) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(vivi_agua_fuente_beber) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(vivi_agua_fuente_beber, ref="Vecino"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhfile, mapping = aes(x = reorder(vivi_agua_fuente_beber, lnipcm, median), 
                             y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "vivi_agua_fuente_beber")


hhtraining %>% tabyl(jefe_tipo_empleo) %>% tibble() %>% arrange(desc(percent))
hhfile %>% tabyl(jefe_tipo_empleo) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(jefe_tipo_empleo, ref="Emple"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhfile, mapping = aes(x = reorder(jefe_tipo_empleo, lnipcm, median), 
                             y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "jefe_tipo_empleo")


# NOTES BELOW ON THOUGHT PROCESS -------------------------------------------------------------------------------------------------------
# ......................................................................................................................................
# ......................................................................................................................................
# ......................................................................................................................................
# continuous outcomes -----------------------------------------------------------------------------------------------------
## histogram ---------------------------------------------------------------------------------

hist_ipcm <- 
  ggplot(hhtraining, aes(lnipcm)) +   
  geom_histogram(binwidth = 1, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +  
  xlab("log monthly income per capita") +
  ylab("Frequency") +
  ggtitle("(a)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

## boxplot ---------------------------------------------------------------------------------------------------

box_ipcm <-
  ggplot(hhtraining, aes(x = "", y = lnipcm)) +
  geom_boxplot(alpha = 0.2) +
  ylab("log monthly income per capita") +
  ggtitle("(b)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip()

## violin ------------------------------------------------------------------------

violin_ipcm <-
  ggplot(hhtraining, aes(x = "", y = lnipcm)) +
  geom_violin(alpha = 0.2) +
  ylab("log monthly income per capita") +
  ggtitle("(c)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip() 

## plot together -------------------------------------------------------------------------

grid.arrange(hist_ipcm, box_ipcm, violin_ipcm, nrow = 3, ncol = 1, heights = c(2, 1, 1))

# predictors -------------------------------------------------------------------------
## categorical on continuous outcome ------------------------------------------------------------------------

hhtraining %>% tabyl(jefe_idioma) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(jefe_idioma, ref="Bilingual"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(jefe_idioma, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "jefe_idioma")


ggplot(hhtraining, mapping = aes(x = reorder(dptorep, lnipcm, median), 
                               y = log(ipcm))) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "dptorep")


##continuous on continuous outcome -------------------------------------------------------------------------

ggplot(hhtraining, aes(x = hh_totpers, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(hhtraining, aes(x = vivi_dormitorios, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(hhtraining, aes(x = vivi_piezas, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")+
  geom_smooth(method="lm")

ggplot(hhtraining, aes(x = log(vivi_piezas/hh_totpers), y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(hhtraining, aes(x = jefe_edad, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")

ggplot(hhtraining, aes(x = jefe_aniosestudio, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")

## categorical on discrete outcome ----------------------------------------------------------------
binom_stats <- function(x, ...) {
  x <- x$totpov[!is.na(x$totpov)]
  res <- prop.test(x = sum(x == "tot_pov"), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

pov_rate <- mean(hhtraining$totpov == "tot_pov")

language_rates <- 
  hhtraining %>%
  group_by(jefe_idioma1) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  mutate(jefe_idioma1 = reorder(factor(jefe_idioma1), Proportion))

hhtraining <- 
  hhtraining %>% 
  mutate(jefe_idioma2 = factor(jefe_idioma1, levels = as.character(language_rates$jefe_idioma1))
  )

bars <- 
  ggplot(hhtraining, aes(x = jefe_idioma2, fill = totpov)) +
  geom_bar(position = position_dodge()) + scale_fill_brewer(palette = "Paired") +
  xlab("") +
  theme(legend.position = "top", axis.text = element_text(size = 8)) +
  ggtitle("(a)")

stacked_vars <-
  ggplot(hhtraining, aes(x = jefe_idioma2, fill = totpov)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Paired") +
  xlab("") + ylab("Proportion") +
  theme(legend.position = "none", axis.text = element_text(size = 8)) +
  ggtitle("(b)")

ci_plots <- 
  ggplot(language_rates, aes(x = jefe_idioma1, y = Proportion)) +
  geom_hline(yintercept = pov_rate, col = "red", alpha = .35, lty = 2) + 
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .1) +
  theme(axis.text = element_text(size = 8)) +
  xlab("") +
  ggtitle("(c)")

grid.arrange(bars, stacked_vars, ci_plots, ncol = 1, heights= c(4, 3, 3))

## continuous on categorical outcome -----------------------------------------------------------------------------
gam_dat <- 
  hhtraining %>% 
  dplyr::select(hh_totpers, totpov) %>% 
  arrange(hh_totpers) %>% 
  mutate(totpov = as.factor(totpov)) %>% 
  mutate(totpov = factor(totpov, levels = c("tot_pov", "nontot_pov")))

gam_small <- 
  gam_dat %>%
  distinct(hh_totpers) 

gam_mod <- mgcv::gam(totpov ~ s(hh_totpers), data = gam_dat, family = binomial())

gam_small <- gam_small %>%
  mutate(
    link = -predict(gam_mod, gam_small, type = "link"),
    se = predict(gam_mod, gam_small, type = "link", se.fit = TRUE)$se.fit,
    upper = link + qnorm(.975) * se,
    lower = link - qnorm(.975) * se,
    lower = binomial()$linkinv(lower),
    upper = binomial()$linkinv(upper),
    probability = binomial()$linkinv(link)
  )

totpers_hist <- 
  ggplot(hhtraining, aes(x = hh_totpers)) + 
  geom_histogram(binwidth = .1, col = "#FEB24C", fill = "#FED976") + 
  facet_wrap(~ totpov, ncol = 1) + 
  scale_x_continuous(breaks = c(1:18), labels = c(1:18)) +
  xlab("Total People in Household") + 
  theme_bw() +
  theme(plot.margin = unit(c(0,1,0,1.2), "cm")) + 
  ggtitle("(a)")

totpers_gam <- 
  ggplot(gam_small, aes(x = hh_totpers)) + 
  geom_line(aes(y = probability)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = .5) + 
  geom_hline(yintercept = pov_rate, col = "red", alpha = .35, lty = 2)  + 
  scale_x_continuous(breaks = c(1:18), labels = c(1:18)) +
  theme_bw() + 
  xlab("") +
  theme(plot.margin = unit(c(0,1,0,1.2), "cm"))+ 
  ggtitle("(b)")

grid.arrange(totpers_hist, totpers_gam, ncol = 1, heights= c(2, 1.25))

# Predictor relationships ---------------------------------------------------------------------------------------------
## correlation between continuous predictors --------------------------------------------------------------------------

cor_mat <- 
  hhtraining %>% 
  select(-c("upm", "nvivi", "nhoga", "year", "fex", "facpob", "area", "ipcm", "lnipcm", 
            "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov")) %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  cor()

cor_map <- 
  heatmaply_cor(
    cor_mat, 
    symm = TRUE, 
    cexRow = .0001, 
    cexCol = .0001, 
    branches_lwd = .1
  ) 

## mosaic between categorical predictors -------------------------------------------------------------------
dd_tab <- table(hhtraining$vivi_agua_proveedor1, hhtraining$vivi_agua_fuente1, dnn = c("water provider", "water source"))

library(vcd)
library(colorspace)

mosaic(
  t(dd_tab),
  highlighting = TRUE,
  highlighting_fill = rainbow_hcl,
  margins = unit(c(6, 1, 1, 8), "lines"),
  labeling = labeling_border(
    rot_labels = c(90, 0, 0, 0),
    just_labels = c("left", "right",
                    "center",  "right"),
    offset_varnames = unit(c(3, 1, 1, 4), "lines")
  ),
  keep_aspect_ratio = FALSE
)

## cross tab and correspondence analysis ------------------------------------------------------------------------------

sjPlot::tab_xtab(hhtraining$vivi_agua_proveedor1, hhtraining$vivi_agua_fuente1)


## not working below due to grouping vivi_agua_proveedor1 and vivi_agua_fuente1
library(FactoMineR)

ca_obj <- CA(dd_tab, graph = FALSE)

ca_water <- as.data.frame(ca_obj$row$coord)
ca_water$label <- gsub("_", " ", rownames(ca_water))
ca_water$Variable <- "water provider"

ca_source <- as.data.frame(ca_obj$col$coord)
ca_source$label <- gsub("_", " ", rownames(ca_drink))
ca_source$Variable <- "water source"

ca_rng <- extendrange(c(ca_source$`Dim 1`, ca_source$`Dim 2`))
ca_x <- paste0("Dimension #1 (",
               round(ca_obj$eig["dim 1", "percentage of variance"], 0),
               "%)")
ca_y <- paste0("Dimension #2 (",
               round(ca_obj$eig["dim 2", "percentage of variance"], 0),
               "%)")

ca_coord <- rbind(ca_water, ca_source)

ca_plot <-
  ggplot(ca_coord, aes(x = `Dim 1`, y = `Dim 2`, col = Variable)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = label)) + 
  xlim(ca_rng) + ylim(ca_rng) + 
  xlab(ca_x) + ylab(ca_y) + 
  coord_equal()

# renv ---------------------------------------------------------------------------------------
renv::snapshot()