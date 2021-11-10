library(tidyverse)
library(janitor)
library(tidymodels)

library(heatmaply)
library(gridExtra)

# load household data created in 03_encoding_categorical_numerical.R ----------------------------------------------------------------------

hhfile <- read_rds("data/hh_merged_allyears_cleaned.rds")


# some explorations ----------------------------------------------------------------------------------------------------------------------
hhfile <- 
  hhfile %>% 
  mutate(hh_tiene_trabajo_remunerado = case_when(jefe_tipo_empleo %in% c("Cuenta_propia", "Publico", "Patron", "Privado", "Domestico",
                                                                         "Empleado_extranjero", "Empleador_extranjero") ~ hh_tiene_trabajo_remunerado-1,
                                                 TRUE ~ hh_tiene_trabajo_remunerado),
         perc_tiene_trabajo_remunerado = (hh_tiene_trabajo_remunerado/(hh_totpers-1)),
         perc_tiene_trabajo_remunerado = replace(perc_tiene_trabajo_remunerado, hh_totpers==1, 0),
         perc_females = hh_females/hh_totpers,
         perc_hh_miembros_6a14 = hh_miembros_6a14/hh_totpers,
         perc_hh_miembros_15a64 = hh_miembros_15a64/hh_totpers,
         perc_hh_miembros_65ymas = hh_miembros_65ymas/hh_totpers,
         jefe_65ymas = as_factor(case_when(jefe_edad>=65 ~ "Yes",
                                 TRUE ~ "No")))
  

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


ggplot(hhtraining, aes(x = log(hh_tiene_trabajo_remunerado+.5), y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm") +
  geom_smooth(method="loess")
  
summary(lm(lnipcm ~ log(hh_miembros_5ymenos+.5) + log(hh_miembros_6a14+0.5) + log(hh_miembros_15a64+0.5) + log(hh_miembros_65ymas+.5) + 
             log(hh_females+0.5) + log(hh_tiene_trabajo_remunerado+0.5) + log(hh_tiene_trabajo_noremunerado+0.5) + jefe_edad + jefe_aniosestudio +
             log((vivi_piezas+.5)/hh_totpers), data = hhtraining))

summary(lm(lnipcm ~ log(hh_totpers+.5) + log(hh_tiene_trabajo_remunerado+0.5) + jefe_aniosestudio +
             log((vivi_piezas+.5)/hh_totpers) + jefe_tipo_empleo + jefe_caja_jubilacion1 + jefe_65ymas, data = hhtraining))

## nominal -----------------------------------------------------------------------------------------------------------
# removals
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


hhfile <- 
  hhfile %>% 
    mutate(vivi_piso1 = as_factor(case_when(vivi_piso %in% c("Porcelanato", "Parquet", "Otro", "Baldosa", "Madera") ~ "high",
                                            vivi_piso %in% c("Ladrillo", "Lecherada") ~ "mid",
                                            vivi_piso %in% c("Tierra") ~ "low")),
           vivi_techo1 = as_factor(case_when(vivi_techo %in% c("Palma", "Hormigon", "Teja") ~ "high",
                                             vivi_techo %in% c("Otro", "Zinc", "Fibrocemento") ~ "mid",
                                             vivi_techo %in% c("Paja", "Madera", "Carton") ~ "low")),
           vivi_agua_fuente1 = as_factor(case_when(vivi_agua_fuente %in% c("Caneria_vivienda") ~ "high",
                                                   vivi_agua_fuente %in% c("Otros", "Vecino", "Caneria_terreno", "Pozo_terreno") ~ "low")),
           vivi_banho_desague1 = as_factor(case_when(vivi_banho_desague %in% c("Camara_septica") ~ "high",
                                                     vivi_banho_desague %in% c("Pozo_ciego") ~ "mid",
                                                     vivi_banho_desague %in% c("Letrina_ventilada", "Letrina_comun", "Letrina_comun_sin_techo", "Red_sanitario", "Hoyo_abierto", "Otro", "No_banho") ~ "low")),
           vivi_basura1 = as_factor(case_when(vivi_basura %in% c("Vertedero_municipal", "Recoleccion_publica", "Recoleccion_privada") ~ "high",
                                              vivi_basura %in% c("Otro", "Arroyo", "Hoyo", "Chacra", 
                                                                 "Patio", "Quema") ~ "low")),
           vivi_agua_fuente_beber1 = as_factor(case_when(vivi_agua_fuente_beber %in% c("Embotellada", "Canaeria_vivienda") ~ "high",
                                                         vivi_agua_fuente_beber %in% c("Caneria_terreno", "Pozo_terreno", "Vecino", "Otros", "Canilla_publica") ~ "low")))

tidy(lm(lnipcm ~ relevel(vivi_piso1, ref="mid"), data=hhfile)) %>% arrange(desc(estimate))
tidy(lm(lnipcm ~ relevel(vivi_techo1, ref="mid"), data=hhfile)) %>% arrange(desc(estimate))
tidy(lm(lnipcm ~ relevel(vivi_agua_fuente1, ref="low"), data=hhfile)) %>% arrange(desc(estimate))
tidy(lm(lnipcm ~ relevel(vivi_banho_desague1, ref="mid"), data=hhfile)) %>% arrange(desc(estimate))
tidy(lm(lnipcm ~ relevel(vivi_basura1, ref="low"), data=hhfile)) %>% arrange(desc(estimate))

hhtraining %>% tabyl(jefe_idioma) %>% tibble() %>% arrange(desc(percent))
tidy(lm(lnipcm ~ relevel(jefe_idioma, ref="Bilingual"), data=hhtraining)) %>% arrange(desc(estimate))
ggplot(hhtraining, mapping = aes(x = reorder(jefe_idioma, lnipcm, median), 
                                 y = lnipcm)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "jefe_idioma")

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

ggplot(training, aes(x = hh_totpers, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(training, aes(x = vivi_dormitorios, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(training, aes(x = vivi_piezas, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")+
  geom_smooth(method="lm")

ggplot(training, aes(x = log(vivi_piezas/hh_totpers), y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(training, aes(x = jefe_edad, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")

ggplot(training, aes(x = jefe_aniosestudio, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")

## categorical on discrete outcome ----------------------------------------------------------------
binom_stats <- function(x, ...) {
  x <- x$Class[!is.na(x$Class)]
  res <- prop.test(x = sum(x == "pov"), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

training <- 
  training %>% 
  mutate(Class = case_when(ipcm<=linea_pobreza_total ~ "pov",
                           ipcm>linea_pobreza_total ~ "no_pov"))

pov_rate <- mean(training$Class == "pov")

language_rates <- 
  training %>%
  group_by(jefe_idioma) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  mutate(jefe_idioma = reorder(factor(jefe_idioma), Proportion))

training <- 
  training %>% 
  mutate(jefe_idioma2 = factor(jefe_idioma, levels = as.character(language_rates$jefe_idioma))
  )

bars <- 
  ggplot(training, aes(x = jefe_idioma2, fill = Class)) +
  geom_bar(position = position_dodge()) + scale_fill_brewer(palette = "Paired") +
  xlab("") +
  theme(legend.position = "top", axis.text = element_text(size = 8)) +
  ggtitle("(a)")

stacked_vars <-
  ggplot(training, aes(x = jefe_idioma2, fill = Class)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Paired") +
  xlab("") + ylab("Proportion") +
  theme(legend.position = "none", axis.text = element_text(size = 8)) +
  ggtitle("(b)")

ci_plots <- 
  ggplot(language_rates, aes(x = jefe_idioma, y = Proportion)) +
  geom_hline(yintercept = pov_rate, col = "red", alpha = .35, lty = 2) + 
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .1) +
  theme(axis.text = element_text(size = 8)) +
  xlab("") +
  ggtitle("(c)")

grid.arrange(bars, stacked_vars, ci_plots, ncol = 1, heights= c(4, 3, 3))

## continuous on categorical outcome -----------------------------------------------------------------------------
gam_dat <- 
  training %>% 
  dplyr::select(hh_totpers, Class) %>% 
  arrange(hh_totpers) %>% 
  mutate(Class = as.factor(Class)) %>% 
  mutate(Class = factor(Class, levels = c("pov", "no_pov")))

gam_small <- 
  gam_dat %>%
  distinct(hh_totpers) 

gam_mod <- mgcv::gam(Class ~ s(hh_totpers), data = gam_dat, family = binomial())

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
  ggplot(training, aes(x = hh_totpers)) + 
  geom_histogram(binwidth = .1, col = "#FEB24C", fill = "#FED976") + 
  facet_wrap(~ Class, ncol = 1) + 
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
  training %>% 
  select(vivi_piezas, vivi_dormitorios, jefe_edad, jefe_aniosestudio) %>% 
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
dd_tab <- table(training$vivi_agua_proveedor, training$vivi_agua_proveedor_beber, dnn = c("water provider", "drinking water provider"))

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

sjPlot::tab_xtab(training$vivi_agua_proveedor, training$vivi_agua_proveedor_beber)

ca_obj <- CA(dd_tab, graph = FALSE)

ca_water <- as.data.frame(ca_obj$row$coord)
ca_water$label <- gsub("_", " ", rownames(ca_water))
ca_water$Variable <- "water provider"

ca_drink <- as.data.frame(ca_obj$col$coord)
ca_drink$label <- gsub("_", " ", rownames(ca_drink))
ca_drink$Variable <- "drinking water provider"

ca_rng <- extendrange(c(ca_drink$`Dim 1`, ca_drink$`Dim 2`))
ca_x <- paste0("Dimension #1 (",
               round(ca_obj$eig["dim 1", "percentage of variance"], 0),
               "%)")
ca_y <- paste0("Dimension #2 (",
               round(ca_obj$eig["dim 2", "percentage of variance"], 0),
               "%)")

ca_coord <- rbind(ca_water, ca_drink)

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