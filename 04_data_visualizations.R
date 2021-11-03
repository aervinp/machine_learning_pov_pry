library(tidyverse)
library(janitor)
library(skimr)

library(tidymodels)
library(gridExtra)
library(lubridate)
library(ggiraph)
library(heatmaply)
library(RColorBrewer)
library(scales)

library(mgcv)
library(FactoMineR)
library(vcd)
library(colorspace)

# load household data created in 03_encoding_categorical_numerical.R ----------------------------------------------------------------------

hhfile <- read_rds("data/hh_merged_allyears_cleaned.rds")

# training set
training <- 
  hhfile %>% 
  filter(year == 2018)

# continuous outcomes -----------------------------------------------------------------------------------------------------
## histogram ---------------------------------------------------------------------------------

hist_ipcm <- 
  ggplot(training, aes(ipcm)) +   
  geom_histogram(binwidth = 500000, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +  
  xlab("monthly income per capita") +
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
  ggplot(training, aes(x = "", y = ipcm)) +
  geom_boxplot(alpha = 0.2) +
  ylab("monthly income per capita") +
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
  ggplot(training, aes(x = "", y = ipcm)) +
  geom_violin(alpha = 0.2) +
  ylab("monthly income per capita") +
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

ggplot(training, mapping = aes(x = reorder(as.factor(dptorep), ipcm, median), 
                               y = log(ipcm))) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "dptorep")

ggplot(training, mapping = aes(x = reorder(as.factor(jefe_idioma), ipcm, median), 
                               y = log(ipcm))) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "jefe_idioma")

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