library(tidyverse)
library(tidymodels)
library(glmnet)
library(vip)

# data ---------------------------------------------------------------------------------------------

hhfile <- 
  read_rds("data/hh_merged_allyears_cleaned.rds")

## explore missing values --------------------------------------------------------------------------

allNA <- hhfile %>% mutate_if(is.factor, ~as.numeric(.)) %>%  summarize_if(is.numeric, mean) %>% select(where(is.na)) %>% names()
numNA <- hhfile %>% summarize_if(is.numeric, mean) %>% select(where(is.na)) %>% names()

nominal_NAs <- allNA[(!allNA %in% numNA)]
numeric_NAs <- allNA[!(allNA %in% nominal_NAs)]

## model on 2018 survey --------------------------------------------------------------------

hh2018 <- 
  hhfile %>% 
  filter(year == 2018)

## splitting training and testing -----------------------------------------------------------------------------------------

set.seed(123)

hhsplit <- 
  hh2018 %>% initial_split(prop = 0.75, strata = ipcm, breaks = 10)

hhtrain <- training(hhsplit)
hhtesting <- testing(hhsplit)

## cross-validation folds -------------------------------------------------------------------------------
set.seed(456)
hhfolds <- vfold_cv(hhtrain, strata = ipcm, breaks = 10)

# recipe ----------------------------------------------------------------------------------
hh_rec <- recipe(lnipcm ~ ., data = hhtrain) %>%
  update_role(c("upm", "nvivi", "nhoga", "year", "fex", "facpob", "area", "ipcm", 
                "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov"), new_role = "ID") %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>%
  step_mutate(jefe_tipo_empleo1 = as_factor(case_when(jefe_tipo_empleo %in% c("Domestico", "Cuenta_propia", "No_trabajo", "No_remunerado") ~ "Low",
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
  step_rm(jefe_tipo_empleo, hh_tiene_trabajo_remunerado, hh_tiene_trabajo_noremunerado, hh_tipo_hogar,
          jefe_idioma, jefe_estado_civil, dptorep, vivi_pared, vivi_agua_proveedor, vivi_agua_proveedor_beber, vivi_agua_proveedor_beber1,
          vivi_combustible, vivi_lote_propiedad, vivi_piso, vivi_agua_fuente, vivi_agua_fuente_beber, vivi_agua_fuente_beber1,
          vivi_basura, vivi_techo, vivi_banho_desague, vivi_piezas, hh_dependents, hh_youth_dependents, hh_old_dependents, hh_males,
          hh_miembros_5ymenos, hh_miembros_6a14, hh_miembros_15a64, hh_miembros_65ymas, hh_females, vivi_celular, vivi_tableta, vivi_termocalefon, vivi_antena_parabolica,
          vivi_vivienda_propiedad, vivi_dormitorios, vivi_techo1, jefe_caja_jubilacion1, jefe_aniosestudio1) %>% 
  step_log(hh_totpers, piezas_por_miembro) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_numeric_predictors()) %>%
  check_missing(all_predictors())

# hh_prep <- hh_rec %>%
#   prep(log_changes = TRUE)

# names(hh_prep$steps[[1]]$medians)
# names(hh_prep$steps[[2]]$modes)
# hh_prep$steps[[5]]$removals

## Lasso 
# lasso_spec <- linear_reg(penalty = 0.00569, mixture = 1) %>%
#   set_engine("glmnet")
# 
# wf <- workflow() %>%
#   add_recipe(hh_rec)
# 
# lasso_fit <- wf %>%
#   add_model(lasso_spec) %>%
#   fit(data = hhtrain)
# 
# lasso_fit %>%
#   extract_fit_parsnip() %>%
#   tidy() %>% View()


# lasso w/ tuning----------------------------------------------------------------------------------
lasso_tune <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

p_regular <- grid_regular(penalty(), levels = 50)

wf <- workflow() %>%
  add_recipe(hh_rec)

p_tuning <-
  wf %>% 
  add_model(lasso_tune) %>% 
  tune_grid(resamples = hhfolds,
            grid = p_regular,
            metrics = metric_set(rmse))

p_tuning %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- 
  p_tuning %>% 
  select_best("rmse")

final_lasso <- finalize_workflow(
  wf %>% add_model(lasso_tune),
  lowest_rmse
)

final_lasso %>%
  fit(hhtrain) %>% 
  extract_fit_parsnip() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

## fit final lasso to the unseen data -------------------------------------------------------------------------------
pred_lasso <- 
  final_lasso %>%
  fit(hhtrain) %>% 
  predict(hhfile) 

new_lasso_data <- 
  hhfile %>% 
  bind_cols(pred_lasso)

new_lasso_data %>% 
  group_by(year) %>% 
  rsq(truth = lnipcm, estimate = .pred)
  
## cross-validation 
# lm_rs_fit <- 
#   wf %>% 
#   add_model(lm_model) %>% 
#   fit_resamples(resamples = hhfolds)
# 
# lm_rs_fit %>% collect_metrics(summarize = FALSE) %>% filter(.metric=="rsq")


# linear model post lasso -----------------------------------------------------------------------------------------
vars <- final_lasso %>%
  fit(hhtrain) %>%
  extract_fit_parsnip() %>% 
  tidy() %>% 
  filter(abs(estimate)>0.025) %>% 
  pull(term)

outcome <- "lnipcm"
vars <- vars[-1]

training_data_prep <- hh_rec %>% prep() %>% bake(new_data = hhtrain)
hhfile_prep <- hh_rec %>% prep() %>% bake(new_data = hhfile)

f <- as.formula(paste(outcome, paste(vars, collapse = " + "), sep = " ~ "))


# hhprepsplit <- 
#   training_data_prep %>% initial_split(prop = 0.75, strata = ipcm, breaks = 10)
# 
# training_data_preptrain <- training(hhprepsplit)
# training_data_preptest <- testing(hhprepsplit)
#
#  update_recipe(recipe(paste0("lnipcm ~ ", paste(vars, collapse = "+"), ", hhtrain")))

lm_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

wf <- workflow() %>%
  add_recipe(hh_rec) %>%
  update_recipe(recipe(f, training_data_prep))

wf %>%
  add_model(lm_model) %>%
  fit(data = training_data_prep) %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  View()

pred_lm <- 
  wf %>%
  add_model(lm_model) %>%
  fit(data = training_data_prep) %>% 
  predict(hhfile_prep) 

new_lm_data <- 
  hhfile_prep %>% 
  bind_cols(pred_lm)

new_lm_data %>% 
  group_by(year) %>% 
  rsq(truth = lnipcm, estimate = .pred)


##################################################################

lm_fit <- 
  lm_model %>% 
  fit(lnipcm ~ hh_totpers + jefe_female, data = hhtraining)

tidy(lm_fit)

ipcm_pred <- 
  lm_fit %>% 
  predict(new_data = hhtesting)

ipcm_test_results <- 
  hhtesting %>% 
  select(lnipcm, hh_totpers, jefe_female) %>%
  bind_cols(ipcm_pred)
  
ipcm_test_results %>% 
  rmse(truth = lnipcm, estimate = .pred)

ipcm_test_results %>% 
  rsq(truth = lnipcm, estimate = .pred)

ggplot(ipcm_test_results, aes(x=lnipcm, y=.pred)) +
  geom_point() +
  geom_abline(color = "blue", linetype = 2) +
  coord_obs_pred() +
  labs(title = "R-Squared Plot",
       y = "Predicted lnipcm",
       x = "Actual lnipcm")

lm_last_fit <- 
  lm_model %>% 
  last_fit(lnipcm ~ hh_totpers + jefe_female,
           split = hhsplit)

lm_last_fit %>% 
  collect_metrics()

lm_last_fit %>% 
  collect_predictions()

# classification ---------------------------------------------------------------------------------
## splitting ---------------------------------------------------------
pov_split <- 
  initial_split(hh2018,
                prop = 0.75,
                strata = poverty)

pov_training <- 
  pov_split %>% training()

pov_testing <- 
  pov_split %>% testing()

## logistic (default cutoff prob>=.5 then poverty)--------------------------------------------------------
logistic_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logistic_fit <- 
  logistic_model %>% 
  fit(poverty ~ jefe_female + hh_totpers, data = pov_training)

class_preds <-
  logistic_fit %>%
  predict(new_data = pov_testing,
          type = "class")

prob_preds <-
  logistic_fit %>%
  predict(new_data = pov_testing,
          type = "prob")

pov_results <- 
  pov_testing %>% 
  select(poverty, jefe_female, hh_totpers) %>% 
  bind_cols(class_preds, prob_preds)

conf_mat(pov_results,
         truth = poverty,
         estimate = .pred_class)

accuracy(pov_results,
         truth = poverty,
         estimate = .pred_class)

sens(pov_results,
     truth = poverty,
     estimate = .pred_class)

specificity(pov_results, truth=poverty, estimate = .pred_class)
yardstick::spec(pov_results, truth=poverty, estimate = .pred_class)

fpr = 1-specificity(pov_results, truth=poverty, estimate = .pred_class)[1,3]

custom_metrics <- 
  metric_set(accuracy, sens, specificity)

custom_metrics(pov_results,
               truth = poverty,
               estimate = .pred_class)

conf_mat(pov_results,
         truth = poverty,
         estimate = .pred_class) %>% 
  summary()


## graphing ----------------------------------------------------------------------------
conf_mat(pov_results,
         truth = poverty,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

conf_mat(pov_results,
         truth = poverty,
         estimate = .pred_class) %>% 
  autoplot(type = "mosaic")

# threshold range -----------------------------------------------------------------------
pov_thresholds <- 
  pov_results %>% 
  roc_curve(truth = poverty, estimate = .pred_pov) %>% 
  mutate(youden_index = sensitivity + specificity - 1)

pov_results %>% 
  roc_curve(truth = poverty, estimate = .pred_pov) %>%
  autoplot()

roc_auc(pov_results,
        truth = poverty,
        estimate = .pred_pov)

pov_results %>% 
  mutate(pov_cutoff = factor(case_when(.pred_pov>=.35 ~ "pov",
                          TRUE ~ "non_pov"), levels = c("pov", "non_pov"))) %>% 
conf_mat(truth = poverty,
         estimate = pov_cutoff) %>% 
  autoplot(type = "heatmap")

# last fit workflow --------------------------------------------------------------------------
pov_split <- 
  initial_split(hh2018,
                prop = 0.75,
                strata = poverty)

logistic_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logistic_last_fit <- 
  logistic_model %>% 
  last_fit(poverty ~ jefe_female + hh_totpers, split = pov_split)

logistic_last_fit %>% 
  collect_metrics()

last_fit_results <- 
  logistic_last_fit %>% 
  collect_predictions()

custom_metrics <- metric_set(accuracy, sens, specificity, roc_auc)

custom_metrics(last_fit_results,
               truth = poverty,
               estimate = .pred_class, .pred_pov)


# feature engineering -----------------------------------------------------------------------------
# nominal need to be encoded as factors
# numeric need to check for missing

hhfile_recipe <- recipe(ipcm ~ ., data = hhfile) %>% 
  step_log(ipcm) %>% 
  step_corr(vivi_piezas, vivi_dormitorios,
            threshold = 0.9) %>% 
  step_normalize(vivi_dormitorios) %>% 
  step_dummy(vivi_techo) %>% 
  prep(training = hhtraining) %>% 
  bake(new_data = NULL)

hhfile_rec_prep <- hhfile_recipe %>% 
  prep(training = hhtraining)

hhfile_rec_prep %>% 
  bake(new_data = NULL) %>% select(ipcm)

hhfile_rec_prep %>% 
  bake(new_data = hhtesting)

hhtraining %>% 
  select_if(is.numeric) %>% 
  cor()


###################################
###################################

## training set ----------------------------------------------------------------------------------------------------------
training <- 
  hhfile %>% 
  filter(year == 2018)

## testing set -----------------------------------------------------------------------------------------------------------
testing <- 
  hhfile %>% 
  filter(year>2018)

## analysis set ----------------------------------------------------------------------------------------------------------
set.seed(123)
split <- 
  training %>% 
  initial_split(strata = ipcm, prop = 3/4, breaks = 10)

analysis <- 
  training(split)

## assessment set --------------------------------------------------------------------------------------------------------
assessment <- 
  testing(split)

hhfile %>% 
  group_by(year) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = facpob))

training %>% 
  group_by(year, dptorep) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = facpob))

analysis %>% 
  group_by(year, dptorep) %>% 
  summarize(pov = weighted.mean(as.numeric(ipcm<=linea_pobreza_total), w = facpob))

mean(analysis$ipcm)
mean(training$ipcm)

# Box-Cox --------------------------------------------------

simple_trans_rec <- recipe(lnipcm ~ ., data = analysis) %>%
  step_BoxCox(jefe_aniosestudio, piezas_per_pers) %>%
  prep(training = analysis)

simple_trans_test <- bake(simple_trans_rec, analysis)

pred_b_lambda <-
  tidy(simple_trans_rec, number = 1) %>% 
  filter(terms == "piezas_per_pers") %>% 
  select(value)

bc_before <- ggplot(analysis, aes(x = piezas_per_pers)) + 
  geom_histogram(bins = 35, col = "blue", fill = "blue", alpha = .6) + 
  xlab("piezas_per_pers") + 
  ggtitle("(a)")

bc_after <- ggplot(simple_trans_test, aes(x = piezas_per_pers)) + 
  geom_histogram(bins = 35, col = "red", fill = "red", alpha = .6) + 
  xlab("piezas_per_pers") + 
  ggtitle("(b)")

pred_b_lambda2 <-
  tidy(simple_trans_rec, number = 1) %>% 
  filter(terms == "jefe_aniosestudio") %>% 
  select(value)

bc_before2 <- ggplot(analysis, aes(x = jefe_aniosestudio)) + 
  geom_histogram(bins = 35, col = "blue", fill = "blue", alpha = .6) + 
  xlab("jefe_aniosestudio") + 
  ggtitle("(a)")

bc_after2 <- ggplot(simple_trans_test, aes(x = jefe_aniosestudio)) + 
  geom_histogram(bins = 35, col = "red", fill = "red", alpha = .6) + 
  xlab("jefe_aniosestudio") + 
  ggtitle("(b)")


ggplot(analysis, aes(x = piezas_per_pers, y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(simple_trans_test, aes(x = piezas_per_pers, y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(analysis, aes(x = jefe_aniosestudio, y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

ggplot(simple_trans_test, aes(x = jefe_aniosestudio, y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")

analysis %>%
  mutate(test = (piezas_per_pers^(-0.0662)-1)/(-0.0662*(exp(mean(log(piezas_per_pers)))^-0.0662)-1)) %>%
  ggplot(aes(x = test, y = lnipcm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess") +
  geom_smooth(method="lm")
