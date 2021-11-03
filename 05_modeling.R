library(tidyverse)
library(tidymodels)

# data ---------------------------------------------------------------------------------------------

hhfile <- 
  read_csv("data/hh_merged_allyears.csv") %>% 
  mutate(lnipcm = log(ipcm),
         poverty = factor(case_when(ipcm<linea_pobreza_total ~ "pov",
                                       ipcm>=linea_pobreza_total ~ "non_pov"), 
                             levels = c("pov", "non_pov")))

# check levels of poverty --------------------------------------------------------------------------

levels(hhfile[["poverty"]])

hh2018 <- 
  hhfile %>% 
  filter(year == 2018)

# splitting -----------------------------------------------------------------------------------------

set.seed(123)

hhsplit <- 
  hh2018 %>% 
  initial_split(prop = 0.75, strata = ipcm)

hhtraining <- 
  training(hhsplit)

hhtesting <- 
  testing(hhsplit)

# Distribution of ipcm in data
hhtraining %>% group_by(dptorep) %>% 
  summarize(min_ipcm = min(ipcm),
            max_ipcm = max(ipcm),
            mean_ipcm = mean(ipcm),
            sd_ipcm = sd(ipcm))

hhtesting %>% group_by(dptorep) %>% 
  summarize(min_ipcm = min(ipcm),
            max_ipcm = max(ipcm),
            mean_ipcm = mean(ipcm),
            sd_ipcm = sd(ipcm))

# linear regression -------------------------------------------------------------------------------------------------

lm_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

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
