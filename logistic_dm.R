library(tidyverse)
library(mlbench)
library(broom)
library(cutpointr)
library(janitor)
library(easystats)

dm_data <- data("PimaIndiansDiabetes2", package = "mlbench")

# build model, all variables
dm_mod <- glm(diabetes ~ ., 
              data = PimaIndiansDiabetes2, 
              family = "binomial")
# output
summary(dm_mod)
#tidy version
tidy(dm_mod)
# model performance
glance(dm_mod)

# augment data with fitted predictions and residuals
dm_data_plus <- augment(dm_mod) %>% 
  mutate(pct_prob = 100 * plogis(.fitted)) %>% 
  relocate(diabetes, .fitted, pct_prob) %>% 
  arrange(-.fitted)

# select a cut point for classification
cp <- dm_data_plus %>% 
  cutpointr(pct_prob, diabetes,
            pos_class = "pos",
            method= maximize_metric,
            metric = sum_sens_spec)

cp
summary(cp)

plot(cp)

plot_metric(cp)

# classify based on cut point
dm_data_plus <- dm_data_plus %>% 
  mutate(pred_yes_dm = 
  case_when(pct_prob > cp$optimal_cutpoint ~ "pred_yes_dm", 
            TRUE ~ "pred_no")) %>% 
  relocate(pred_yes_dm, .after =pct_prob)

# check confusion matrix
dm_data_plus %>% 
  tabyl(diabetes, pred_yes_dm) %>% 
  adorn_totals("both") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting()


#check model performance
performance::check_model(dm_mod, panel = FALSE)
# use panel = TRUE in Rmarkdown to get 2x3 panels for 6 plots
# 
performance::model_performance(dm_mod)

#try a different model
dm_mod2 <- glm(diabetes ~ glucose + mass + pedigree + age, 
              data = PimaIndiansDiabetes2, 
              family = "binomial")

# build a really simple (NULL) model as a baseline

dm_mod3 <- glm(diabetes ~ 1,
                 data = PimaIndiansDiabetes2, 
              family = "binomial")

summary(dm_mod3)

# compare models
compare_performance(dm_mod, dm_mod2, dm_mod3, rank = TRUE)

plot(compare_performance(dm_mod, dm_mod2, dm_mod3, rank = TRUE)) + labs(subtitle = "Larger Area is Better")

plot(compare_performance(dm_mod, dm_mod2, rank = TRUE)) + labs(subtitle = "Larger Area is Better")

test_performance(dm_mod, dm_mod2, dm_mod3)

# save model to RDS
saveRDS(dm_mod, "dm_mod.RDS")
