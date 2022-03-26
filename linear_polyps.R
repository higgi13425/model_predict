library(tidyverse)
library(medicaldata)
library(broom)
library(janitor)
library(easystats)
library(performance)
library(insight)

polyp_data <- medicaldata::polyps

# build model
polyp_mod <- lm(number12m ~ sex + age + baseline + treatment, 
                data = polyp_data)
# model output
summary(polyp_mod)

tidy(polyp_mod)

# model performance
glance(polyp_mod)

# add predictions and residuals to data
polyp_data_plus <- augment(polyp_mod) %>% 
  relocate(.fitted, number12m, .resid) %>% 
  arrange(-.fitted)

# check model
performance::check_model(polyp_mod, panel = FALSE)
# use panel = TRUE in Rmarkdown to get 2x3 panels for 6 plots

check_heteroscedasticity(polyp_mod)

performance::model_performance(polyp_mod)

# build a simpler model
polyp_mod2 <- lm(number12m ~ age + baseline + treatment,
                 data = polyp_data)

# build a really simple (NULL) model as a baseline

polyp_mod3 <- lm(number12m ~ NULL,
                 data = polyp_data)

summary(polyp_mod3)

# compare models
compare_performance(polyp_mod, polyp_mod2, polyp_mod3, 
                    rank = TRUE)

plot(compare_performance(polyp_mod, polyp_mod2, polyp_mod3, rank = TRUE)) + 
  labs(subtitle = "Larger Area is Better")


test_performance(polyp_mod, polyp_mod2, polyp_mod3)

# save model to RDS
saveRDS(polyp_mod2, "polyp_mod.RDS")

