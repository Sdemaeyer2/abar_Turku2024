


# 0. Preparation --------

## 0.1 Load packages ----

library(tidyverse) # for different purposes
library(here) # easy accessing files
library(brms) # to estimate the models
library(loo)  # for model comparisons

## 0.2 Load the dataset ----

load(
  file = here("Presentations", "WritingData.RData")
)

## 0.3 Recode Condition variable ----

WritingData <- WritingData %>%
  mutate(
    Experimental_condition = 
    case_when(
      Condition == 1 ~ 1,
      Condition == 2 ~ 0 
    )
  )

save(
  WritingData,
  file = here("Presentations", "WritingData.RData")
)

# 1. Estimate three models ----

# 1.1. Only effect of FirstVersion_GM - no random slopes

M1 <- brm(
  SecondVersion ~ FirstVersion_GM + (1|Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  seed = 1975 
)

summary(M1)

# 1.2. Only effect of FirstVersion_GM - random slopes

M2 <- brm(
  SecondVersion ~ FirstVersion_GM + (1 + FirstVersion_GM |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  seed = 1975 
)

summary(M2)

# 1.3. Effect of Condition 

M3 <- brm(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  seed = 1975 
)

summary(M3)

#  1.4 Compare the models

loo_M1 <- loo(M1)
loo_M2 <- loo(M2)
loo_M3 <- loo(M3)

looComp <- loo_compare(loo_M1,
           loo_M2,
           loo_M3)

print(looComp,
      simplify = F)

M3 <- brm(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  control = list(adapt_delta = 0.9),
  seed = 1975 
)

saveRDS(
  M3,
  file = here("Presentations", "Part3", "M3.RDS")
  )

M4 <- brm(
  SecondVersion ~ FirstVersion_GM * Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  control = list(adapt_delta = 0.9),
  seed = 1975 
)

loo_M4 <- loo(M4)

looComp <- loo_compare(loo_M1,
                       loo_M2,
                       loo_M3,
                       loo_M4)

print(looComp,
      simplify = F)


set.seed = 2021

as_draws_df(M3) %>% 
  select(ends_with(",Intercept]")) %>%
  pivot_longer(starts_with("r_Class")) %>% 
  mutate(sigma_i = value) %>%
  ggplot(aes(x = sigma_i, y = reorder(name, sigma_i))) +
  stat_pointinterval(
    point_interval = mean_qi, 
    .width = .89, 
    size = 1/6) +
  scale_y_discrete(expression(italic(j)), breaks = NULL) +
  labs(x = expression(italic(u)[italic(j)])) +
  coord_flip()



