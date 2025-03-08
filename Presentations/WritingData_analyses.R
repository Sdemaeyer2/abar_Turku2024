


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


saveRDS(
  M3,
  file = here("Presentations", "Part3", "M3.RDS")
  )

get_prior(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData)


Custom_priors <- 
  c(
    set_prior(
      "normal(1,5)", 
      class = "b", 
      coef = "FirstVersion_GM"),
    set_prior(
      "normal(3.4,17)", 
      class = "b", 
      coef = "Experimental_condition"),
    set_prior(
      "student_t(3,0,5)", 
      class = "sd", 
      coef = "FirstVersion_GM",
      group = "Class")
  )

M3_custom <- brm(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  prior = Custom_priors,
  backend = "cmdstanr",
  cores = 4,
  seed = 1975
)

saveRDS(
  M3_custom,
  file = here("Presentations", "Part3", "M3_custom.RDS")
)

M3_custom_priorsOnly <- brm(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  prior = Custom_priors,
  backend = "cmdstanr",
  cores = 4,
  seed = 1975, 
  sample_prior = "only"
)

saveRDS(
  M3_custom_priorsOnly,
  file = here("Presentations", "Part3", "M3_custom_priorsOnly.RDS")
)

sd <- as_draws_df(M3_custom) %>%
  select("sd_Class__Intercept") %>%
  rename(
    sd = `sd_Class__Intercept`
  ) 

plot(sd)

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


pp_check(M3_custom_priorsOnly ,
         group = "Class")

pp_check(
  M3_custom_priorsOnly ,
  type = "intervals_grouped",
  group = "Class",
  ndraws = 300)

pp_check(
  M3_custom_priorsOnly ,
  type = "ecdf_overlay_grouped",
  group = "Class",
  ndraws = 100)

pp_check(
  M3_custom_priorsOnly ,
  type = "dens_overlay_grouped",
  group = "Class",
  ndraws = 100) +
  scale_x_continuous(limits = c(0,200))


priors <- get_prior(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData)

## ggdist approach to plot priors!

library(ggdist)

# https://mjskay.github.io/ggdist/articles/slabinterval.html#visualizing-priors

p <- priors %>%
  parse_dist(prior)


priors %>%
  parse_dist(prior) %>%
  filter(
    class == "Intercept"
  ) %>%
  ggplot(aes(y = paste(class, "~", format(.dist_obj)), xdist = .dist_obj)) +
  stat_halfeye(subguide = subguide_inside(position = "right", title = "density")) +
  labs(
    title = "stat_halfeye()",
    subtitle = "with parse_dist() and brms::prior() to show priors",
    x = NULL,
    y = NULL
  )
