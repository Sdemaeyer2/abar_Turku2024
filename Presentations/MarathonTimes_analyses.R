################################################
## Examplary analyses with MarathonData.RData ##
################################################

# 0. Preparation --------

## 0.1 Load packages ----

library(tidyverse) # for different purposes
library(here) # easy accessing files
library(brms) # to estimate the models
library(loo)  # for model comparisons

## 0.2 Load the dataset ----

load(
  file = here("Presentations", "MarathonData.RData")
)

# 1. Part 1 of workshop ----

## 1.1 Estimate the models used ----

### 1.1.1 Model 1 with fictious data ----

MT <- c(185, 193, 240, 245, 155, 234, 189, 196, 206, 263) # <1>

# First make a dataset from our MT vector
DataMT <- data_frame(MT)  

Mod_MT1 <- brm(                        
  MT ~ 1, # We only model an intercept 
  data = DataMT,                         
  backend = "cmdstanr",                
  seed = 1975                          
)

### 1.1.2 Two alternative models on actual dataset (first excercise)

# run the models

MarathonTimes_Mod1 <- brm(                        
  MarathonTimeM ~ 1, # We only model an intercept 
  data = MarathonData,                         
  #backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

MarathonTimes_Mod2 <- brm(                        
  MarathonTimeM ~ km4week + sp4week, 
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

posterior_PD <- as_draws_df(MarathonTimes_Mod1)

summary(MarathonTimes_Mod2)

pp_check(MarathonTimes_Mod2) 

# save the models

saveRDS(
  MarathonTimes_Mod1,
  file = here("Presentations",
              "Output",
              "MarathonTimes_Mod1.RDS")
)

saveRDS(
  MarathonTimes_Mod2,
  file = here("Presentations",
              "Output",
              "MarathonTimes_Mod2.RDS")
)

# compare the models

loo_Mod1 <- loo(MarathonTimes_Mod1)
loo_Mod2 <- loo(MarathonTimes_Mod2)

Comparison<- 
  loo_compare(
    loo_Mod1, 
    loo_Mod2
  )

print(Comparison, simplify = F)

# posterior predictive checks

pp_check()

get_prior(
  MarathonTimeM ~ 1 + km4week + sp4week, 
  data = MarathonData
)

MarathonData <- MarathonData %>% 
  mutate(
    Age40 = case_when(
      Category == 'M40' ~ 0,
      Category == 'M45' ~ 0,
      Category == 'M50' ~ 0,
      Category == 'M55' ~ 0,
      Category == 'MAM' ~ 1,
      Category == 'WAM' ~ 1
    ),
    km4week_z = km4week / sd(km4week, na.rm = T),
    sp4week_z = sp4week / sd(sp4week, na.rm = T)
  )



MarathonTimes_Mod3 <- brm(                        
  MarathonTimeM ~ km4week_z + sp4week_z + Age40, 
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

saveRDS(
  MarathonTimes_Mod3,
  file = here("Presentations",
              "Output",
              "MarathonTimes_Mod3.RDS")
)

save(
  MarathonData,
  file = here(
    "Presentations",
    "MarathonData.RData"
  )
)

summary(MarathonTimes_Mod3)

library(sjPlot)

plot_model(
  MarathonTimes_Mod2
)

plot_model(
  MarathonTimes_Mod2,
  type = 'pred'
)

MarathonData <- MarathonData %>% 
  mutate(
    Age40F = as.factor(Age40)
    )

MarathonTimes_Mod3b <- brm(                        
  MarathonTimeM ~ km4week_z + sp4week_z + Age40F, 
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

plot_model(
  MarathonTimes_Mod3b,
  type = 'pred'
)


summary(MarathonTimes_Mod3b)



#######

MarathonTimes_Mod1 <- brm(                        
  MarathonTimeM ~ 1, # We only model an intercept 
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

MarathonTimes_Mod2 <- brm(                        
  MarathonTimeM ~ km4week + sp4week, 
  data = MarathonData,    
  prior=Custom_priors,
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

plot(MarathonTimes_Mod2)

get_prior(
  Mm
)

library(tidybayes)

km4week <- rnorm(
  200,
  mean = 0,
  sd = 27
)

sp4week <- rnorm(
  200,
  mean = 0,
  sd = 1.2
)

Age40 <- rbinom(
  200,
  1,
  0.5
)

PredData <- tibble(km4week,sp4week,Age40)

Predictions <- epred_draws(
  MarathonTimes_Mod2,
  seed = 1975,
  newdata = PredData
)

library(tidybayes)

Predictions2 <-
  MarathonData %>% 
  data_grid(
    km4week = seq_range(km4week, n = 100)
  ) %>% 
  add_fitted_draws(
    MarathonTimes_Mod2,
    n = 200
  )

library(tidyverse)
P<-ggplot(
  Predictions,
  aes(
    x = km4week,
    y = .epred
  )
 ) +
 geom_line(aes( group = .draw))

P <-predictions(
  MarathonTimes_Mod2, 
  newdata = Predictions, 
  by = "km4week", 
  re_formula = NA
)




plot_predictions2 <- 
  function(
    model = model,
    par = par,
    n = n,
    min = min,
    max = max,
    by = by,
    seed = seed){
    
    set.seed(seed)
    
    S <- as_draws_df(model) %>% 
      select(
        b_Intercept,
        par,
      ) %>%
      mutate(
        mu = b_Intercept,
        b = par,
      ) %>% 
      select(
        mu,
        b
      ) %>% 
      slice_sample(
        n = n, 
        replace = T
        )
    
    X <- seq(min, max, by = by)
    
    Predictions <- tibble(
      draw = NULL,
      X = NULL,
      Pred1 = NULL  
      )
    
    for(i in 1:n){
      
      Si <- S[i,]
      
      Pred1 <- Si$mu + Si$b*X
      
      draw <- rep(i,length(X))
      
      Pred <- tibble(
        draw,
        X,  
        Pred1, 
      )
      
      Predictions <- rbind(Predictions, Pred)
    }
    
    P1 <- Predictions %>%
      select(draw, X, Pred1) %>%
      ggplot(aes(x = X,
                 y = Pred1,
                 group = draw)) +
      geom_line(color = "gray60", alpha = .6) +
      scale_y_continuous("predicted values") +
      scale_x_continuous("independent variable")
    P1
    
  }

library(brms)
library(tidyverse)

plot_predictions2(
  model = MarathonTimes_Mod2,
  par = "b_km4week",
  min = 10,
  max = 100,
  by = 1,
  n = 20,
  seed = 1975
)



model_parameters(
  MarathonTimes_Mod3,
  ci_method = "hdi",
  rope_range = c(-2.262,2.262), #sd MarathonTimeM = 22.62 so 22.62*0.1 
  test = c("rope", "pd")
  ) %>% 
  filter(
      Parameter == "b_km4week_z" |
      Parameter =="b_sp4week_z" | 
      Parameter == "b_Age40"
  ) %>%
  plot()

equivalence_test(
  MarathonTimes_Mod3
)%>%
  plot()

