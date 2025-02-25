## Exercises during Day 2

library(here)
library(tidyverse)
library(brms)
library(bayesplot)
library(ggmcmc)
library(patchwork)
library(priorsense)

## Load the dataset

load(
  file = here("Data", 
              "MarathonData.RData"
              )
  )

## Estimate two alternative models

MarathonTimes_Mod1 <- brm(                        
  MarathonTimeM ~ 1, # We only model an intercept 
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

MarathonTimes_Mod2 <- brm(                        
  MarathonTimeM ~ km4week + sp4week, # added the effects
  data = MarathonData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

plot(MarathonTimes_Mod2)

## Leave-one-out cross-validation

loo_Mod1 <- loo(MarathonTimes_Mod1)
loo_Mod2 <- loo(MarathonTimes_Mod2)

Comparison<- 
  loo_compare(
    loo_Mod1, 
    loo_Mod2
  )

print(Comparison, simplify = F)