## Exercises during Day 3

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
              "WritingData.RData"
  )
)

## Estimate two alternative models

M1 <- brm(                        
  SecondVersion ~ FirstVersion_GM + (1|Class), 
  data = WritingData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

M2 <- brm(                        
  SecondVersion ~ FirstVersion_GM + (1 + FirstVersion_GM |Class), 
  data = WritingData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

M3 <- brm(                        
  SecondVersion ~ FirstVersion_GM + Experimental_condition + 
    (1 + FirstVersion_GM |Class), 
  data = WritingData,                         
  backend = "cmdstanr",  
  cores = 4,
  seed = 1975                          
)

## Leave-one-out cross-validation

loo_M1 <- loo(M1)
loo_M2 <- loo(M2)
loo_M3 <- loo(M3)

Comparison<- 
  loo_compare(
    loo_M1, 
    loo_M2,
    loo_M3
  )

print(Comparison, simplify = F)

## re-run M3 with some tweaks in the estimation algorithm

M3 <- brm(                        
  SecondVersion ~ FirstVersion_GM + Experimental_condition + 
    (1 + FirstVersion_GM |Class), 
  data = WritingData,                         
  backend = "cmdstanr",  
  cores = 4,
  control = list(adapt_delta = 0.9),
  seed = 1975                          
)

loo_M3 <- loo(M3)

Comparison<- 
  loo_compare(
    loo_M1, 
    loo_M2,
    loo_M3
  )

print(Comparison, simplify = F)
