
library(brms)

Subtitles <- Subtitles %>%
  mutate(

    # dummy for Occ2
    Occ2 = case_when(
      occasion == "Occ2" ~ 1,
      occasion == "Occ1" ~ 0,
      occasion == "Occ3" ~ 0,
    ),
    
    # dummy for Occ3
    Occ3 = case_when(
      occasion == "Occ3" ~ 1,
      occasion == "Occ1" ~ 0,
      occasion == "Occ2" ~ 0,
    ),
    
    # dummy for FL condition
    FL = case_when(
      condition == "FL" ~ 1,
      condition == "MT" ~ 0,
      condition == "NoSub" ~ 0,
    ),
    
    # dummy for MT
    MT = case_when(
      condition == "MT" ~ 1,
      condition == "FL" ~ 0,
      condition == "NoSub" ~ 0,
    )
  )

# Estimate the models

M0 <- brm(
  fluency ~ 1 + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975
)

M1 <- brm(
  fluency ~ 1 + Occ2 + Occ3 + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975
)

M2 <- brm(
  fluency ~ 1 + Occ2 + Occ3 + FL + MT + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975
)

M3 <- brm(
  fluency ~ 1 + Occ2*FL + Occ2*MT + Occ3*FL + Occ3*MT + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975
)

# Save the models

saveRDS(
  M0,
  file = here("Integrated_exercise", "M0.RDS")
)

saveRDS(
  M1,
  file = here("Integrated_exercise", "M1.RDS")
)

saveRDS(
  M2,
  file = here("Integrated_exercise", "M2.RDS")
)

saveRDS(
  M3,
  file = here("Integrated_exercise", "M3.RDS")
)

# loo cross-validation of the models

loo_M0 <- loo(M0)
loo_M1 <- loo(M1)
loo_M2 <- loo(M2)
loo_M3 <- loo(M3)

loo_models <- loo_compare(
  loo_M0,
  loo_M1,
  loo_M2,
  loo_M3
)

print(loo_models, simplify = F)

saveRDS(loo_models,
     file = here("Integrated_exercise", "loo_models.RDS"))


Custom_prior <- c(
  set_prior(
    "normal(0,5.7)",
    class = "b"
  )
)

M3_priors <- brm(
  fluency ~ 1 + Occ2*FL + Occ2*MT + Occ3*FL + Occ3*MT + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975,
  prior = Custom_prior,
  sample_prior = "only"
)

M3b <- brm(
  fluency ~ 1 + Occ2*FL + Occ2*MT + Occ3*FL + Occ3*MT + (1|student),
  data = Subtitles,
  cores = 4,
  backend = "cmdstanr",
  seed = 1975,
  prior = Custom_prior
)

saveRDS(
  M3_priors,
  file = here("Integrated_exercise", "M3_priors.RDS")
)

saveRDS(
  M3b,
  file = here("Integrated_exercise", "M3b.RDS")
)