library(here)
library(foreign)
library(brms)

School_Eff <- read.spss(
  file = here("Data", "SchoolEffectiveness.sav"),
  to.data.frame = TRUE,
  use.value.labels = FALSE
)

Model1 <- brm(
  End_score ~ 1 + (1|Class) + (1|School),
  seed = 1975,
  backend = "cmdstanr",
  cores = 4,
  data = School_Eff
)

summary(Model1)

get_prior(
  End_score ~ 1 + (1|Class) + (1|School),
  data = School_Eff
)

custom_priors <-
  c(
    set_prior(
      "uniform(107,111)",
      lb = 107,
      ub = 111,
      class = "Intercept"
      )
  )

Model1b <- brm(
  End_score ~ 1 + (1|Class) + (1|School),
  seed = 1975,
  prior = custom_priors,
  backend = "cmdstanr",
  cores = 4,
  data = School_Eff
)

summary(Model1b)

plot(Model1b)
