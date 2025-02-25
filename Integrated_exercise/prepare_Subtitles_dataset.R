library(foreign)
library(here)

library(tidyverse)

Subtitles <- read.spss(
  file = here("Integrated_exercise", "subtitles.sav"),
  to.data.frame = T,
  use.value.labels = F
)

Subtitles <- Subtitles %>%
  mutate(
    condition = case_when(
      condition == 1 ~ "FL",
      condition == 2 ~ "MT",
      condition == 3 ~ "NoSub"
    ),
    occasion = case_when(
      occassion == 1 ~ "Occ1",
      occassion == 2 ~ "Occ2",
      occassion == 3 ~ "Occ3"
    )
  ) %>%
  select(
    student,
    occasion,
    condition,
    fluency
  )

save(
  Subtitles,
  file = here("Integrated_exercise", "Subtitles.RData")
)


