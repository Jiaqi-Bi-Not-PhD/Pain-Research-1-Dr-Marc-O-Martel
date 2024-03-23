## Create Day indicators based on the Date
## And save to sav file
library(tidyverse)
library(haven)
library(lubridate)
df <- read_sav("File To; Jiaqi; 2024; DayIndicator.sav")
df <- df |>
  mutate(Temporary_Date = dmy(SurveySubmittedDate)) |>
  group_by(ID) |>
  arrange(ID, Temporary_Date) |>
  mutate(Wave_Day = as.integer(Temporary_Date - first(Temporary_Date) + 1))

write_sav(df, "File Back To; Marco; 2024; DayIndicator.sav")
