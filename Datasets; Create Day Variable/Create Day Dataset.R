## Create Day indicators based on the Date
## And save to sav file
library(tidyverse)
library(haven)
library(lubridate)
df <- read_sav("File To; Jiaqi; 2024; DayIndicator.sav")
df2 <- read_sav("File To; Jiaqi; 2024; No-String2.sav")
df3 <- read_sav("File To; Jiaqi; 2024; No-String3.sav")
df4 <- read_sav("File To; Jiaqi; 2024; No-String4.sav")
df5 <- read_sav("File To; Jiaqi; 2024; No-String5.sav")
df6 <- read_sav("CDO; Dataset; For R.sav")
df <- df |>
  mutate(Temporary_Date = dmy(SurveySubmittedDate)) |>
  group_by(ID) |>
  arrange(ID, Temporary_Date) |>
  mutate(Wave_Day = as.integer(Temporary_Date - first(Temporary_Date) + 1))

## Wave_Day
df2 <- df2 |>
  mutate(Temporary_Date = dmy(TimeStamp_SubmittedDate)) |>
  group_by(ID) |>
  arrange(ID, Temporary_Date) |>
  mutate(Wave_DayIndicator = as.integer(Temporary_Date - first(Temporary_Date) + 1))

## Moments
df3 <- df3 |>
  group_by(ID, Wave_Day) |>
  arrange(ID, Wave_Day, Wave_Time_Submitted) |>
  mutate(Wave_Moment = row_number())

## Conversion on variable type
df6 <- df6 |>
  mutate(across(-c(ID, Wave_Day, Wave_Moment, Wave_Time_Submitted, Wave_Date_Submitted), ~as.numeric(.)))

write_sav(df, "File Back To; Marco; 2024; DayIndicator.sav")
write_sav(df2, "File Back To; Marco; 2024; No-String2.sav")
write_sav(df3, "File Back To; Marco; 2024; No-String3.sav")
write_sav(df6, "CDO; Dataset; Converted to Numeric.sav")
check <- read_sav("File Back To; Marco; 2024; DayIndicator.sav")
