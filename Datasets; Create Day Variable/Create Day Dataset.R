## Create Day indicators based on the Date
## And save to sav file
library(tidyverse)
library(haven)
library(lubridate)
df <- read_sav("File To; Jiaqi; 2024; DayIndicator.sav")
df2 <- read_sav("File To; Jiaqi; 2024; No-String2.sav")
df3 <- read_sav("File To; Jiaqi; 2024; No-String3.sav")
df <- df |>
  mutate(Temporary_Date = dmy(SurveySubmittedDate)) |>
  group_by(ID) |>
  arrange(ID, Temporary_Date) |>
  mutate(Wave_Day = as.integer(Temporary_Date - first(Temporary_Date) + 1))

df2 <- df2 |>
  mutate(Temporary_Date = dmy(TimeStamp_SubmittedDate)) |>
  group_by(ID) |>
  arrange(ID, Temporary_Date) |>
  mutate(Wave_DayIndicator = as.integer(Temporary_Date - first(Temporary_Date) + 1))

df3 <- df3 |>
  group_by(ID, Wave_Day) |>
  arrange(ID, Wave_Day, Wave_Time_Submitted) |>
  mutate(Wave_Moment = row_number())

write_sav(df, "File Back To; Marco; 2024; DayIndicator.sav")
write_sav(df2, "File Back To; Marco; 2024; No-String2.sav")
write_sav(df3, "File Back To; Marco; 2024; No-String3.sav")
check <- read_sav("File Back To; Marco; 2024; DayIndicator.sav")
