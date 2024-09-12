## Load packages
library(tidyverse)
library(ggplot2)
library(tidyr)
library(haven) ## This library provides functions to read sav file into R
library(lme4)
library(lmerTest)
library(performance)

data_central <- read_sav("Dataset; Bruneau; Central.sav")
data_Cortisol <- readxl::read_excel("Dataset; Bruneau; Cortisol.xlsx")
data_EMA <- read_sav("Dataset; EMA; 2024.7.1.1.sav")
data_EMA <- read_sav("Dataset; 2024.9.2.sav")
data_EMA$Wave_Day <- data_EMA$Wave_Lev2_DayNumber
data_EMA$Wave_Moment <- data_EMA$Wave_Lev1_MomentNumber

## Convert the string to Date
data_EMA$Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat <- as.POSIXct(data_EMA$Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a, format="%Y-%m-%d %H:%M:%S")
data_EMA$Raw_Lev1_MedIntake_TimeStamp_DateTime_TimeFormat <- as.POSIXct(data_EMA$Raw_Lev1_MedIntake_TimeStamp_DateTime, format="%Y-%m-%d %H:%M:%S")

data_EMA$Time_Difference_Hours <- as.numeric(difftime(data_EMA$Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat, data_EMA$Raw_Lev1_MedIntake_TimeStamp_DateTime_TimeFormat, units="hours"))

## Time difference check to see if Follow Up Time is greater than the MedIntake Time
data_EMA_check <- data_EMA |>
  select(c(ID, Wave_Day, Wave_Moment,
           Time_Difference_Hours,
           Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat,
           Raw_Lev1_MedIntake_TimeStamp_DateTime_TimeFormat))

## Chronogical order check for Follow Up Time based on the Wave_Moment
## Within each ID and Wave_Day
data_EMA_check <- data_EMA_check |>
  group_by(ID, Wave_Day) |>
  arrange(Wave_Moment, .by_group = TRUE) |>
  mutate(Between_WaveMoment_TimeDiff = c(NA, diff(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat)),
         Chronological_Order_Check = all(Between_WaveMoment_TimeDiff >= 0 | is.na(Between_WaveMoment_TimeDiff))) |>
  mutate(FU_MedIntake_TooLong = ifelse(Time_Difference_Hours > 24, 1, 0), 
         FU_lessthan_MedIntake = ifelse(Time_Difference_Hours < 0, 1, 0), 
         Wave_Moment_Problematic = ifelse(!Chronological_Order_Check, 1, 0)) |>
  filter(!Chronological_Order_Check | Time_Difference_Hours < 0 | Time_Difference_Hours > 24)

write_sav(data_EMA_check, "Data; EMA; Problems flagged.sav")

## Fix the Wave_Moment problem
data_EMA_merged <- data_EMA |>
  left_join(data_EMA_check, by = c("ID", "Wave_Day", "Wave_Moment", "Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat")) |>
  mutate(Wave_Moment_Problematic = ifelse(is.na(Wave_Moment_Problematic), 0, Wave_Moment_Problematic))

data_EMA_fixed <- data_EMA_merged |>
  group_by(ID, Wave_Day) |>
  mutate(Wave_Moment = ifelse(Wave_Moment_Problematic == 1, 
                              row_number(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat), 
                              Wave_Moment)) |>
  ungroup() |>
  select(-Wave_Moment_Problematic)

data_double_check <- data_EMA_fixed |>
  group_by(ID, Wave_Day) |>
  arrange(Wave_Moment, .by_group = TRUE) |>
  mutate(Between_WaveMoment_TimeDiff = c(NA, diff(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat)),
         Chronological_Order_Check = all(Between_WaveMoment_TimeDiff >= 0 | is.na(Between_WaveMoment_TimeDiff))) |>
  mutate(Wave_Moment_Problematic = ifelse(!Chronological_Order_Check, 1, 0)) |>
  filter(!Chronological_Order_Check) # Double check! 

## Make sure the consecutive day matches the Date variable
data_EMA_fixed <- data_EMA_fixed |>
  mutate(
    Wave_Day_Fixed = as.Date(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat, format = "%Y-%m-%d")
  ) |>
  group_by(ID) |>
  mutate(
    Wave_Day_Fixed = dense_rank(Wave_Day_Fixed) ) |>
  group_by(ID, Wave_Day_Fixed) |>
  mutate(Wave_Moment_Fixed = dense_rank(format(as.POSIXct(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))) |>
  ungroup() |>
  mutate(Wave_Day = Wave_Day_Fixed, 
         Wave_Moment = Wave_Moment_Fixed)

write_sav(data_EMA_fixed, "Data; EMA; Wave Moment Fixed V3.sav")

data_EMA_fixed_check <- data_EMA_fixed |>
  mutate(Date_FU_Lev1 = as.Date(Raw_Lev1_MedIntake_FU_TimeStamp_DateTime1a_TimeFormat)) |>
  group_by(ID, Wave_Day_Fixed) |>
  arrange(Wave_Moment_Fixed, .by_group = TRUE) |>
  mutate(Date_Diff_Days = c(0, diff(Date_FU_Lev1)),
         Wave_Day_Problematic = ifelse(Date_Diff_Days >= 1, 1, 0)) |>
  ungroup() 
# ----------------------------------------------------------------------------------#
## From now on, the corrected data (Wave_Moment corrected) is named data_EMA_fixed ##
## From now on, the corrected data (Wave_Moment corrected) is named data_EMA_fixed ##
## From now on, the corrected data (Wave_Moment corrected) is named data_EMA_fixed ##
# ----------------------------------------------------------------------------------#



