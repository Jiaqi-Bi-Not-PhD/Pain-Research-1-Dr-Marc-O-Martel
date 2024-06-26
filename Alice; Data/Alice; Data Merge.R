library(tidyverse)
library(ggplot2)
library(tidyr)
library(haven)
library(lme4)
library(lmerTest)
library(readxl)

data_alice <- read_sav("Dataset; Bruneau; Central.sav")
data_cortisol <- read_excel("Dataset; Bruneau; Cortisol copy.xlsx")
data_cortisol <- data_cortisol[,-c(8:18)]
data_cortisol$Mean <- as.numeric(data_cortisol$Mean)
data_cortisol$Submission_Code <- 1

data_alice <- data_alice |>
  group_by(ID, Diary_Day) |>
  mutate(Sample_align = ifelse(Diary_Submission_Code == 1, cumsum(Diary_Submission_Code == 1), NA_integer_))

## Create additional variables in data_alice in order to make the merge easier
data_merged <- full_join(data_cortisol, data_alice, by = c("ID" = "ID", 
                                                            "Day_number" = "Saliva_Day", 
                                                            "Sample_number" = "Sample_align"))
data_merged2 <- left_join(data_cortisol, data_alice, by = c("ID" = "ID", 
                                                            "Day_number" = "Saliva_Day", 
                                                            "Submission_Code" = "Diary_Submission_Code"))

data_check <- read_sav("Merged Dataset; Bruneau.sav")

## Check everything
check <- data_merged |>
  select(c(Name, Mean, `CV%`, ID, Day_number, Sample_number, 
           Submission_Code, Diary_Day, Diary_Moment, Diary_Submission_Raw))
check <- check |>
  filter(ID == "MGH_EMA_009")

data_merged <- data_merged |> mutate(CV_Percent = `CV%`) |> 
  select(-`CV%`)

check <- data_merged |>
  select(c(Name, ID, Day_number, Sample_number, 
           Mean, Std_Dev, CV_Percent, Raw_Lev1_FU_response_type))

write_sav(data_merged, "Merged Dataset2; Bruneau.sav")
