library(tidyverse)
library(ggplot2)
library(tidyr)
library(haven)
library(lme4)
library(lmerTest)
library(readxl)

data_alice <- read_sav("Dataset; Bruneau; EMA.sav")
data_cortisol <- read_excel("Dataset; Bruneau; Cortisol.xlsx")
data_cortisol <- data_cortisol[,-c(8:18)]

## Create additional variables in data_alice in order to make the merge easier
data_merged <- full_join(data_cortisol, data_alice, by = c("ID" = "ID", 
                                                            "Day_number" = "Wave_Lev2_DayNumber", 
                                                            "Sample_number" = "Wave_Lev1_MomentNumber"))

data_merged <- data_merged |> mutate(CV_Percent = `CV%`,
                                     Wave_Lev2_DayNumber = Day_number, 
                                     Wave_Lev1_MomentNumber = Sample_number) |> 
  select(-`CV%`)
check <- data_merged |>
  select(c(Name, ID, Day_number, Sample_number, 
           Mean, Std_Dev, CV_Percent, Raw_Lev1_FU_response_type))

write_sav(data_merged, "Merged Dataset; Bruneau.sav")
