## Check normal distribution
library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(modelr)
library(broom)

df <- read_sav("File To; Jiaqi; 2024.2.sav")

df |> ggplot(aes(x = AggAtLev3_CanFreqYN_Tot)) +
  geom_density()

df |> ggplot(aes(x = AggAtLev3_CanFreqYN_Tot)) +
  geom_histogram()

shapiro.test(df$AggAtLev3_CanFreqYN_Tot)
