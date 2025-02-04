library(tidyverse)
library(ggplot2)
library(tidyr)
library(haven) ## This library provides functions to read sav file into R
library(lme4)
library(lmerTest)
library(performance)
library(viridis)

df_bruneau <- read_sav("Dataset; 2024.12.23.sav")
model1 <- lmer(Raw_Lev1_PANAS_5 ~ cLev1_Pain + cLev1_PA + cLev1_PCS + cLev3_Log10_cortisol + (1|ID/Wave_Lev2_Day), data = df_bruneau)
summary(model1)
