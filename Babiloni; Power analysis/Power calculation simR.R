library(readr)
library(dplyr)
library(tidyverse)
library(simr)
library(haven)
library(lmerTest)
Bab <- read_sav("Babiloni; Power analysis 1.2.sav")
## converts as factors only labelled data from the step before 
## (see ?as_factor) for more information.
Bab <- as_factor(Bab, only_labelled = TRUE)
Bab2 <- Bab |>
  group_by(ID) |>
  summarise(n = sum(is.na(Lev2_FlareAMPM))) |>
  filter(n != 14)
Bab <- Bab |>
  filter(ID %in% Bab2$ID) |>
  filter(Wave_Diary_Moment == 1)

## GLMM model
model_glmm <- glmer(Lev2_FlareAMPM ~ . + (1 | ID) - Wave_Diary_Day - ID - Wave_Diary_Moment - Lev2_PainInt, data = Bab, family = binomial())
summary(model_glmm)

## Power calculation for Lev2_PCS
model_glmm_PCS <- glmer(Lev2_FlareAMPM ~ Lev2_PCS + (1 | ID), data = Bab, family = binomial())
fixef(model_glmm_PCS)["Lev2_PCS"] <- log(1.5) # This line sets the desired effect size, may skip
summary(model_glmm_PCS)
powerSim(model_glmm_PCS)

model_glmm_PCS_n80 <- extend(model_glmm_PCS, along = "ID", n = 80)
powerSim(model_glmm_PCS_n80)
plot_power <- powerCurve(model_glmm_PCS_n80, along = "ID")
plot(plot_power, xlab = "# of Patients")

## Power calculation for Lev2_NA
model_glmm_NA <- glmer(Lev2_FlareAMPM ~ Lev2_NA + (1 | ID), data = Bab, family = binomial())
summary(model_glmm_NA)
powerSim(model_glmm_NA)
fixef(model_glmm_NA)["Lev2_NA"] <- log(1.5) # This line sets the desired effect size, may skip

model_glmm_NA_n80 <- extend(model_glmm_NA, along = "ID", n = 80)
powerSim(model_glmm_NA_n80)
plot_power <- powerCurve(model_glmm_NA_n80, along = "ID")
plot(plot_power, xlab = "# of Patients")
