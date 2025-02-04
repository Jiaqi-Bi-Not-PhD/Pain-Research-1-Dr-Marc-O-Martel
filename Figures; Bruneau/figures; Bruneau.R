library(haven)
library(ggplot2)
library(tidyverse)
library(lme4)
library(lmerTest)
data_bruneau <- read_sav("Dataset; 2024.10.25.sav")

source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/extract_model.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_RE_binomial.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_random_slope.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/plot_RE_poisson.R")
source("~/Desktop/PhD Biostatistics/R Packages/ggmultilevel/R/REplot.R")
## Graph: Multilevel linear reg graph (LMM)
## Varname (y-axis): ComP_Lev1_OCS_Ave
## Varname (x-axis): Raw_Lev1_MedIntake_FU_Pain_Intens
model1 <- lmer(ComP_Lev1_OCS_Ave ~ Raw_Lev1_MedIntake_FU_Pain_Intens +  (Raw_Lev1_MedIntake_FU_Pain_Intens|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model1)
performance::r2(model1)
model1_null <- lmer(ComP_Lev1_OCS_Ave ~ 1 + (1|ID), data = data_bruneau,
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
performance::r2(model1_null)
# 0.617 - 0.566 = 0.051

ggsave(REplot(model = model1, data = data_bruneau, 
                  predictor = "Raw_Lev1_MedIntake_FU_Pain_Intens",
                  outcome = "ComP_Lev1_OCS_Ave", grouping_var = "ID", x_limits = c(0,100), 
                  y_limits = c(0,100), x_breaks = seq(0, 100, by = 25), 
                  y_breaks = seq(0, 100, by = 25), 
                  x_num_size = 26, y_num_size = 26,
                  plot_title = "Raw_Lev1_MedIntake_FU_Pain_Intens vs. ComP_Lev1_OCS_Ave"), 
       dpi=1200, filename="Raw_Lev1_MedIntake_FU_Pain_Intens vs. ComP_Lev1_OCS_Ave; 1200dpi.tif")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_OCS_Ave 
## Varname (x-axis): ComP_Lev1_PA 
model2 <- lmer(ComP_Lev1_OCS_Ave ~ ComP_Lev1_PA + (ComP_Lev1_PA|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model2)
model2_null <- lmer(ComP_Lev1_OCS_Ave ~ 1 + (1|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

performance::r2(model2)
performance::r2(model2_null)
# 0.638 - 0.566 = 0.072



REplot(model = model2, data = data_bruneau, predictor = "ComP_Lev1_PA",
                  outcome = "ComP_Lev1_OCS_Ave", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,100), x_breaks = seq(0, 15, by = 3), 
                  y_breaks = seq(0, 100, by = 25), 
                  x_num_size = 26, y_num_size = 26,
                  plot_title = "ComP_Lev1_PA vs. ComP_Lev1_OCS_Ave")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_OCS_Ave 
## Varname (x-axis): ComP_Lev1_NA 
model3 <- lmer(ComP_Lev1_OCS_Ave ~ ComP_Lev1_NA + (ComP_Lev1_NA|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model3)
performance::r2(model3)

model3_null <- lmer(ComP_Lev1_OCS_Ave ~ 1 + (1|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
performance::r2(model3_null)
# 0.633 - 0.566 = 0.067


REplot(model = model3, data = data_bruneau, predictor = "ComP_Lev1_NA",
                  outcome = "ComP_Lev1_OCS_Ave", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,100), x_breaks = seq(0, 15, by = 3), 
                  y_breaks = seq(0, 100, by = 25), 
                  x_num_size = 26, y_num_size = 26,
                  plot_title = "ComP_Lev1_NA vs. ComP_Lev1_OCS_Ave")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_OCS_Ave 
## Varname (x-axis): ComP_Lev1_PCS 
model4 <- lmer(ComP_Lev1_OCS_Ave ~ ComP_Lev1_PCS + (ComP_Lev1_PCS|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model4)
model4_null <- lmer(ComP_Lev1_OCS_Ave ~ 1 + (1|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
performance::r2(model4)
performance::r2(model4_null)
# 0.753 - 0.566 = 0.187


REplot(model = model4, data = data_bruneau, predictor = "ComP_Lev1_PCS",
                  outcome = "ComP_Lev1_OCS_Ave", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,100), x_breaks = seq(0, 15, by = 3), 
                  y_breaks = seq(0, 100, by = 25), 
                  y_num_size = 26, x_num_size = 26,
                  plot_title = "ComP_Lev1_PCS vs. ComP_Lev1_OCS_Ave")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_OCS_Ave 
## Varname (x-axis): ComP_Lev1_OWI_Tot 
model5 <- lmer(ComP_Lev1_OCS_Ave ~ ComP_Lev1_OWI_Tot + (ComP_Lev1_OWI_Tot|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model5)
model5_null <- lmer(ComP_Lev1_OCS_Ave ~ 1 + (1|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
performance::r2(model5)
performance::r2(model5_null)
# 0.617 - 0.566 = 0.051

REplot(model = model5, data = data_bruneau, predictor = "ComP_Lev1_OWI_Tot",
                  outcome = "ComP_Lev1_OCS_Ave", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,100), x_breaks = seq(0, 15, by = 3), 
                  y_breaks = seq(0, 100, by = 25), 
                  y_num_size = 26, x_num_size = 26,
                  plot_title = "ComP_Lev1_OWI_Tot vs. ComP_Lev1_OCS_Ave")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## Varname (x-axis): Raw_Lev1_MedIntake_FU_Pain_Intens 
model6 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ Raw_Lev1_MedIntake_FU_Pain_Intens +  (Raw_Lev1_MedIntake_FU_Pain_Intens|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model6)

plot_random_slope(model = model6, data = data_bruneau, predictor = "Raw_Lev1_MedIntake_FU_Pain_Intens",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,100), 
                  y_limits = c(0,225), x_breaks = seq(0, 100, by = 25), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "Raw_Lev1_MedIntake_FU_Pain_Intens vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## arname (x-axis): ComP_Lev1_PA  
model7 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ ComP_Lev1_PA + (ComP_Lev1_PA|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model7)

plot_random_slope(model = model7, data = data_bruneau, predictor = "ComP_Lev1_PA",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,225), x_breaks = seq(0, 15, by = 1), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "ComP_Lev1_PA vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## Varname (x-axis): ComP_Lev1_NA  
model8 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ ComP_Lev1_NA + (ComP_Lev1_NA|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model8)

plot_random_slope(model = model8, data = data_bruneau, predictor = "ComP_Lev1_NA",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,225), x_breaks = seq(0, 15, by = 1), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "ComP_Lev1_NA vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## Varname (x-axis): ComP_Lev1_PCS 
model9 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ ComP_Lev1_PCS + (ComP_Lev1_PCS|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model9)

plot_random_slope(model = model9, data = data_bruneau, predictor = "ComP_Lev1_PCS",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,225), x_breaks = seq(0, 15, by = 1), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "ComP_Lev1_PCS vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## Varname (x-axis): ComP_Lev1_OWI_Tot 
model10 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ ComP_Lev1_OWI_Tot + (ComP_Lev1_OWI_Tot|ID), data = data_bruneau,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model10)

plot_random_slope(model = model10, data = data_bruneau, predictor = "ComP_Lev1_OWI_Tot",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,225), x_breaks = seq(0, 15, by = 1), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "ComP_Lev1_OWI_Tot vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel linear reg graph (LMM) 
## Varname (y-axis): ComP_Lev1_Opioids_MED_Tot 
## Varname (x-axis): ComP_Lev1_OCS_Ave  

model11 <- lmer(ComP_Lev1_Opioids_MED_Tot ~ ComP_Lev1_OCS_Ave + (ComP_Lev1_OCS_Ave|ID), data = data_bruneau,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(model11)

plot_random_slope(model = model11, data = data_bruneau, predictor = "ComP_Lev1_OCS_Ave",
                  outcome = "ComP_Lev1_Opioids_MED_Tot", grouping_var = "ID", x_limits = c(0,100), 
                  y_limits = c(0,225), x_breaks = seq(0, 100, by = 25), 
                  y_breaks = seq(0, 225, by = 25), 
                  plot_title = "ComP_Lev1_OCS_Ave vs. ComP_Lev1_Opioids_MED_Tot")

## Graph: Multilevel logistic reg graph (GLMM) 
## Varname (y-axis): Raw_Lev2_EndDay_Misuse_OtherWay 
## Varname (x-axis): ComP_Lev2_Aggreg_PCS 

model12 <- glmer(Raw_Lev2_EndDay_Misuse_OtherWay ~ ComP_Lev2_Aggreg_PCS + (ComP_Lev2_Aggreg_PCS|ID), data = data_bruneau,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                 family = "binomial")
summary(model12)

REplot(model = model12, data = data_bruneau, predictor = "ComP_Lev2_Aggreg_PCS",
                  outcome = "Raw_Lev2_EndDay_Misuse_OtherWay", 
                  grouping_var = "ID", x_limits = c(0,15), 
                  y_limits = c(0,0.5), x_breaks = seq(0, 15, by = 3), 
                  y_breaks = seq(0, 0.5, by = 0.1), 
                  family = "binomial",
                  plot_title = "ComP_Lev2_Aggreg_PCS vs. Raw_Lev2_EndDay_Misuse_OtherWay",
                  y_scale = "probability", x_num_size = 26, y_num_size = 26)

## Graph: Multilevel logistic reg graph (GLMM) 
## Varname (y-axis): Raw_Lev2_EndDay_Misuse_OtherWay 
## Varname (x-axis): ComP_Lev2_Aggreg_OCS_Ave 

model13 <- glmer(Raw_Lev2_EndDay_Misuse_OtherWay ~ ComP_Lev2_Aggreg_OCS_Ave + (ComP_Lev2_Aggreg_OCS_Ave|ID), data = data_bruneau,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                 family = "binomial")
summary(model13)

REplot(model = model13, data = data_bruneau, predictor = "ComP_Lev2_Aggreg_OCS_Ave",
                        outcome = "Raw_Lev2_EndDay_Misuse_OtherWay", 
                        grouping_var = "ID", x_limits = c(0,100), 
                        y_limits = c(0,0.5), x_breaks = seq(0, 100, by = 25), 
                        y_breaks = seq(0, 0.5, by = 0.1), 
                        family = "binomial",
                        plot_title = "ComP_Lev2_Aggreg_OCS_Ave vs. Raw_Lev2_EndDay_Misuse_OtherWay",
                        y_scale = "probability", x_num_size = 26, y_num_size = 26)

## Effect Size Estimates
# Effect size estimates
# Outcome: ComP_Lev2_Aggreg_MED_Tot
# ComP_Lev2_Aggreg_PainIntens
model_null <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ 1 + (1|ID), data = data_bruneau)
model_1 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_PainIntens + (ComP_Lev2_Aggreg_PainIntens|ID), data = data_bruneau)
model_2 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_PA + (ComP_Lev2_Aggreg_PA|ID), data = data_bruneau)
model_3 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_NA + (ComP_Lev2_Aggreg_NA|ID), data = data_bruneau)
model_4 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_PCS + (ComP_Lev2_Aggreg_PCS|ID), data = data_bruneau)
model_5 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_OWI_Tot + (ComP_Lev2_Aggreg_OWI_Tot|ID), data = data_bruneau)
model_6 <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Aggreg_OCS_Ave + (ComP_Lev2_Aggreg_OCS_Ave|ID), data = data_bruneau)
performance::r2(model_null)
performance::r2(model_1) # 0.872 - 0.77 = 0.102
performance::r2(model_2) # 0.825 - 0.77 = 0.055
performance::r2(model_3) # 0.882 - 0.77 = 0.112
performance::r2(model_4) # 0.895 - 0.77 = 0.125
performance::r2(model_5) # 0.824 - 0.77 = 0.054
performance::r2(model_6) # 0.878 - 0.77 = 0.108
# ComP_Lev2_Aggreg_PA
# ComP_Lev2_Aggreg_NA
# ComP_Lev2_Aggreg_PCS
# ComP_Lev2_Aggreg_OWI_Tot
# ComP_Lev2_Aggreg_OCS_Ave
