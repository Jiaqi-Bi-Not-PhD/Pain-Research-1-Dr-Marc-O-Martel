## Multilevel plot
library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(modelr)
library(broom)

df <- read_sav("CDO; Diaries; 3.sav")
df2 <- df |> select(c(AggregDay_All_CanCravings, AggregDay_All_PainAve, ID, Wave_Day))

model1 <- lmer(AggregDay_All_CanCravings ~ AggregDay_All_PainAve + (1 + AggregDay_All_PainAve|ID), data = df)
summary(model1)

## Plot - Method 1
predicted_values <- modelr::data_grid(df, ID, AggregDay_All_PainAve) %>%
                    modelr::add_predictions(model1)

predicted_values |>
  ggplot(aes(AggregDay_All_PainAve, pred, color = ID))+
  geom_line()+
  geom_point(data = df, aes(AggregDay_All_PainAve, AggregDay_All_CanCravings, color = ID))


## Plot - Method 2
## New data - for plot
new_data <- expand.grid(
  AggregDay_All_PainAve = seq(min(df$AggregDay_All_PainAve, na.rm = TRUE), 
                              max(df$AggregDay_All_PainAve, na.rm = TRUE), length.out = 100),
  ID = unique(df$ID)
)

new_data$predicted <- predict(model1, newdata = new_data, re.form = NULL) 

## The mean line
average_predictions <- new_data |>
  group_by(AggregDay_All_PainAve) |>
  summarise(mean_predicted = mean(predicted, na.rm = TRUE))

ggplot() +
  geom_line(data = new_data, aes(x = AggregDay_All_PainAve, y = predicted, group = ID), color = "grey") +
  geom_line(data = average_predictions, aes(x = AggregDay_All_PainAve, y = mean_predicted), color = "black", size = 1) +
  labs(title = "Model 1", 
       x = "Daily pain intensity", 
       y = "Cannabis craving") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
  
