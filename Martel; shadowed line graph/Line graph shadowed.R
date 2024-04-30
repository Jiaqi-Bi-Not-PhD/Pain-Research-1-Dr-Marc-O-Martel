library(tidyverse)
library(ggplot2)
library(tidyr)
library(haven)
library(lme4)
library(lmerTest)
library(readxl)

week_diary <- read_sav("Weekly diaries; 1.sav")
week_diary_global <- week_diary |>
  group_by(Wave_Week, demog_gender) |>
  summarise(
    Mean_Pods_Cog = mean(Pods_Cog, na.rm = TRUE),
    SD = sd(Pods_Cog, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    CI_lower = Mean_Pods_Cog - 1.96 * SE,
    CI_upper = Mean_Pods_Cog + 1.96 * SE
  )

ggplot(week_diary_global, aes(x=Wave_Week, y=Mean_Pods_Cog, group = factor(demog_gender), fill = factor(demog_gender))) + 
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.5) +
  geom_line(aes(color = factor(demog_gender))) +
  scale_color_discrete(name = "Gender") +
  scale_x_continuous(breaks = seq(14, 42, by = 7), limits = c(14, 42)) +
  scale_y_continuous(breaks = seq(1, 5, by = 0.5), limits = c(1, 5)) +
  labs(y = "Cognitive Side Effect", x = "Week", color = "Gender", fill = "Gender") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

week_diary_global2 <- week_diary |>
  group_by(Wave_Week, demog_gender) |>
  summarise(
    Mean_Pods_Phy = mean(Pods_Phys, na.rm = TRUE),
    SD = sd(Pods_Phys, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    CI_lower = Mean_Pods_Phy - 1.96 * SE,
    CI_upper = Mean_Pods_Phy + 1.96 * SE
  )

ggplot(week_diary_global2, aes(x=Wave_Week, y=Mean_Pods_Phy, group = factor(demog_gender), fill = factor(demog_gender))) + 
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.5) +
  geom_line(aes(color = factor(demog_gender))) +
  scale_color_discrete(name = "Gender") +
  scale_x_continuous(breaks = seq(14, 42, by = 7), limits = c(14, 42)) +
  scale_y_continuous(breaks = seq(1, 5, by = 0.5), limits = c(1, 5)) +
  labs(y = "Physical Side Effect", x = "Week", color = "Gender", fill = "Gender") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

## Multilevel plot
library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(modelr)
library(broom)

## NA Cog
model_NA_cog <- lmer(Pods_Cog ~ panas_Tot_NA + (1 + panas_Tot_NA|ID) , data = week_diary)
summary(model_NA_cog)

predicted_values <- modelr::data_grid(week_diary, ID, panas_Tot_NA) %>%
  modelr::add_predictions(model_NA_cog)

## The mean line
average_predictions <- predicted_values |>
  group_by(panas_Tot_NA) |>
  summarise(mean_predicted = mean(pred, na.rm = TRUE))

ggplot() +
  geom_line(data = predicted_values, aes(x = panas_Tot_NA, y = pred, color = factor(ID))) +
  geom_line(data = average_predictions, aes(x = panas_Tot_NA, y = mean_predicted), color = "black", size = 1) +
  labs(title = "Association between cognitive side effects and negative affect", 
       x = "Negative affect", 
       y = "Cognitive side effect") +
  ylim(0,10) +
  theme_minimal() +
  theme(element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Plot
ggplot(week_diary, aes(x = Pods_Cog, y = factor(Wave_Week), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Cognitive Side Effects", option = "D") +
  labs(x="Cognitive Side Effects", y = "Days") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8), 
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold", size = 14)
  )

ggplot(week_diary, aes(x = Pods_Phys, y = factor(Wave_Week), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient(name = "Physical Side Effects", low = "#B6C9B9", high = "#3D4351") +
  labs(x="Physical Side Effects", y = "Days") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold", size = 14)
  )
