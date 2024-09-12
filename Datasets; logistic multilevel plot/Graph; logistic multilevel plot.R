library(haven)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(car)

logistic_data <- read_sav("File To; Jiaqi; 2024.2.sav") |>
  filter(!ID %in% c("MGH_EMA_036", "MGH_EMA_038", "MGH_EMA_047"))
multi_logistic <- glmer(Lev2_Misuse ~ Lev2_Pain + (Lev2_Pain|ID), data = logistic_data,
                        family = binomial())
summary(multi_logistic)

pain_range <- seq(min(logistic_data$Lev2_Pain, na.rm = TRUE), max(logistic_data$Lev2_Pain, na.rm = TRUE), length.out = 100)
new_data <- logistic_data |>
  select(ID) |>
  distinct() |>
  crossing(Lev2_Pain = pain_range) |>
  filter(!is.na(Lev2_Pain))

## Plot the probability
new_data$fixed_effect <- predict(multi_logistic, newdata = new_data, re.form = NA, type = "response")
new_data$random_effect <- predict(multi_logistic, newdata = new_data, type = "response")

ggplot(new_data, aes(x = Lev2_Pain)) +
  geom_line(aes(y = random_effect, group = ID), color = "grey", linetype = "dashed") +
  geom_line(aes(y = fixed_effect), color = "black", size = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(y = "Probability", x = "Lev2_Pain", title = "Pain vs Probability") +
  theme_minimal() +
  theme(element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
