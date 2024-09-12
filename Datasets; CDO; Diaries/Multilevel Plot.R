## Datasets; CDO; Diaries
library(haven)
CDO_diaries <- read_sav("CDO; Diaries; 7.1.sav")
CDO_diaries_multilevel <- lmer(AggAtLev2_Day_AllLev1_CanCravings ~ AggAtLev2_Day_AllLev1_CanHedonic + (1 + AggAtLev2_Day_AllLev1_CanHedonic|ID), data = CDO_diaries)
summary(CDO_diaries_multilevel)
CDO_diaries <- CDO_diaries |>
  filter(!is.na(AggAtLev2_Day_AllLev1_CanCravings) & !is.na(AggAtLev2_Day_AllLev1_CanHedonic) & !is.na(AggAtLev2_Day_AllLev1_CanHedonic))

predicted_values <- modelr::data_grid(CDO_diaries, ID, AggAtLev2_Day_AllLev1_CanHedonic) |>
  modelr::add_predictions(CDO_diaries_multilevel)

average_predictions <- predicted_values |>
  group_by(AggAtLev2_Day_AllLev1_CanHedonic) |>
  summarise(mean_predicted = mean(pred, na.rm = TRUE))


  ggplot() +
  geom_line(data = predicted_values, aes(AggAtLev2_Day_AllLev1_CanHedonic, pred, color = ID)) +
  geom_line(data = average_predictions, aes(x = AggAtLev2_Day_AllLev1_CanHedonic, y = mean_predicted), color = "black", size = 1) +
  geom_point(data = CDO_diaries, aes(AggAtLev2_Day_AllLev1_CanHedonic, AggAtLev2_Day_AllLev1_CanCravings, color = ID)) +
  theme_minimal() +
  theme(element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")
