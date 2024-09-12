## Multilevel plot
ggmultilevel <- function(data, x, y, model, cluster, title, xlab, ylab) {
  data <- data |>
    dplyr::filter(!is.na(y) & !is.na(x))
  ## New data - for plot
  new_data <- expand.grcluster(
    x = seq(min(data$x, na.rm = TRUE),
            max(data$x, na.rm = TRUE), length.out = 100),
    cluster = unique(data$cluster)
  )
  
  new_data$predicted <- predict(model, newdata = new_data, re.form = NULL) 
  
  ## The mean line
  average_predictions <- new_data |>
    group_by(x) |>
    summarise(mean_predicted = mean(predicted, na.rm = TRUE))
  
  ggplot() +
    geom_line(data = new_data, aes(x = x, y = predicted, group = cluster), color = "grey") +
    geom_line(data = average_predictions, aes(x = x, y = mean_predicted), color = "black", size = 1) +
    labs(title = title, 
         x = xlab, 
         y = ylab) +
    theme_minimal() +
    theme(element_blank()) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
