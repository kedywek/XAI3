install.packages(c("randomForest", "pdp", "ggplot2", "dplyr", "gridExtra", "viridis"))

library(randomForest)
library(pdp)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)


bike_data <- read.csv("data/day.csv")

bike_data$dteday <- as.Date(bike_data$dteday)
bike_data$days_since_2011 <- as.numeric(bike_data$dteday - min(bike_data$dteday))


set.seed(42)

rf_bike <- randomForest(
  cnt ~ days_since_2011 + temp + hum + windspeed,
  data = bike_data,
  ntree = 500,
  importance = TRUE
)

print(rf_bike)
importance(rf_bike)
varImpPlot(rf_bike)


features <- c("days_since_2011", "temp", "hum", "windspeed")

pdp_list <- lapply(features, function(x) {

  pd <- partial(rf_bike, pred.var = x, train = bike_data)

  ggplot(pd, aes(x = .data[[x]], y = yhat)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_rug(data = bike_data, aes(x = .data[[x]]), inherit.aes = FALSE) +
    theme_minimal() +
    labs(
      title = paste("PDP for", x),
      x = x,
      y = "Predicted Bike Count"
    )
})

grid.arrange(grobs = pdp_list, ncol = 2)

ggsave("figures/pdp_days_since_2011.png", plot = pdp_list[[1]], width = 6, height = 4)
ggsave("figures/pdp_temp.png",            plot = pdp_list[[2]], width = 6, height = 4)
ggsave("figures/pdp_hum.png",             plot = pdp_list[[3]], width = 6, height = 4)
ggsave("figures/pdp_windspeed.png",       plot = pdp_list[[4]], width = 6, height = 4)