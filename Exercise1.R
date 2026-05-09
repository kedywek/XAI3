install.packages("gridExtra")
install.packages(c("randomForest", "pdp", "ggplot2", "dplyr"))

library(randomForest)
library(pdp)
library(ggplot2)
library(dplyr)

bike_data <- read.csv("day.csv")

bike_data$dteday <- as.Date(bike_data$dteday)
bike_data$days_since_2011 <- as.numeric(bike_data$dteday - min(bike_data$dteday))

set.seed(42)
rf_bike <- randomForest(cnt ~ days_since_2011 + temp + hum + windspeed, 
                        data = bike_data, ntree = 500)

features <- c("days_since_2011", "temp", "hum", "windspeed")
pdp_list <- lapply(features, function(x) {
  partial(rf_bike, pred.var = x, plot = TRUE, plot.engine = "ggplot2") +
    ggtitle(paste("PDP for", x))
})

do.call(gridExtra::grid.arrange, c(pdp_list, ncol = 2))