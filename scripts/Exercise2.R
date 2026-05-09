install.packages(c("randomForest", "pdp", "ggplot2", "dplyr", "viridis", "cowplot"))

library(randomForest)
library(pdp)
library(ggplot2)
library(dplyr)
library(viridis)
library(cowplot)


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


pdp_2d <- partial(
  rf_bike,
  pred.var = c("temp", "hum"),
  train = bike_data
)

# Compute tile dimensions from grid spacing to avoid holes
temp_step <- diff(sort(unique(pdp_2d$temp)))[1]
hum_step  <- diff(sort(unique(pdp_2d$hum)))[1]

# Main heatmap
plot_2d_base <- ggplot(pdp_2d, aes(x = temp, y = hum)) +
  geom_tile(aes(fill = yhat), width = temp_step, height = hum_step) +
  scale_fill_viridis(name = "Predicted\nBike Count") +
  geom_contour(aes(z = yhat), color = "white", alpha = 0.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  labs(title = "2D PDP: Temperature vs Humidity", y = "Humidity")

# Top marginal: temperature density
top_density <- ggplot(bike_data, aes(x = temp)) +
  geom_density(aes(y = after_stat(density)),
               fill = "steelblue",
               alpha = 0.4,
               color = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

# Right marginal: humidity density (flipped)
right_density <- ggplot(bike_data, aes(x = hum)) +
  geom_density(aes(y = after_stat(density)),
               fill = "steelblue",
               alpha = 0.4,
               color = NA) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

# Blank spacer for top-right corner
blank <- ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Assemble
plot_2d_marginal <- plot_grid(
  top_density,   blank,
  plot_2d_base,  right_density,
  ncol        = 2,
  rel_widths  = c(4, 1),
  rel_heights = c(1, 4)
)

print(plot_2d_marginal)

ggsave("figures/pdp_2d_temp_hum.png", plot = plot_2d_marginal, width = 8, height = 6)