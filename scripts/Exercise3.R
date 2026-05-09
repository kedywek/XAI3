install.packages(c("randomForest", "pdp", "ggplot2", "dplyr", "gridExtra"))
library(randomForest)
library(pdp)
library(ggplot2)
library(dplyr)
library(gridExtra)


house_data <- read.csv("data/kc_house_data.csv")

house_features <- house_data %>%
  select(price, bedrooms, bathrooms, sqft_living, sqft_lot, floors, yr_built)

set.seed(42)
house_sample <- house_features %>% sample_n(5000)


rf_house <- randomForest(
  price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + yr_built,
  data = house_sample,
  ntree = 300,
  importance = TRUE
)

print(rf_house)
importance(rf_house)
varImpPlot(rf_house)


house_vars <- c("bedrooms", "bathrooms", "sqft_living", "floors")

house_pdp_list <- lapply(house_vars, function(x) {

  pd <- partial(rf_house, pred.var = x, train = house_sample)

  ggplot(pd, aes(x = .data[[x]], y = yhat)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_rug(data = house_sample, aes(x = .data[[x]]), inherit.aes = FALSE) +
    theme_minimal() +
    labs(
      title = paste("PDP for", x),
      x = x,
      y = "Predicted House Price"
    )
})

grid.arrange(grobs = house_pdp_list, ncol = 2)

ggsave("figures/pdp_bedrooms.png",    plot = house_pdp_list[[1]], width = 6, height = 4)
ggsave("figures/pdp_bathrooms.png",   plot = house_pdp_list[[2]], width = 6, height = 4)
ggsave("figures/pdp_sqft_living.png", plot = house_pdp_list[[3]], width = 6, height = 4)
ggsave("figures/pdp_floors.png",      plot = house_pdp_list[[4]], width = 6, height = 4)