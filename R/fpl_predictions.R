


#' Function used behind the scenes to generate predictions for xP (expected points)
#'
#' Split data into a testing and training set to carry out machine learning techniques / regression modelling
#'
#' @param data
#'
#' @param split_val value between 0 and 1 to define proportion to assign to training and testing split
#'
#' @param predictors predict based on either the odds or index methods
#'
#' @export



##################################################


# fpl_predictions

# predictors <- c("total_points", "ict_index")

# split_val <- 0.9


# data argument will accept any dataset to fit a linear model for predicting "total_points"
# "total_points" must be present, for example:
# data <- players_split
# data <- all_gameweeks


fpl_predictions <- function(data, split_val = 0.9, predictors = everything()) {
  split <- round(nrow(data) * split_val)
  # Create train
  train <- data[1:split, ]
  # Create test
  test <- data[(split + 1):nrow(data), ]

  model <- lm(total_points ~ ., data = train %>% select(predictors))
  p <- predict(model, test)
  test$p <- round(p, 2)
  output <- list(model, train, test)

  return(output)
}






