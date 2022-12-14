% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpl_predictions.R
\name{fpl_predictions}
\alias{fpl_predictions}
\title{Function used behind the scenes to generate predictions for xP (expected points)}
\usage{
fpl_predictions(data, split_val = 0.9, predictors = everything())
}
\arguments{
\item{data}{}

\item{split_val}{value between 0 and 1 to define proportion to assign to training and testing split}

\item{predictors}{predict based on either the odds or index methods}
}
\value{
Regression model outputs.
}
\description{
Split data into a testing and training set to carry out machine learning techniques / regression modelling
}
