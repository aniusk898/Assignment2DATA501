#' Cooks Distance Measure Calculation
#'
#' This function calculates Cooks distance, which is a measure of influence used to assess the impact that an observation has on the estimates of a least squares regression model.
#'
#' @param model An object of class \code{lm} that is the linear model for which Cooks distance is going to be calculated.
#'
#' @return A numerical vector that contains the Cooks distance of each observation in the model.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' cooks_d <- cooks.distance(model)
#' @importFrom stats resid hatvalues df.residual rstudent
cooks_distance <- function(model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }

  res <- resid(model)
  lev <- hatvalues(model)
  mse <- sum(res ^ 2) / df.residual(model)
  p <- length(model$coefficients)
  cdist <- (res ^ 2 / (p * mse)) * (lev / (1 - lev) ^ 2)
  return(cdist)
}

#' Difference in fits Measure Calculation
#'
#' This function calculates the difference in fits which is a metric used to identify influential data points by measuring how much the predicted value changes, in terms of standard deviations, when a specific observation is removed from the model.
#'
#' @inheritParams cooks_distance
#'
#' @return A numerical vector that contains the difference in fits of each observation in the model.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' dfits <- diffits(model)
#' @importFrom stats resid hatvalues df.residual rstudent
diffits <- function(model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }

  res_stand <- rstudent(model)
  lev <- hatvalues(model)
  diffits <- res_stand * sqrt(lev / (1 - lev))
  return(diffits)
}

#' Hadis Influence Measure Calculation
#'
#' This function calculates the Hadis influence measure that considers that influential observations may occur in the response variable, the predictors, or in both.
#'
#' @inheritParams cooks_distance
#'
#' @return A numerical vector that contains Hadis influence measure of each observation in the model.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' hadis_influences <- hadis_influence(model)
#' @importFrom stats resid hatvalues df.residual rstudent coef
hadis_influence <- function(model) {
  if (!inherits(model, "lm")) {
    stop("The argument 'model' must be an object of class 'lm'")
  }

  lev <- hatvalues(model)
  sse <- sum(resid(model) ^ 2)
  di <- resid(model) / sqrt(sse)

  di[di >= 1] <- 0.9999

  p <- length(coef(model)) - 1

  hadi_influence <- (lev / (1 - lev)) + ((p + 1) / (1 - lev)) * (di ^ 2 / (1 - di ^
                                                                             2))
  return(hadi_influence)
}

#' Validate Inputs and Outputs
#'
#' This function validates the inputs to ensure that the provided data and model are appropriate for calculating influence measures.
#'
#' @param data A dataframe  that contains the data  used to fit the linear model.
#' @param model  An object of class \code{lm} that is the linear model used to calculate the measures of influence (cooks_distance, diffits, hadis_influence).
#'
#' @return The function returns \code{TRUE} if all validations pass. If any validation fails, the function stops execution and returns an error message.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' valInputs(mtcars, model)
valInputs <- function(data, model) {
  if (!inherits(model, "lm")) {
    stop("The argument 'model' must be an object of class 'lm'.")
  }

  if (!is.data.frame(data)) {
    stop("The data must be a data frame.")
  }

  if (any(sapply(data, function(col)
    any(is.na(col)) || any(is.infinite(col))))) {
    stop("The data contains NA or Inf values.")
  }

  if (nrow(data) != nrow(model$model)) {
    stop("The dimensions of the data do not match the number of observations in the model.")
  }

  if (any(duplicated(data))) {
    stop("The data contains duplicated rows.")
  }

  if (any(sapply(data, is.character))) {
    stop("The data contains character variables. Please convert them to factors or numeric.")
  }

  return(TRUE)
}



