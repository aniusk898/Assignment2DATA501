#' Plot Influence Measures for a Linear Model
#'
#' This function plots various influence measures (Cook's Distance, DFFITS, Hadi's Influence Measure) for a given linear model.
#'
#' @param model An object of class \code{lm} that represents the linear model used to calculate the measures of influence.
#' @param data A dataframe that contains the data used to fit the linear model.
#' @param measure A character string specifying the measure of influence to be plotted. Options are \code{"cooks_distance"}, \code{"diffits"}, or \code{"hadis_influence"}. If not specified, \code{"cooks_distance"} will be used as the default.
#' @param threshold A numeric value specifying the threshold above which an observation is considered influential. If \code{NULL}, the function uses the default threshold based on the selected measure of influence.
#'
#' @return A ggplot object displaying the influence measures with outliers highlighted. The plot includes vertical lines representing the influence values for each observation and a horizontal line indicating the threshold.
#' @import ggplot2
#' @export
#'
#' @examples
#' #' data <- data.frame(
#' y <- c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
#' x1 <- c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
#' )
#' model <- lm(y ~ x1, data <- data)
#' plot_infmeasures(model, mtcars, measure = "cooks_distance")
#' plot_infmeasures(model, mtcars, measure = "diffits", threshold = 0.5)
#' plot_infmeasures(model, mtcars, measure = "hadis_influence")
plot_infmeasures <- function(model,
                             data,
                             measure = "cooks",
                             threshold = NULL) {
  valInputs(data, model)

  n <- nrow(data)
  p <- length(model$coefficients)

  infl <- switch(
    measure,
    cooks = {
      if (is.null(threshold))
        threshold <- 4 / n
      cooks_distance(model)
    },
    diffits = {
      if (is.null(threshold))
        threshold <- 2 * sqrt(p / n)
      diffits(model)
    },
    hadis_influences = {
      if (is.null(threshold))
        threshold <- 2 * sqrt(p / n)
      hadis_influence(model)
    },
    stop("not valid measure")
  )

  outliers <- if (measure == "diffits")
    abs(infl) > threshold
  else
    infl > threshold

  p <- ggplot() +
    geom_segment(aes(
      x = 1:length(infl),
      xend = 1:length(infl),
      y = 0,
      yend = infl,
      color = outliers
    ),
    size = 1) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    geom_hline(yintercept = threshold,
               color = "red",
               size = 0.3) +
    geom_text(
      aes(
        x = 1:length(infl),
        y = infl,
        label = ifelse(outliers, 1:length(infl), "")
      ),
      vjust = -0.5,
      color = "black"
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste("Threshold =", round(threshold, 3)),
      hjust = 1.1,
      vjust = 2,
      color = "red",
      size = 4
    ) +
    labs(
      title = paste("Measure of Influence:", measure),
      x = "Observation",
      y = measure) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  if (measure == "diffits") {
    p <- p + geom_hline(yintercept = -threshold, color = "red", size = 0.3)
  }

  print(p)
}
