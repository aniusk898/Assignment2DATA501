#' Plot Influence Measures for a Linear Model
#'
#' This function plots various influence measures (Cooks Distance, DFFITS, hadis_influenceInfluence Measure) for a given linear model.
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
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' plot_infmeasures(model, mtcars, measure = "cooks")
#' plot_infmeasures(model, mtcars, measure = "diffits", threshold = 0.5)
#' plot_infmeasures(model, mtcars, measure = "hadis_influence")
plot_infmeasures <- function(model, data, measure = "cooks", threshold = NULL) {
  valInputs(data, model)

  n <- nrow(data)
  p <- length(model$coefficients)

  infl <- switch(measure,
                 "cooks" = {
                   if (is.null(threshold)) threshold <- 4 / n
                   cooks_distance(model)
                 },
                 "diffits" = {
                   if (is.null(threshold)) threshold <- 2 * sqrt(p / n)
                   diffits(model)
                 },
                 "hadis_influence" = {hadis_influence(model)},
                 stop("not valid measure"))

  outliers <- if (measure == "hadis_influence") FALSE else if (measure == "diffits") abs(infl) > threshold else infl > threshold

  p <- ggplot() +
    geom_segment(aes(x = 1:length(infl), xend = 1:length(infl), y = 0, yend = infl, color = outliers), size = 1) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.3) +
    geom_text(aes(x = 1:length(infl), y = infl, label = ifelse(outliers, 1:length(infl), "")), vjust = -0.5, color = "black") +
    labs(title = paste("Measure of Influence:", measure), x = "Observation", y = measure) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  if (!is.null(threshold) && measure != "hadis_influence") {
    p <- p + geom_hline(yintercept = threshold, color = "red", linetype = "dashed", size = 1)
    p <- p + annotate("text", x = Inf, y = Inf, label = paste("Threshold =", round(threshold, 4)), hjust = 1.1, vjust = 2, color = "red", size = 4)
    if (measure == "diffits") {
      p <- p + geom_hline(yintercept = -threshold, color = "red", linetype = "dashed", size = 1)
    }
  }

  print(p)
}

