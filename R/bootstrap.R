#' Calculate Weighted Mean and Confidence Intervals
#'
#' This function calculates the weighted mean and its confidence intervals.
#'
#' @param pm A numeric vector representing the values for which the weighted mean is calculated.
#' @param pop_size A numeric vector representing the weights for the weighted mean.
#' @param conf_level The confidence level for the interval. Default is 0.95.
#' @return A list containing the weighted mean and its lower and upper confidence intervals.
#' @importFrom boot boot
#' @examples
#' \dontrun{
#' pm <- c(1, 2, 3, 4)
#' pop_size <- c(10, 20, 30, 40)
#' result <- calculate_weighted_mean_ci(pm, pop_size)
#' }
calculate_weighted_mean_ci <- function(pm, pop_size, conf_level = 0.95, R = 1000) {
  # Load required package
  library(boot)

  # Create a data frame from the vectors
  data <- data.frame(pm, pop_size)

  # Define the statistic function for bootstrapping
  weighted_mean_stat <- function(data, indices) {
    d <- data[indices,]
    return(weighted.mean(d$pm, d$pop_size))
  }

  # Perform bootstrapping
  boot_obj <- boot(data = data, statistic = weighted_mean_stat, R = R)

  # Calculate confidence intervals
  ci <- boot.ci(boot.out = boot_obj, conf = conf_level, type = "norm")
  lower <- ci$normal[2]
  upper <- ci$normal[3]

  # Calculate weighted mean
  pop_weight_pm_exp <- weighted.mean(pm, pop_size)

  return(list(pop_weight_pm_exp = pop_weight_pm_exp, lower = lower, upper = upper))
}

#---test---
# Load required packages
library(dplyr)
library(boot)

# Define the calculate_weighted_mean_ci function
calculate_weighted_mean_ci <- function(pm, pop_size, conf_level = 0.95) {
  data <- data.frame(pm, pop_size)
  weighted_mean_stat <- function(data, indices) {
    d <- data[indices,]
    return(weighted.mean(d$pm, d$pop_size))
  }
  boot_obj <- boot(data = data, statistic = weighted_mean_stat, R = 1000)
  ci <- boot.ci(boot.out = boot_obj, conf = conf_level, type = "norm")
  lower <- ci$normal[4]
  upper <- ci$normal[5]
  pop_weight_pm_exp <- weighted.mean(pm, pop_size)
  return(list(pop_weight_pm_exp = pop_weight_pm_exp, lower = lower, upper = upper))
}

# Generate some example data
set.seed(123)
n <- 100
paf_di <- data.frame(
  group = sample(c("A", "B", "C"), n, replace = TRUE),
  pm = runif(n, 0, 100),
  pop_size = runif(n, 1, 50)
)

# Use dplyr to group and calculate weighted mean and confidence intervals
result <- paf_di %>%
  dplyr::group_by_at(vars(one_of(setdiff(colnames(paf_di), c("pm", "pop_size"))))) %>%
  do({
    pm = .$pm
    pop_size = .$pop_size
    result = calculate_weighted_mean_ci(pm, pop_size)
    data.frame(pop_weight_pm_exp = result$pop_weight_pm_exp, lower = result$lower, upper = result$upper)
  }) %>%
  ungroup()

# Show the result
print(result)

