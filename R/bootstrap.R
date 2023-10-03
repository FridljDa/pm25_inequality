#' Calculate Weighted Mean and Confidence Intervals
#'
#' This function calculates the weighted mean and its confidence intervals.
#'
#' @param pm A numeric vector representing the values for which the weighted mean is calculated.
#' @param pop_size A numeric vector representing the weights for the weighted mean.
#' @param conf_level The confidence level for the interval. Default is 0.95.
#' @param R The number of bootstrap replicates. Default is 10.
#' @return A list containing the weighted mean and its lower and upper confidence intervals.
#' @importFrom boot boot
#' @examples
#' \dontrun{
#' pm <- c(1, 2, 3, 4)
#' pop_size <- c(10, 20, 30, 40)
#' result <- calculate_weighted_mean_ci(pm, pop_size)
#' }
calculate_weighted_mean_ci <- function(pm, pop_size, conf_level = 0.95, R = 10) {

  # Create a data frame from the vectors
  data <- data.frame(pm, pop_size)

  # Calculate weighted mean
  pop_weight_pm_exp <- weighted.mean(pm, pop_size)

  # Check if pm has no variation or all bootstrap resampled means are identical
  if (length(unique(pm)) == 1 | length(unique(pop_size)) == 1) {
    lower <- upper <- pop_weight_pm_exp
  } else {
    # Define the statistic function for bootstrapping
    weighted_mean_stat <- function(data, indices) {
      d <- data[indices,]
      return(weighted.mean(d$pm, d$pop_size))
    }

    # Perform bootstrapping
    boot_obj <- boot::boot(data = data, statistic = weighted_mean_stat, R = R)

    # Check if all bootstrap resampled means are identical
    if (length(unique(boot_obj$t)) == 1) {
      lower <- upper <- pop_weight_pm_exp
    } else {
      # Calculate confidence intervals
      ci <- boot::boot.ci(boot.out = boot_obj, conf = conf_level, type = "norm")
      lower <- ci$normal[2]
      upper <- ci$normal[3]
    }
  }

  return(list(pop_weight_pm_exp = pop_weight_pm_exp, lower = lower, upper = upper))
}



#' Estimate the Mean and Confidence Interval for the Product of Two Random Variables Using the Delta Method
#'
#' @param mean_x Mean of the first random variable X
#' @param lb_x Lower bound of the confidence interval for X
#' @param ub_x Upper bound of the confidence interval for X
#' @param mean_y Mean of the second random variable Y
#' @param lb_y Lower bound of the confidence interval for Y
#' @param ub_y Upper bound of the confidence interval for Y
#' @param alpha Significance level for the confidence interval (default is 0.05 for a 95% CI)
#'
#' @return A list containing the estimated mean, lower bound, and upper bound of the confidence interval for Z = X * Y
#' @export
#' @examples
#' # Example: Estimate the mean and 95% CI for the product of X and Y
#' # where X has a mean of 10 and 95% CI [8, 12]
#' # and Y has a mean of 5 and 95% CI [4, 6]
#' result <- delta_method_product(mean_x = 10, lb_x = 8, ub_x = 12, mean_y = 5, lb_y = 4, ub_y = 6)
#' print(paste("Estimated mean of Z:", result$mean))
#' print(paste("Lower bound of 95% CI for Z:", result$lb))
#' print(paste("Upper bound of 95% CI for Z:", result$ub))
delta_method_product <- function(mean_x, lb_x, ub_x, mean_y, lb_y, ub_y, alpha = 0.05) {
  # Calculate standard deviations from confidence intervals
  z_alpha = qnorm(1 - alpha / 2)
  sd_x = (ub_x - lb_x) / (2 * z_alpha)
  sd_y = (ub_y - lb_y) / (2 * z_alpha)

  # Estimate the mean of Z
  mean_z = mean_x * mean_y

  # Estimate the variance of Z using the Delta Method
  var_z = (mean_y * sd_x)^2 + (mean_x * sd_y)^2

  # Estimate the standard deviation of Z
  sd_z = sqrt(var_z)

  # Calculate the confidence interval for Z
  lb_z = mean_z - z_alpha * sd_z
  ub_z = mean_z + z_alpha * sd_z

  return(list(mean = mean_z, lb = lb_z, ub = ub_z))
}

#' Estimate the Mean and Confidence Interval for the Quotient of Two Random Variables Using the Delta Method
#'
#' @param mean_x Mean of the first random variable X
#' @param lb_x Lower bound of the confidence interval for X
#' @param ub_x Upper bound of the confidence interval for X
#' @param mean_y Mean of the second random variable Y
#' @param lb_y Lower bound of the confidence interval for Y
#' @param ub_y Upper bound of the confidence interval for Y
#' @param alpha Significance level for the confidence interval (default is 0.05 for a 95% CI)
#'
#' @return A list containing the estimated mean, lower bound, and upper bound of the confidence interval for Z = X / Y
#' @export
#' @examples
#' # Example: Estimate the mean and 95% CI for the quotient of X and Y
#' # where X has a mean of 10 and 95% CI [8, 12]
#' # and Y has a mean of 5 and 95% CI [4, 6]
#' result <- delta_method_quotient(mean_x = 10, lb_x = 8, ub_x = 12, mean_y = 5, lb_y = 4, ub_y = 6)
#' print(paste("Estimated mean of Z:", result$mean))
#' print(paste("Lower bound of 95% CI for Z:", result$lb))
#' print(paste("Upper bound of 95% CI for Z:", result$ub))
delta_method_quotient <- function(mean_x, lb_x, ub_x, mean_y, lb_y, ub_y, alpha = 0.05) {
  # Calculate standard deviations from confidence intervals
  z_alpha = qnorm(1 - alpha / 2)
  sd_x = (ub_x - lb_x) / (2 * z_alpha)
  sd_y = (ub_y - lb_y) / (2 * z_alpha)

  # Estimate the mean of Z
  mean_z = mean_x / mean_y

  # Estimate the variance of Z using the Delta Method
  var_z = ((sd_x / mean_y)^2) + ((mean_x * sd_y / (mean_y^2))^2)

  # Estimate the standard deviation of Z
  sd_z = sqrt(var_z)

  # Calculate the confidence interval for Z
  lb_z = mean_z - z_alpha * sd_z
  ub_z = mean_z + z_alpha * sd_z

  return(list(mean = mean_z, lb = lb_z, ub = ub_z))
}


#' Estimate the Mean and Confidence Interval for the Sum of Two Random Variables Using the Delta Method
#'
#' @param mean_x Mean of the first random variable X
#' @param lb_x Lower bound of the confidence interval for X
#' @param ub_x Upper bound of the confidence interval for X
#' @param mean_y Mean of the second random variable Y
#' @param lb_y Lower bound of the confidence interval for Y
#' @param ub_y Upper bound of the confidence interval for Y
#' @param alpha Significance level for the confidence interval (default is 0.05 for a 95% CI)
#'
#' @return A list containing the estimated mean, lower bound, and upper bound of the confidence interval for Z = X + Y
#' @export
#' @examples
#' # Example: Estimate the mean and 95% CI for the sum of X and Y
#' # where X has a mean of 10 and 95% CI [8, 12]
#' # and Y has a mean of 5 and 95% CI [4, 6]
#' result <- delta_method_sum(mean_x = 10, lb_x = 8, ub_x = 12, mean_y = 5, lb_y = 4, ub_y = 6)
#' print(paste("Estimated mean of Z:", result$mean))
#' print(paste("Lower bound of 95% CI for Z:", result$lb))
#' print(paste("Upper bound of 95% CI for Z:", result$ub))
delta_method_sum <- function(mean_x, lb_x, ub_x, mean_y, lb_y, ub_y, alpha = 0.05) {
  # Calculate standard deviations from confidence intervals
  z_alpha = qnorm(1 - alpha / 2)
  sd_x = (ub_x - lb_x) / (2 * z_alpha)
  sd_y = (ub_y - lb_y) / (2 * z_alpha)

  # Estimate the mean of Z
  mean_z = mean_x + mean_y

  # Estimate the variance of Z using the Delta Method
  var_z = sd_x^2 + sd_y^2

  # Estimate the standard deviation of Z
  sd_z = sqrt(var_z)

  # Calculate the confidence interval for Z
  lb_z = mean_z - z_alpha * sd_z
  ub_z = mean_z + z_alpha * sd_z

  return(list(mean = mean_z, lb = lb_z, ub = ub_z))
}

#' Calculate Mean and 95% Confidence Interval
#'
#' This function takes a numeric vector and calculates the mean, lower, and upper
#' estimates of the 95% confidence interval for the mean.
#'
#' @param x A numeric vector for which the mean and 95% CI are to be calculated.
#'
#' @return A data frame containing the mean, lower, and upper estimates of the 95% CI.
#'
#' @examples
#' calculate_mean_and_ci(c(1, 2, 3, 4, 5))
calculate_mean_and_ci <- function(x) {
  # Check if the input is a numeric vector
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }

  # Calculate the mean
  mean_val <- mean(x)

  # Calculate the standard deviation
  sd_val <- sd(x)

  # Calculate the sample size (n)
  n <- length(x)

  # Calculate the standard error of the mean
  se_val <- sd_val / sqrt(n)

  # Calculate the Z-value for a 95% confidence interval
  z_value <- 1.96

  # Calculate the margin of error
  margin_of_error <- z_value * se_val

  # Calculate the lower and upper bounds of the 95% confidence interval
  lower_bound <- mean_val - margin_of_error
  upper_bound <- mean_val + margin_of_error

  # Create a data frame to store these values
  confidence_interval_df <- data.frame(
    mean = mean_val,
    lower_95 = lower_bound,
    upper_95 = upper_bound
  )

  return(confidence_interval_df)
}

