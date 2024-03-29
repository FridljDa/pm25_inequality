% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{parametric_bootstrap_sum}
\alias{parametric_bootstrap_sum}
\title{Parametric Bootstrap for Sum of Means with 95\% CI}
\usage{
parametric_bootstrap_sum(df, n_bootstrap = 1000)
}
\arguments{
\item{df}{A data frame with columns: mean, lower, upper}

\item{n_bootstrap}{Number of bootstrap samples to generate}
}
\value{
A data frame with columns: mean, lower, upper representing the 95\% CI for the sum
}
\description{
This function performs parametric bootstrapping to estimate the 95\% confidence
interval for the sum of means based on a data frame containing mean, lower,
and upper 95\% confidence intervals.
}
\examples{
df <- data.frame(mean = c(10, 20, 30), lower = c(9, 19, 29), upper = c(11, 21, 31))
ci_df <- parametric_bootstrap_sum(df, 1000)
}
