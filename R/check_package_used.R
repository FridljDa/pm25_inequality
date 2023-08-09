
library(dplyr)

#' Check Package Usage in an R Script
#'
#' This function checks which functions from a given set of packages are used in an R script.
#' It takes the path to the R script and a vector of package names, then returns the packages that are actually used.
#'
#' @param path_to_script A string representing the path to the R script.
#' @param pkgs A character vector containing the names of the packages to check.
#'
#' @return A character vector containing the names of the packages that are used in the script, or NULL if none of the specified packages are used.
#' @examples
#' \dontrun{
#'   path <- "path/to/your/script.R"
#'   packages <- c("dplyr", "ggplot2", "tidyr")
#'   result <- check_package_usage(path, packages)
#'   print(result)
#' }
#' @export
check_package_usage <- function(path_to_script, pkgs) {

  # Helper function to get functions from a package
  get_package_functions <- function(package_name) {
    ls(name = asNamespace(package_name))
  }

  # Read the R script
  script_content <- readLines(path_to_script)

  # Store packages that are used
  packages_used <- c()

  # Iterate over packages
  for (pkg in pkgs) {
    # Load the package functions without attaching it
    pkg_funcs <- get_package_functions(pkg)

    # Check for usage
    used_funcs <- sapply(pkg_funcs, function(func) {
      pattern <- paste0("\\b", func, "\\b")
      any(grepl(pattern, script_content))
    })

    # Check if any functions were used
    if (any(used_funcs)) {
      packages_used <- c(packages_used, pkg)
    }
  }

  # Return the packages used or a message indicating no usage
  if (length(packages_used) > 0) {
    message("The following packages are used in the script:")
    return(packages_used)
  } else {
    message("No functions from the specified packages are used in the script.")
    return(NULL)
  }
}
