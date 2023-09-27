#' Get Group Colors
#'
#' This function returns a named vector of colors for various demographic and social groups.
#' If a data frame is provided, the function will filter the colors to only include groups present in the data frame.
#'
#' @param df A data frame containing the groups. Default is NULL.
#' @return A named vector of colors for the groups.
#' @examples
#' get_group_colors()
#' get_group_colors(df = data.frame(Column1 = c("NH White", "Hispanic or Latino White"), Column2 = c("Black American", "White")))
get_group_colors <- function(df = NULL) {
  group.colors <- c(
    RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6, 8:10, 12)],
    RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3],
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:3]
  )

  group.colors[c(12, 2)] <- group.colors[c(2, 12)]
  names(group.colors) <- c(
    "NH White",
    "Hispanic or Latino White",
    "Black American",
    "White",
    "Asian or Pacific Islander",
    "American Indian or Alaska Native",
    "High school graduate or lower",
    "Some college education but no 4-year college degree",
    "4-year college graduate or higher",
    "Non metro",
    "Large metro",
    "Small-medium metro",
    "Resilient SVI",
    "Moderate SVI",
    "Vulnerable SVI",

    "High SES",
    "Middle SES",
    "Low SES",

    "High HC",
    "Middle HC",
    "Low HC",

    "High MS",
    "Middle MS",
    "Low MS",

    "High HTT",
    "Middle HTT",
    "Low HTT")

  #if (!is.null(df)) {
  #  browser()
    # Search through all columns to find existing groups
  #  existing_groups <- unique(unlist(df))
  #  group.colors <- group.colors[names(group.colors) %in% existing_groups]
  #}

  return(group.colors)
}


get_legend_custom <- function(group.colors, ncol = 3){
  # https://stackoverflow.com/questions/27803710/ggplot2-divide-legend-into-two-columns-each-with-its-own-title
  toy_data <- tibble(
    x = seq_along(group.colors),
    labels = names(group.colors)
  )

  toy_data$labels <- factor(toy_data$labels, levels = names(group.colors))
  toy_plot <- ggplot(toy_data, aes(x, x, color = labels)) +
    geom_point() +
    scale_color_manual(values = group.colors) +
    theme(legend.title = element_blank()) +
    guides(color = guide_legend(ncol = ncol, byrow = TRUE))
  legend_plot <- get_legend(toy_plot)
  legend_plot <- as_ggplot(legend_plot)
  return(legend_plot)
}

#' Plot data frame using ggplot2
#'
#' @param df Data frame containing the data to plot.
#' @param color.column The column in df that will be used to color the lines.
#' @param group.colors A vector of colors to use for the different groups.
#' @param remove_legend Logical value to indicate whether to remove the legend or not.
#' @param remove_y_titel Logical value to indicate whether to remove the y-axis title or not.
#' @return A ggplot object.
#' @examples
#' plot_df(data.frame(Year = c(2000, 2001), mean = c(1, 2), color = c("red", "blue")))
plot_df <- function(df, color.column, group.colors = get_group_colors(df),
                    remove_legend = TRUE, remove_y_titel = TRUE, title = NA) {
  # Check that all required columns are present
  if (!all(c("Year", color.column) %in% colnames(df))) {
    stop("Required columns (Year, mean, and color column) are not present in the data frame.")
  }
  df <- df %>% distinct()

  # Check for multiple values for x, y, and color
  check_df <- df %>% group_by(Year, !!sym(color.column)) %>% summarise(n = n())
  if(any(check_df$n > 1)) {

    # Check for multiple unique values in all columns except "Year" and color.column
    excluded_columns <- c("Year", color.column, "lower", "upper", "mean", "value")
    other_columns <- setdiff(colnames(df), excluded_columns)

    for (col in other_columns) {
      unique_values <- df %>%
        select(!!sym(col)) %>%
        unlist %>%
        unique
        #group_by(Year, !!sym(color.column)) %>%
        #summarise(unique_vals = list(unique(!!sym(col)))) %>%
        #ungroup()

      #problematic_rows <- unique_values %>% filter(length(unique_vals) > 1)

      if (length(unique_values) > 1) {
        print(glue::glue("Column '{col}' has multiple unique values: {unique_values}"))
        #print(unique(unlist(unique_values$unique_vals)))
        #print("problematic multiple values:")
        #print(unique_values)
      }
    }

    stop("Multiple values for x, y, and color are not allowed.")
  }

  # Add ribbon if lower and upper columns are present
  if ("lower" %in% colnames(df)) {
    g <- ggplot(df, aes(x = Year, y = mean, color = !!sym(color.column)))
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)
  } else if ("value" %in% colnames(df)) {
    g <- ggplot(df, aes(x = Year, y = value, color = !!sym(color.column)))
  }

  # Add line and other elements
  g <- g +
    geom_line(linewidth = 1.5) +
    xlab("Year") +
    scale_colour_manual(values = group.colors) +
    ylim(0, NA)

  if(remove_legend){
    g <- g + theme(legend.title = element_blank(), legend.position = "none")
  } else {
    g <- g + theme(legend.position = "bottom")
  }
  if(remove_y_titel){
    g <- g +
      theme(axis.title.y = element_blank())
  }

  if(!is.na(title)){
    g <- g +
      theme(title = title)
  }

  return(g)
}


#' Create Combined Plot
#'
#' This function combines multiple plots and additional elements like text and legends into a single plot.
#'
#' @param plots A list of ggplot objects to be combined.
#' @param legend_plot A ggplot object for the legend.
#' @param row_annotations A list of strings for row annotations.
#' @param column_annotations A list of strings for column annotations.
#' @param y_axis A string for the y-axis label.
#' @return A combined plot.
#' @examples
#' # Assuming plots, legend_plot, row_annotations, and column_annotations are defined
#' create_combined_plot(plots, legend_plot, row_annotations, column_annotations, y_axis = "Age-adjusted mortality per 100,000")
create_combined_plot <- function(plots, legend_plot, row_annotations, column_annotations, y_axis = "Age-adjusted mortality per 100,000") {
  require(gridExtra)
  require(grid)
  blank_space <- 0.05
  figure_width <- 1.3
  figure_height <- 1

  # Check that length(legend_plot) = n_row * n_row
  n_row <- length(row_annotations)
  n_col <- length(column_annotations)
  if (length(plots) != n_row * n_col) {
    stop("Error: The length of plots must be equal to n_row * n_col")
  }

  # Update y-axis limits for each set of ggplot objects in the same column
  for (i in seq_len(n_col)) {
    start_idx <- (i - 1) * n_row + 1
    end_idx <- i * n_row
    plots[start_idx:end_idx] <- update_ylim(plots[start_idx:end_idx])
  }

  if(TRUE){
    total_rows <- 2 * n_row + 1 # Each plot row is separated by a blank space row, plus one row for col_text_grobs
    total_cols <- 2 * n_col + 1
    # Initialize matrix with NA
    lay <- matrix(NA, nrow = total_rows, ncol = total_cols)
    # Fill in matrix with indices for plots
    counter <- 1

    for (c in seq(3, total_cols, by = 2)) {
      for (r in seq(2, total_rows, by = 2)) {
        #print(c+r)
        #print(c)
        lay[r, c] <- counter
        counter <- counter + 1
      }
    }
    # Add for y title
    lay[2:(total_rows-1), 2] <- counter
    counter <- counter + 1

    # Add index for legend
    lay[total_rows, 3:total_cols] <- counter
    counter <- counter + 1

    # Add indices for row_text_grobs and col_text_grobs
    lay[seq(2, total_rows, by = 2), 1] <- counter:(counter + n_row - 1)
    counter <- counter + n_row

    lay[1, seq(3, total_cols, by = 2)] <- counter:(counter + n_col - 1)
    counter <- counter + n_col
  }

  #lay <- rbind(
  #  c(NA, NA, 13, NA, 14),
  #  c(10, 7, 1, NA, 4),
  #  c(NA, 7, NA, NA, NA),
  #  c(11, 7, 2, NA, 5),
  #  c(NA, 7, NA, NA, NA),
  #  c(12, 7, 3, NA, 6),
  #  c(NA, NA, 9, 9, 9)
  #)

  t1 <- textGrob(y_axis, rot = 90, gp = gpar(fontsize = 10), vjust = 1)
  # Create textGrobs for row and column annotations
  row_text_grobs <- lapply(row_annotations, function(text) {
    text <- insert_line_break(text, n = 15)
    grobTree(
      rectGrob(gp = gpar(fill = "grey")),
      textGrob(text, rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
    )
  })
  col_text_grobs <- lapply(column_annotations, function(text) {
    text <- insert_line_break(text, n = 15)
    grobTree(
      rectGrob(gp = gpar(fill = "grey")),
      textGrob(text, gp = gpar(fontsize = 10, fontface = "bold"))
    )
  })
  # Combine all grobs
  all_grobs <- c(plots, list(t1), list(legend_plot), row_text_grobs, col_text_grobs)

  #g_combined <- grid.arrange(
  #  grobs = all_grobs,
  #  widths = c(0.25, 0.1, figure_width, blank_space, figure_width),
  #  heights = c(0.2, figure_height, blank_space, figure_height, blank_space, figure_height, 0.6),
  #  layout_matrix = lay
  #)
  widths = c(0.25, 0.1, figure_width, rep(c(blank_space, figure_width), times = n_col - 1))
  heights = c(0.2, figure_height, rep(c(blank_space, figure_height), times = n_row - 1), 0.6)

  g_combined <- grid.arrange(
    grobs = all_grobs,
    widths = widths,
    heights = heights,
    layout_matrix = lay
  )

  g_combined <- as_ggplot(g_combined)
  return(g_combined)
}

#' Update y-axis limits for a list of ggplot objects
#'
#' This function takes a list of ggplot objects and sets the y-axis limits
#' to be the same for all plots, based on the maximum value in either the 'upper', 'value', or a specified column.
#' Throws an error if neither column is present.
#'
#' @param ggplot_list A list of ggplot objects.
#' @param column The name of the column to use for setting the y-axis limits. Defaults to NULL.
#' @return A list of ggplot objects with updated y-axis limits.
#' @examples
#' g1 <- ggplot(data.frame(x = 1:10, upper = rnorm(10)), aes(x, upper)) + geom_line()
#' g2 <- ggplot(data.frame(x = 1:10, value = rnorm(10)), aes(x, value)) + geom_line()
#' g3 <- ggplot(data.frame(x = 1:10, upper = rnorm(10)), aes(x, upper)) + geom_line()
#' ggplot_list <- list(g1, g2, g3)
#' updated_ggplot_list <- update_ylim(ggplot_list)
update_ylim <- function(ggplot_list, column = NULL) {
  # Initialize an empty vector to store maximum values
  max_values <- c()

  # Loop through each ggplot object to find the maximum value
  for (g in ggplot_list) {
    if (!is.null(column) && column %in% names(g$data)) {
      max_values <- c(max_values, max(g$data[[column]], na.rm = TRUE))
    } else if ("upper" %in% names(g$data)) {
      max_values <- c(max_values, max(g$data$upper, na.rm = TRUE))
    } else if ("value" %in% names(g$data)) {
      max_values <- c(max_values, max(g$data$value, na.rm = TRUE))
    } else {
      stop("Error: Neither 'upper' nor 'value' column found in ggplot data.")
    }
  }

  # Calculate the overall maximum value
  max1 <- max(max_values, na.rm = TRUE)

  suppressMessages({
    # Update y-axis limits for each ggplot
    updated_ggplot_list <- lapply(ggplot_list, function(g) g + ylim(0, max1))
  })

  return(updated_ggplot_list)
}


#' Plot Combined Data for attr_burd and all_burd
#'
#' This function takes two data frames, attr_burd and all_burd, along with the names of
#' the columns to be used for coloring and splitting. It then generates a combined plot.
#'
#' @param attr_burd Data frame containing the attr_burd data.
#' @param all_burd Data frame containing the all_burd data.
#' @param color.column The name of the column to be used for coloring the plots.
#' @param split.column The name of the column to be used for splitting the data frames.
#'
#' @return A combined ggplot object.
#' @examples
#' \dontrun{
#' result <- plot_attr_all_burd(attr_burd, all_burd, "Ethnicity", "svi_bin1")
#' }
#' @export
plot_attr_all_burd <- function(attr_burd, all_burd, color.column, split.column) {
  library(dplyr)
  library(purrr)

  findreplace <- read.csv("data/final_findreplace.csv")
  all_burd <- all_burd %>% replace_values(findreplace)
  attr_burd <- attr_burd %>% replace_values(findreplace)

  # Drop empty factor levels
  attr_burd <- attr_burd %>% dplyr::mutate(across(where(is.factor), droplevels))
  all_burd <- all_burd %>% dplyr::mutate(across(where(is.factor), droplevels))

  # Get unique split values
  unique_split_values <- unique(attr_burd[[split.column]])

  # Split data frames based on unique values
  attr_burd_split <- map(unique_split_values, ~ filter(attr_burd, !!sym(split.column) == .x))
  all_burd_split <- map(unique_split_values, ~ filter(all_burd, !!sym(split.column) == .x))

  # Combine the two lists
  attr_all_burd_split <- append(attr_burd_split, all_burd_split)

  # Generate plots
  plots <- lapply(attr_all_burd_split, function(x) plot_df(x, color.column))

  # Formatting (assuming get_group_colors and get_legend_custom are defined)
  group.colors <- get_group_colors(attr_burd)
  group.colors <- group.colors[names(group.colors) %in% unique(attr_burd[[color.column]])]
  legend_plot <- get_legend_custom(group.colors)

  # Arrange plots (assuming create_combined_plot is defined)
  g_combined <- create_combined_plot(plots,
                                     legend_plot,
                                     unique_split_values,
                                     c("PM2.5-attributable mortality", "All-cause mortality"),
                                     y_axis = "Age-adjusted mortality per 100,000")

  return(g_combined)
}

#' Insert a line break at an appropriate space in a string
#'
#' This function takes a string and an integer n as input.
#' If the string has length n or more, it inserts a "\n" at a space character,
#' ideally as centrally as possible within the string.
#'
#' @param string Character vector containing the string to be modified.
#' @param n Integer specifying the length threshold for line break insertion.
#'
#' @return A modified string with a line break at an appropriate space if the original string's length is n or more.
#' @examples
#' insert_line_break("alle meine Entchen schwimmen auf dem See", 20)
#' insert_line_break("short string", 20)
insert_line_break <- function(string, n = 20) {
  str_len <- nchar(string)

  if (str_len < n) {
    return(string)
  }

  # Find all the positions of space characters in the string
  space_positions <- gregexpr(" ", string)[[1]]

  # Find the space that is closest to the middle of the string
  central_space <- space_positions[which.min(abs(space_positions - str_len / 2))]

  # Insert the line break at the central space position
  modified_string <- paste0(substr(string, 1, central_space - 1), "\n ", substr(string, central_space + 1, str_len))

  return(modified_string)
}

