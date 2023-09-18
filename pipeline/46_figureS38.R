#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
# rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice", "ggsci"
)

# Load required packages
library(purrr)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

# Load package with pkgload
suppressMessages({
  pkgload::load_all()
})

# Set some options
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

# Extract arguments
summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]
min_ageI <- args[13]

# TODO delete
# Set default values if arguments are empty
if (rlang::is_empty(args)) {
  scenarioI <- "real"
  methodI <- "di_gee"
  min_ageI <- 25

  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"
}

# List files in summaryDir
file_list <- list.files(summaryDir)

# Filter files with specific names
file_list <- file_list[grepl("attr_bur", file_list)]
file_list <- file_list[grepl("nation", file_list)]

# Create file paths
file_list <- file.path(summaryDir, file_list)

# Read data from file_list into a list and combine into a single data table
attr_burd <- lapply(file_list, read.csv) %>% rbindlist(use.names = TRUE, fill = TRUE)
attr_burd <- attr_burd %>% select(-starts_with("..."))

# Read data from 'all_burd.csv' into a data table
all_burd <- file.path(summaryDir, "all_burd.csv") %>% read.csv() # read_data()

# Filter data tables based on 'min_ageI'
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
all_burd <- all_burd %>% filter(min_age == min_ageI)

# Set plot theme and options
theme_set(theme_classic(base_family = "Helvetica"))
options(bitmapType = "cairo")

# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)

# Filter and prepare data
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI &
    agr_by == "nation" & method == methodI & measure2 == "age-adjusted rate per 100,000")

attr_burd_prop <- attr_burd %>% filter(measure3 == "prop. of overall burden")
attr_burd <- attr_burd %>% filter(measure3 == "value")

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & agr_by == "nation")

### get distinct---
#all_burd <- all_burd %>% distinct()
#attr_burd <- attr_burd %>% distinct()

# Generate filtered data frames
attr_burd_prop_filtered_dfs <- generate_filtered_dfs(attr_burd_prop)
attr_burd_filtered_dfs <- generate_filtered_dfs(attr_burd)
all_burd_filtered_dfs <- generate_filtered_dfs(all_burd)

# Get the names of filtered data frames
attr_burd_filtered_dfs_names <- names(attr_burd_filtered_dfs)

# Filter names to include only those with '*'
attr_burd_filtered_dfs_names <- attr_burd_filtered_dfs_names[grep("\\*", attr_burd_filtered_dfs_names)]

# Create plots
plots <- lapply(attr_burd_filtered_dfs_names, function(attr_burd_filtered_dfs_names_i) {
  attr_burd_i <- attr_burd_filtered_dfs[[attr_burd_filtered_dfs_names_i]]
  all_burd_i <- all_burd_filtered_dfs[[attr_burd_filtered_dfs_names_i]]

  split_color_var <- strsplit(attr_burd_filtered_dfs_names_i, "\\*")
  split_color_var <- unlist(split_color_var)

  # Create the plot
  plot_i <- plot_attr_all_burd(attr_burd_i, all_burd_i, split_color_var[1], split_color_var[2])

  # Save the plot as both PNG and PDF
  ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "attr_all", paste0("figure_", attr_burd_filtered_dfs_names_i, ".png")),
    dpi = 300, plot_i, height = 9, width = 8
  )
  # ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),"attr_all", paste0("figure_", attr_burd_filtered_dfs_names_i, ".pdf")),
  #       dpi = 300, plot_i, height = 9, width = 8)

  return(plot_i)
})

# Get the names of filtered data frames
attr_burd_prop_filtered_dfs_names <- names(attr_burd_prop_filtered_dfs)

# Filter names to include only those with '*'
attr_burd_prop_filtered_dfs_names <- attr_burd_prop_filtered_dfs_names[grep("\\*", attr_burd_prop_filtered_dfs_names)]

plots <- lapply(attr_burd_prop_filtered_dfs_names, function(attr_burd_prop_filtered_dfs_names_i) {
  split_color_var <- strsplit(attr_burd_prop_filtered_dfs_names_i, "\\*")
  split_color_var <- unlist(split_color_var)
  color.column <- split_color_var[[1]]
  split.column <- split_color_var[[2]]

  attr_burd_prop_i <- attr_burd_prop_filtered_dfs[[attr_burd_prop_filtered_dfs_names_i]]
  attr_burd_prop_split_i <- split(attr_burd_prop_i, attr_burd_prop_i[[split.column]])

  plots <- lapply(attr_burd_prop_split_i, function(x) plot_df(x, color.column))
  plots <- update_ylim(plots)

  group.colors <- get_group_colors(attr_burd_prop_i)
  group.colors <- group.colors[names(group.colors) %in% unique(attr_burd_prop_i[[color.column]])]
  legend_plot <- get_legend_custom(group.colors)
  #browser()

  combined_plot <- create_combined_plot(
    plots = plots,
    legend_plot = legend_plot,
    row_annotations = names(plots),
    column_annotations = "Percentage of all-cause mortality attributable to PM2.5",
    y_axis = "%"
  )

  # Save the plot as both PNG and PDF
  ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),
                   "attr_all_propor", paste0("figure_", attr_burd_prop_filtered_dfs_names_i, ".png")),
    dpi = 300, combined_plot, height = 9, width = 7
  )

  return(combined_plot)
})
