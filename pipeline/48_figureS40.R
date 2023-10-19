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
# all_burd <- all_burd %>% distinct()
# attr_burd <- attr_burd %>% distinct()

# Generate filtered data frames
attr_burd_prop_filtered_dfs <- generate_filtered_dfs(attr_burd_prop)
attr_burd_filtered_dfs <- generate_filtered_dfs(attr_burd)
all_burd_filtered_dfs <- generate_filtered_dfs(all_burd)

# Get the names of filtered data frames
attr_burd_filtered_dfs_names <- names(attr_burd_filtered_dfs)

# Filter names to include only those with '*'
attr_burd_filtered_dfs_names <- attr_burd_filtered_dfs_names[grep("\\*", attr_burd_filtered_dfs_names)]

# attr_burd_filtered_dfs_names <- "Ethnicity*rural_urban_class"
pairwise_differences <- lapply(attr_burd_filtered_dfs_names, function(attr_burd_filtered_dfs_names_i) {
  data <- attr_burd_filtered_dfs[[attr_burd_filtered_dfs_names_i]]
  split_color_var <- strsplit(attr_burd_filtered_dfs_names_i, "\\*")
  split_color_var <- unlist(split_color_var)

  # Load dplyr package
  color.column <- split_color_var[[1]]
  split.column <- split_color_var[[2]]

  if (color.column == "Ethnicity") {
    data <- data %>%
      filter(Ethnicity != "Asian or Pacific Islander")
  }

  data <- data %>%
    rename(split.column = !!sym(split.column))
  # Calculate pairwise differences
  pairwise_diff <- data %>%
    full_join(data, by = c(color.column, "Year"), multiple = "all")

  pairwise_diff <- pairwise_diff %>%
    filter(!grepl("Moderate|Vulnerable", split.column.x)) %>%
    filter(!grepl("Moderate|Resilient", split.column.y)) %>%
    filter(!grepl("Middle|High", split.column.y)) %>%
    filter(!grepl("Middle|Low", split.column.x)) %>%
    filter(!grepl("Small-medium|Large", split.column.x)) %>%
    filter(!grepl("Small-medium|Non", split.column.y))

    pairwise_diff <- pairwise_diff %>%
      rowwise() %>%
      mutate(
        diff = mean.x - mean.y,
        rel_diff = (mean.x - mean.y) / mean.x,
        bootstrap_results = list(parametric_bootstrap_rel_diff(cur_data()))
      ) %>%
      unnest_wider(bootstrap_results)


  pairwise_diff <- pairwise_diff %>%
    select(Year, !!sym(color.column), diff,
           rel_diff, rel_diff_mean, rel_diff_lower, rel_diff_upper,
           split.column.x, split.column.y, mean.x, mean.y) %>%
    tidyr::unite("difference_col", split.column.x:split.column.y, sep = "-", remove = FALSE) %>%
    mutate(rel_label = paste0("(", split.column.x, "-", split.column.y, ")/", split.column.x))
  return(pairwise_diff)
})
names(pairwise_differences) <- attr_burd_filtered_dfs_names

pairwise_differences <- rbindlist(pairwise_differences, fill = TRUE)
pairwise_differences_ethn <- pairwise_differences %>%
  filter(!is.na(Ethnicity)) %>%
  filter(!grepl("MS", difference_col))

g_rel_ethn <- pairwise_differences_ethn %>%
  rename(`relative difference in PM2.5-attributable mortality rate` = rel_diff_mean) %>%
  ggplot(aes(x = Year, y = `relative difference in PM2.5-attributable mortality rate`, color = Ethnicity)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = rel_diff_lower, ymax = rel_diff_upper),
    linetype = 2, alpha = 0, show.legend = FALSE
  ) +
  facet_wrap(vars(difference_col)) + # rel_label
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = get_group_colors(pairwise_differences_ethn)) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) # Add dashed line at y = 0

ggsave(
  filename = file.path(
    figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),
    "attr_rel_differences", paste0("figure_ethn_", "combined", ".png")
  ),
  plot = g_rel_ethn,
  dpi = 300, height = 6, width = 8
)

pairwise_differences_educ <- pairwise_differences %>%
  filter(is.na(Ethnicity))

g_rel_educ <- pairwise_differences_educ %>%
  rename(`relative difference in PM2.5-attributable mortality rate` = rel_diff_mean) %>%
  ggplot(aes(x = Year, y = `relative difference in PM2.5-attributable mortality rate`, color = Education)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(vars(difference_col)) + # rel_label
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = get_group_colors(pairwise_differences_educ)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) # Add dashed line at y = 0

ggsave(
  filename = file.path(
    figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),
    "attr_rel_differences", paste0("figure_educ_", "combined", ".png")
  ),
  plot = g_rel_educ,
  dpi = 300, height = 6, width = 8
)

if (FALSE) {
  for (pairwise_diff in pairwise_differences) {
    g_abs <- pairwise_diff %>%
      rename(`absolute difference in PM2.5-attributable mortality rate` = diff) %>%
      ggplot(aes(x = Year, y = `absolute difference in PM2.5-attributable mortality rate`, color = !!sym(color.column))) +
      geom_line(linewidth = 1.5) +
      facet_wrap(vars(difference_col)) +
      theme(legend.position = "bottom")

    ggsave(
      filename = file.path(
        figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),
        "attr_abs_differences", paste0("figure_", attr_burd_filtered_dfs_names_i, ".png")
      ),
      plot = g_abs,
      dpi = 300, height = 8, width = 7
    )

    g_rel <- pairwise_diff %>%
      rename(`relative difference in PM2.5-attributable mortality rate` = rel_diff) %>%
      ggplot(aes(x = Year, y = `relative difference in PM2.5-attributable mortality rate`, color = !!sym(color.column))) +
      geom_line(linewidth = 1.5) +
      facet_wrap(vars(difference_col)) + # rel_label
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = label_percent()) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) # Add dashed line at y = 0

    ggsave(
      filename = file.path(
        figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI),
        "attr_rel_differences", paste0("figure_", attr_burd_filtered_dfs_names_i, ".png")
      ),
      plot = g_rel,
      dpi = 300, height = 8, width = 7
    )
  }
}
