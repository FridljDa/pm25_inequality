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
library(patchwork)
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
attr_burd <- lapply(file_list, read_data) %>% rbindlist(use.names = TRUE, fill = TRUE)

# Filter data tables based on 'min_ageI'
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
attr_burd <- attr_burd %>% filter(Year > 1990)

# Set plot theme and options
theme_set(theme_classic(base_family = "Helvetica"))
#options(bitmapType = "cairo")

# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)

# Filter and prepare data
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable"  &
           source == "National Vital Statistics System" & scenario == scenarioI &
           agr_by == "nation" & method == methodI & measure3 == "value")


# ------ Define a custom theme-----
custom_theme <- theme(
  text = element_text(family = "Helvetica", size = 7),  # Set default text properties
  plot.title = element_text(size = 7),  # Set title text size
  axis.title = element_text(size = 6),  # Set axis title text size
  axis.text = element_text(size = 5),   # Set axis text size
  legend.title = element_text(size = 6),  # Set legend title text size
  legend.text = element_text(size = 5)    # Set legend text size
)

# Apply the custom theme globally
theme_set(theme_classic(base_family = "Helvetica") + custom_theme)
# ------ -----

# Generate filtered data frames
attr_burd_filtered_dfs <- generate_filtered_dfs(attr_burd)
attr_burd_filtered_dfs_names <- names(attr_burd_filtered_dfs)
attr_burd_filtered_dfs_names <- attr_burd_filtered_dfs_names[-grep("\\*", attr_burd_filtered_dfs_names)]
attr_burd_filtered_dfs_names <- setdiff(attr_burd_filtered_dfs_names, "All")
#attr_burd_filtered_dfs_names <- attr_burd_filtered_dfs_names[-(attr_burd_filtered_dfs_names == "All")]
attr_burd_filtered_dfs <- attr_burd_filtered_dfs[attr_burd_filtered_dfs_names]

replacement_list <- list(
  SES = "svi_bin1",
  HC = "svi_bin2",
  MS = "svi_bin3",
  HTT = "svi_bin4",
  SVI = "svi_bin",
  Rurality = "rural_urban_class"
)

##test---
plots <- lapply(attr_burd_filtered_dfs_names, function(attr_burd_filtered_dfs_names_i) {
  attr_burd_filtered_dfs_i <- attr_burd_filtered_dfs[[attr_burd_filtered_dfs_names_i]]

  title <- attr_burd_filtered_dfs_names_i
  # Loop through the list and replace each value with its corresponding key
  for (key in names(replacement_list)) {
    value <- replacement_list[[key]]
    title <- str_replace_all(title, fixed(value), key)
  }

  #filtered_pm_summ_names_i
  plot_i <- attr_burd_filtered_dfs_i %>%
    ggplot(aes(x = Year, y = mean, color = !!sym(attr_burd_filtered_dfs_names_i))) +
    geom_line(linewidth = 1.5) +
    xlab("Year") +
    scale_colour_manual(values = get_group_colors(attr_burd_filtered_dfs_i), limits = force) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
    ggtitle(title)+
    theme(legend.position = "bottom", axis.title.y = element_blank()) #+
  # scale_y_continuous(
  #    labels = scales::number_format(suffix = expression(" " * mu * "g/m"^3))
  #  )

  return(plot_i)
})

plots <- update_ylim(plots, use_actual_min = TRUE)

# Combine plots
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]  +
  plots[[5]]  + plots[[7]]+ plots[[6]]+#  +
  plot_layout(ncol = 3)

# Show combined plot
combined_plot


# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
#ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figure_attr_burd_subpopulation.pdf"),
#       dpi = 300, combined_plot, height = 11, width = 8)


ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "extended_data_figure1.eps"), #eps
       dpi = 300,
       plot = combined_plot,
       height = 290,
       width = 205,
       units = "mm")
