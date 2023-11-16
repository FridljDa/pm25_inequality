#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
#rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice"
)

# Load patchwork package
# Load the stringr package
library(stringr)
library(patchwork)
suppressMessages({
  pkgload::load_all()
})

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]
min_ageI <- args[13]

# TODO delete
if (rlang::is_empty(args)) {

  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"

  min_ageI <- 25
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pm_summ <- pm_summ %>% filter(min_age == min_ageI)
rm(file_list)
findreplace <- read.csv("data/final_findreplace.csv")

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----
pm_summ <- pm_summ %>%
  filter(Region == "United States")

pm_summ <- pm_summ %>%
  filter(Gender.Code == "All genders" & Region == "United States" & pm_metric == "mean" & scenario == scenarioI)
pm_summ <- pm_summ %>%
  filter(across(everything(), ~ . != "Unknown")) %>%
  replace_values(findreplace)
filtered_pm_summ <- generate_filtered_dfs(pm_summ)

filtered_pm_summ <- filtered_pm_summ[!grepl("\\*", names(filtered_pm_summ))]
filtered_pm_summ <- filtered_pm_summ[names(filtered_pm_summ) != c("All")]
filtered_pm_summ_names <- names(filtered_pm_summ)

# Define the replacement list
replacement_list <- list(
  SES = "svi_bin1",
  HC = "svi_bin2",
  MS = "svi_bin3",
  HTT = "svi_bin4",
  SVI = "svi_bin",
  Rurality = "rural_urban_class"
)

# Create plots
plots <- lapply(filtered_pm_summ_names, function(filtered_pm_summ_names_i) {
  filtered_pm_summ_i <- filtered_pm_summ[[filtered_pm_summ_names_i]]

  title <- filtered_pm_summ_names_i
  # Loop through the list and replace each value with its corresponding key
  for (key in names(replacement_list)) {
    value <- replacement_list[[key]]
    title <- str_replace_all(title, fixed(value), key)
  }

  #filtered_pm_summ_names_i
  plot_i <- filtered_pm_summ_i %>%
    ggplot(aes(x = Year, y = value, color = !!sym(filtered_pm_summ_names_i))) +
    geom_line(linewidth = 1.5) +
    geom_ribbon(aes(ymin = mean_lower, ymax = mean_upper), linetype = 2, alpha = 0, show.legend = FALSE) +
    xlab("Year") +
    scale_colour_manual(values = get_group_colors(filtered_pm_summ_i), limits = force) +
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
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figureS3.png"),
       dpi = 300, combined_plot, height = 11, width = 8)
