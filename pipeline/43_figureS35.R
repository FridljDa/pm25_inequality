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
  "dplyr", "stringr", # "tidyr",
  "gridExtra", "grid", "lattice", "ggsci"
)

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

summaryDir <- "data/17_summary"
figuresDir <- "data/18_figures"

# TODO delete
if (rlang::is_empty(args)) {

  min_ageI <- 25
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = TRUE, fill = TRUE)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica"))
options(bitmapType = "cairo")
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI)

attr_burd <- attr_burd %>%
  filter(agr_by == "nation" & method == methodI & Year >= 2001 & measure3 == "value")

## -- figure 3, attributable burden---
attr_burd1 <- attr_burd %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" & svi_bin == "Resilient SVI")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))

attr_burd2 <- attr_burd %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" & svi_bin == "Moderate SVI")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Ethnicity))

attr_burd3 <- attr_burd %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" & svi_bin == "Vulnerable SVI")
g3 <- ggplot(attr_burd3, aes(x = Year, y = mean, color = Ethnicity))

attr_burd4 <- attr_burd %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All" & svi_bin == "Resilient SVI")
attr_burd4$Education <- factor(attr_burd4$Education, # Relevel group factor
  levels = c(
    "High school graduate or lower",
    "Some college education but no 4-year college degree",
    "4-year college graduate or higher"
  )
)

g4 <- ggplot(attr_burd4, aes(x = Year, y = mean, color = Education))

attr_burd5 <- attr_burd %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All" & svi_bin == "Moderate SVI")
g5 <- ggplot(attr_burd5, aes(x = Year, y = mean, color = Education))

attr_burd6 <- attr_burd %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All" & svi_bin == "Vulnerable SVI")
g6 <- ggplot(attr_burd6, aes(x = Year, y = mean, color = Education))

## --set range---
min_range <- min(c(attr_burd1$lower, attr_burd2$lower, attr_burd3$lower, attr_burd4$lower, attr_burd5$lower, attr_burd6$lower))
max_range <- max(c(attr_burd1$upper, attr_burd2$upper, attr_burd3$upper, attr_burd4$upper, attr_burd5$upper, attr_burd6$upper))

g1 <- g1 + ylim(min_range, max_range)
g2 <- g2 + ylim(min_range, max_range)
g3 <- g3 + ylim(min_range, max_range)
g4 <- g4 + ylim(min_range, max_range)
g5 <- g5 + ylim(min_range, max_range)
g6 <- g6 + ylim(min_range, max_range)

# g6 <- g6 + scale_y_continuous(breaks= pretty_breaks())

plots <- list(g1, g2, g3, g4, g5, g6)
rm(min_range, max_range)
rm(
  attr_burd1, attr_burd2, attr_burd3, attr_burd4, attr_burd5, attr_burd6,
  g1, g2, g3, g4, g5, g6
)
#----formatting------
group.colors <- get_group_colors()

plots <- lapply(plots, function(g) {
  g +
    geom_line(linewidth = 1.5) +
    xlab("Year") +
    geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
    scale_colour_manual(
      values = group.colors,
      limits = force
    ) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE))
})

legend_plots <- lapply(plots[c(1, 4)], function(g) {
  g %>%
    get_legend() %>%
    as_ggplot()
})

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank())
})
## ----get legends ---

toy_data <- tibble(
  x = seq_along(group.colors),
  labels = names(group.colors)
)
toy_data$labels <- factor(toy_data$labels, levels = names(group.colors))
toy_plot <- ggplot(toy_data, aes(x, x, color = labels)) +
  geom_point() +
  scale_color_manual(values = group.colors) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE))
legend_plot <- get_legend(toy_plot)
legend_plot <- as_ggplot(legend_plot)
rm(toy_data, toy_plot)

## --- arrange plots----
lay <- rbind(
  c(NA, NA, 13, NA, 14),
  c(10, 7, 1, NA, 4),
  c(NA, 7, NA, NA, NA),
  c(11, 7, 2, NA, 5),
  c(NA, 7, NA, NA, NA),
  c(12, 7, 3, NA, 6),
  c(NA, NA, 9, NA, 15)
)

t1 <- textGrob("Age-adjusted mortality rate per 100,000", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Resilient SVI", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)
t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Moderate SVI", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)
t5 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Vulnerable SVI", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)

t6 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Race-Ethnicity", gp = gpar(fontsize = 10, fontface = "bold"))
)
t7 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"))
)

gs <- append(plots, list(t1, legend_plots[[1]], t3, t4, t5, t6, t7, legend_plots[[2]]))
# gs <- lapply(1:14, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.1, 0.1, figure_width, blank_space, figure_width),
  heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.9),
  layout_matrix = lay
)

g_combined
as_ggplot(g_combined)
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figureS7_svi.png"), dpi = 300, g_combined, height = 9, width = 8)
