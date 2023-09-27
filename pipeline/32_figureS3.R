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

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----
pm_summ <- pm_summ %>%
  filter(Region == "United States")

pm_summ <- pm_summ %>%
  filter(Gender.Code == "All genders" & Region == "United States" & pm_metric == "mean" & scenario == scenarioI)

## -- figure 3, attributable burden---
pm_summ1 <- pm_summ %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All")

g1 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity))

pm_summ2 <- pm_summ %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")
pm_summ2$Education <- factor(pm_summ2$Education,                 # Relevel group factor
                             levels = c("High school graduate or lower",
                                        "Some college education but no 4-year college degree",
                                        "4-year college graduate or higher"))

g2 <- ggplot(pm_summ2, aes(x = Year, y = value, color = Education))

pm_summ3 <- pm_summ %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
pm_summ3$rural_urban_class <- factor(pm_summ3$rural_urban_class,                 # Relevel group factor
                                       levels = c("Large metro",
                                                  "Small-medium metro",
                                                  "Non metro"))

g3 <- ggplot(pm_summ3, aes(x = Year, y = value, color = rural_urban_class))

pm_summ4 <- pm_summ %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All" & Year >= 2000)


## --set range---
min1 <- min(c(pm_summ1$value, pm_summ2$value, pm_summ3$value))
max1 <- max(c(pm_summ1$value, pm_summ2$value, pm_summ3$value))

g1 <- g1 + ylim(0, max1)
g2 <- g2 + ylim(0, max1)
g3 <- g3 + ylim(0, max1)

plots <- list(g1, g2, g3)
rm(min1, max1)
rm(
  #pm_summ1, pm_summ2, pm_summ3,
  g1, g2, g3
)
#----formatting------
group.colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6, 8:10, 12)],
  RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2]
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
  "Small-medium metro"
)

plots <- lapply(plots, function(g) {
  g +
    geom_line(linewidth = 1.5) +
    xlab("Year") +
    scale_colour_manual(values = group.colors, limits = force) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE))
})

legend_plots <- lapply(plots, function(g) {
  g %>%
    get_legend() %>%
    as_ggplot()
})

plots <- lapply(plots, function(g) {
  g +
    theme(legend.position = "none", axis.title.y = element_blank())
})

## --- arrange plots----
lay <- rbind(
  c(NA, 4,  NA, 5, NA, 6),
  c(NA, NA, NA, NA, NA, NA),
  c(10, 1,  NA, 2, NA, 3),
  c(NA, 7,  NA, 8, NA, 9)
)

t1 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Race-ethnicity", gp = gpar(fontsize = 10, fontface = "bold"))
)

t2 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"))
)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Rurality", gp = gpar(fontsize = 10, fontface = "bold"))
)

t4 <- textGrob(expression(paste("Mean PM2.5 exposure (", mu, g, "/", m^3,")", sep="")),
           gp = gpar(fontsize = 10, fontface = "bold"),
           rot = 90)

gs <- append(plots, list(t1, t2, t3))
gs <- append(gs, legend_plots)
gs <- append(gs, list(t4))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.25, figure_width, blank_space, figure_width, blank_space, figure_width),
  heights = c(0.1, blank_space, figure_hight, 1),
  layout_matrix = lay
)
as_ggplot(g_combined)

# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figureS3.png"), dpi = 300, g_combined, height = 4, width = 8)
