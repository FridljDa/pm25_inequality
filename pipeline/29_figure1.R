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
#packages <- c(
#  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
#  "dplyr", "stringr", "tidyr",
#  "gridExtra", "grid", "lattice", "ggsci"
#)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(data.table)

suppressMessages({
  pkgload::load_all()
})

#for (p in packages) {
  # if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  # suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
#}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]
min_ageI <- args[13]


if (rlang::is_empty(args)) {
  scenarioI <- "real"
  methodI <- "di_gee"

  min_ageI <- 25
  summaryDir <- "data/17_summary" # 17_summary
  figuresDir <- "data/18_figures"
}
#options(bitmapType = "cairo")
file_list <- list.files(summaryDir)
file_list <- file_list[grepl("attr_bur", file_list)]
file_list <- file_list[grepl("nation", file_list)]
file_list <- file.path(summaryDir, file_list)
attr_burd <- lapply(file_list, read_data) %>% rbindlist(use.names = TRUE, fill = TRUE)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
rm(file_list)

# attr_burd <- read_csv("data/15_sum_higher_geog_level/attributable_burden_age_adj.csv")
# source(paste0("https://raw.githubusercontent.com/mkiang/",
#              "opioid_hotspots/master/code/mk_nytimes.R"))

theme_set(theme_classic(base_family = "Helvetica"))
#options(bitmapType = "cairo")
# source("theme.R")

# theme_set(theme_bw( base_family = "Helvetica"))
dir.create(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI)), recursive = T, showWarnings = F)
#attr_burd <-  attr_burd %>% filter (Year > 1990)

### ----- read stuff----
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All")

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & scenario == scenarioI & method == methodI)

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & method == methodI)

## -- figure 3, attributable burden---
# TODO method di_gee/burnett
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All" #& svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All"
                                   & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "value" & rural_urban_class == "All")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All" & #svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
                                     Education != 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All")
attr_burd2$Education <- factor(attr_burd2$Education, # Relevel group factor
  levels = c(
    "High school graduate or lower",
    "Some college education but no 4-year college degree",
    "4-year college graduate or higher"
  )
)

g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education))


attr_burd3 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All"
                                   & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" &
  rural_urban_class != "All" ) #& Year >= 2000
attr_burd3$rural_urban_class <- factor(attr_burd3$rural_urban_class, # Relevel group factor
  levels = c(
    "Large metro",
    "Small-medium metro",
    "Non metro"
  )
)
g3 <- ggplot(attr_burd3, aes(x = Year, y = mean, color = rural_urban_class))

attr_burd7 <- attr_burd %>% filter(agr_by == "nation" & svi_bin != "All" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" &
  rural_urban_class == "All") # & Year >= 2000
g7 <- ggplot(attr_burd7, aes(x = Year, y = mean, color = svi_bin))

#attr_burd4 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All" #& svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All"
#                                   & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "prop. of overall burden" & rural_urban_class == "All")
#g4 <- ggplot(attr_burd4, aes(x = Year, y = mean, color = Ethnicity))

#attr_burd5 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All" & #svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
#                                     Education != 666 & Ethnicity == "All, All Origins" & measure3 == "prop. of overall burden" & rural_urban_class == "All")
#g5 <- ggplot(attr_burd5, aes(x = Year, y = mean, color = Education))

#attr_burd6 <- attr_burd %>% filter(agr_by == "nation" & svi_bin == "All" &# svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
#                                     Education == 666 & Ethnicity == "All, All Origins" & measure3 == "prop. of overall burden" &
#  rural_urban_class != "All") # & Year >= 2000
#g6 <- ggplot(attr_burd6, aes(x = Year, y = mean, color = rural_urban_class))

#attr_burd8 <- attr_burd %>% filter(agr_by == "nation" & svi_bin != "All" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "prop. of overall burden" &
#  rural_urban_class == "All" ) #& Year >= 2000
#g8 <- ggplot(attr_burd8, aes(x = Year, y = mean, color = svi_bin))

## --set range---
min1 <- min(c(attr_burd1$lower, attr_burd2$lower, attr_burd3$lower, attr_burd7$lower))
#min2 <- min(c(attr_burd4$lower, attr_burd5$lower, attr_burd6$lower, attr_burd8$lower))

max1 <- max(c(attr_burd1$upper, attr_burd2$upper, attr_burd3$upper, attr_burd7$lower))
#max2 <- max(c(attr_burd4$upper, attr_burd5$upper, attr_burd6$upper, attr_burd8$lower))

g1 <- g1 + ylim(min1, max1)
g2 <- g2 + ylim(min1, max1)
g3 <- g3 + ylim(min1, max1)
g7 <- g7 + ylim(min1, max1)

#g4 <- g4 + ylim(min2, max2)
#g5 <- g5 + ylim(min2, max2)
#g6 <- g6 + ylim(min2, max2)
#g8 <- g8 + ylim(min2, max2)

g1 <- g1 + xlim(1990, 2016)
g2 <- g2 + xlim(1990, 2016)
g3 <- g3 + xlim(1990, 2016)
g7 <- g7 + xlim(1990, 2016)

#g4 <- g4 + xlim(1990, 2016)
#g5 <- g5 + xlim(1990, 2016)
#g6 <- g6 + xlim(1990, 2016)
#g8 <- g8 + xlim(1990, 2016)

# g6 <- g6 + scale_y_continuous(breaks = pretty_breaks())

#plots <- list(g1, g2, g3, g4, g5, g6) # , g7, g8
#plots <- list(g1, g2, g3, g4, g5, g6, g7, g8)
plots <- list(g1, g2, g3, g7)

#rm(min1, min2, max1, max2)
#rm(
#  attr_burd1, attr_burd2, attr_burd3, attr_burd4, attr_burd5, attr_burd6,
#  g1, g2, g3, g4, g5, g6, g7, g8
#)
#----formatting------
# group.colors <- c(hue_pal()(6), hue_pal()(3), hue_pal()(3))
# group.colors <- hue_pal()(12)

# https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
# group.colors <- c(RColorBrewer::brewer.pal(6, "Dark2"), RColorBrewer::brewer.pal(5, "YlGnBu")[c(2,4,5)], RColorBrewer::brewer.pal(5, "YlOrRd")[c(2:4)])
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

legend_plots <- lapply(plots, function(g) {
  g %>%
    get_legend() %>%
    as_ggplot()
})

# plots[[1]] <- plots[[1]]  + ggtitle("A)")
## ----get legends ---
# https://stackoverflow.com/questions/27803710/ggplot2-divide-legend-into-two-columns-each-with-its-own-title
# toy_data <- tibble(
#  x = seq_along(group.colors),
#  labels = names(group.colors)
# )
# toy_data$labels <- factor(toy_data$labels, levels = names(group.colors))
# toy_plot <- ggplot(toy_data, aes(x, x, color = labels)) +
#  geom_point() +
#  scale_color_manual(values = group.colors) +
#  theme(legend.title = element_blank()) +
#  guides(color = guide_legend(ncol = 3, byrow = TRUE))
# legend_plot <- get_legend(toy_plot)
# legend_plot <- as_ggplot(legend_plot)
# rm(toy_data, toy_plot)
## --- arrange plots----

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank())
})

t1 <- textGrob("Age-adjusted mortality per 100,000", rot = 90, gp = gpar(fontsize = 10), vjust = 1)
#t2 <- textGrob("%", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Race-Ethnicity", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)
t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)
t5 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Rurality", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)

t6 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Social Vulnerability Index", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)

#t7 <- grobTree(
#  rectGrob(gp = gpar(fill = "grey")),
#  textGrob("PM2.5-attributable mortality rate", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
#)


t7 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob(expression(PM[2.5] ~ "-attributable mortality rate"),
           rot = 90,
           gp = gpar(fontsize = 10, fontface = "bold"))
)

#t8 <- grobTree(
#  rectGrob(gp = gpar(fill = "grey")),
#  textGrob("Percentage of mortality \n attributable to PM2.5", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
#)

#gs <- append(plots, list(t3, t4, t5, t6, t7, t8, t1, t2))
gs <- append(plots, list(t3, t4, t5, t6, t7,  t1))
gs <- append(gs, legend_plots)
# gs <- append(plots, list(t1, t2, legend_plot, t3, t4, t5, t6, t7))
# gs <- lapply(1:6, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.4
figure_hight <- 0.8

# g_combined <- grid.arrange(
#  grobs = gs,
#  widths = c(0.2, 0.1, figure_width, blank_space, figure_width, blank_space, figure_width),
#  heights = c(0.1, figure_hight, blank_space, figure_hight, 0.8),
# widths = c(0.1, 0.1, figure_width , blank_space, 0.1, figure_width),
# heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.6),
#  layout_matrix = lay
# )

#lay <- rbind(
#  c(NA, NA, 7, NA, 8, NA, 9),
#  c(10, 12, 1, NA, 2, NA, 3),
#  c(NA, NA, NA, NA, NA, NA, NA),
#  c(11, 13, 4, NA, 5, NA, 6),
#  c(NA, NA, 14, NA, 15, NA, 16)
#)

#lay <- rbind(
#  c(NA, NA,  9, NA, 10,  NA, 11, NA, 12),
#  c(13, 15,  1, NA,  2,  NA,  3, NA,  7),
#  c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#  c(14, 16,  4, NA,  5, NA,  6, NA,  8),
#  c(NA, NA, 17, NA, 18, NA, 19, NA, 20)
#)

lay <- rbind(
  c(NA, NA,  9, NA, 10,  NA, 11, NA, 12),
  c(13, 15,  1, NA,  2,  NA,  3, NA,  7),
  c(NA, NA, 17, NA, 18, NA, 19, NA, 20)
)

# Update grid.arrange to account for the new column
g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.2, 0.1, figure_width, blank_space, figure_width,
             blank_space, figure_width, blank_space, figure_width), # Added a new blank_space
  heights = c(0.1, figure_hight, 0.6),
  layout_matrix = lay
)

#ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
#          legend_plots[[1]], legend_plots[[2]], legend_plots[[3]], legend_plots[[4]],
#          ncol = 4, nrow = 2,
#          heights = c(1, 0.4))

as_ggplot(g_combined)
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figure1.png"), dpi = 300, g_combined, height = 9, width = 11)
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figure1.pdf"), dpi = 300, g_combined, height = 9, width = 11)
