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
  "gridExtra", "grid", "lattice"
)
suppressMessages({pkgload::load_all()})

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

  min_ageI <- 65
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
all_burden <- all_burden %>% filter(min_age == min_ageI)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica"))
options(bitmapType = "cairo")
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----
# test <- fread(file.path(summaryDir, "all_burd.csv")) %>% #& measure2 == "age-adjusted rate per 100,000"
#  filter(Gender.Code == "All genders" & measure1 == "Deaths"  &
#           source == "National Vital Statistics System" & Region == "United States") %>%
#  filter(Education == 666 & rural_urban_class == "Large metro" &Ethnicity == "All, All Origins" & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
#           rural_urban_class != "All" & Year %in% c(2008, 2009)) %>%
#  filter(min_age == 25)

all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & Region == "United States")

## -- figure 3, attributable burden---
# TODO method di_gee/burnett
all_burden1 <- all_burden %>% filter(Education == 666 & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" & Ethnicity != "All, All Origins" & rural_urban_class == "All")
g1 <- ggplot(all_burden1, aes(x = Year, y = value, color = Ethnicity))

all_burden2 <- all_burden %>%
  filter(Education != 666 & Ethnicity == "All, All Origins" & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
    rural_urban_class == "All")

all_burden2$Education <- factor(all_burden2$Education, # Relevel group factor
  levels = c(
    "High school graduate or lower",
    "Some college education but no 4-year college degree",
    "4-year college graduate or higher"
  )
)
g2 <- ggplot(all_burden2, aes(x = Year, y = value, color = Education))

all_burden3 <- all_burden %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
    rural_urban_class != "All" & Year >= 2000)

all_burden3$rural_urban_class <- factor(all_burden3$rural_urban_class, # Relevel group factor
  levels = c(
    "Large metro",
    "Small-medium metro",
    "Non metro"
  )
)
g3 <- ggplot(all_burden3, aes(x = Year, y = value, color = rural_urban_class))

all_burden4 <- all_burden %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" &
    rural_urban_class == "All" &
    svi_bin != "All" & Year >= 2000)

g4 <- ggplot(all_burden4, aes(x = Year, y = value, color = svi_bin))

## --set range---
min1 <- min(c(all_burden1$value, all_burden2$value, all_burden3$value, all_burden4$value))
max1 <- max(c(all_burden1$value, all_burden2$value, all_burden3$value, all_burden4$value))

g1 <- g1 + ylim(min1, max1)
g2 <- g2 + ylim(min1, max1)
g3 <- g3 + ylim(min1, max1)
g4 <- g4 + ylim(min1, max1)

plots <- list(g1, g2, g3, g4)
rm(min1, max1)
rm(
  all_burden1, all_burden2, all_burden3,
  g1, g2, g3, g4
)
#----formatting------
group.colors <- get_group_colors()

plots <- lapply(plots, function(g) {
  g +
    geom_line(size = 1.5) +
    xlab("Year") +
    scale_colour_manual(values = group.colors, limits = force) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE)) # 3
})

# legend_plot <- as_ggplot(legend_plot)
legend_plots <- lapply(plots, function(g) {
  g %>%
    get_legend() %>%
    as_ggplot()
})

plots <- lapply(plots, function(g) {
  g + # theme(legend.position = "bottom",
    #      axis.title.y = element_blank())
    theme(legend.position = "none", axis.title.y = element_blank())
})

## --- arrange plots----
lay <- matrix(
  c(
    5, NA, 6, NA, 7, NA,8,
    NA, NA, NA, NA, NA, NA,NA,
    1, NA, 2, NA, 3, NA,4,
    9, NA, 10, NA, 11,NA, 12
  ),
  nrow = 4,
  byrow = TRUE
)


t1 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Race-Ethnicity", gp = gpar(fontsize = 10, fontface = "bold"))
)

t2 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"))
)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Rurality", gp = gpar(fontsize = 10, fontface = "bold"))
)

t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Social Vulnerability Index", gp = gpar(fontsize = 10, fontface = "bold"))
)

gs <- append(plots, list(t1, t2, t3, t4))
gs <- append(gs, legend_plots)
# gs <- lapply(1:9, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(figure_width, blank_space, figure_width, blank_space, figure_width, blank_space, figure_width),
  heights = c(0.1, blank_space, figure_hight, 1),
  layout_matrix = lay
)

as_ggplot(g_combined)
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figureS4.png"), dpi = 300, g_combined, height = 4, width = 8)
