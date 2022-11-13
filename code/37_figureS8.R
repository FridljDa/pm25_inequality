#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "stringr", #"tidyr",
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

# TODO delete
if (rlang::is_empty(args)) {
  scenarioI <- "real"
  methodI <- "di_gee"

  summaryDir <- "/Users/default/Desktop/paper2021/data/15_summary"
  # summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/16_figures"

  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/15_summary"
  # figuresDir <- "C:/Users/Daniel/Desktop/paper2021/data/16_figures"

  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/15_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/16_figures"

  summaryDir <- "data/15_summary"
  figuresDir <- "data/16_figures"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = TRUE)
all_burd <- file.path(summaryDir, "all_burd.csv") %>% fread()
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI)

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & agr_by == "nation")

## -- figure 3, attributable burden---
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "di_gee" & Year >= 1999)
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "di_gee") # burnett
attr_burd2$Education <- factor(attr_burd2$Education,                 # Relevel group factor
                               levels = c("High school graduate or lower", 
                                          "Some college education but no 4-year college degree",
                                          "4-year college graduate or higher"))

g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education))


attr_burd3 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" &
  rural_urban_class != "All" & Year >= 2000 & method == "di_gee")
attr_burd3$rural_urban_class <- factor(attr_burd3$rural_urban_class,                 # Relevel group factor
                                       levels = c("Large metro", 
                                                  "Small-medium metro",
                                                  "Non metro"))

g3 <- ggplot(attr_burd3, aes(x = Year, y = mean, color = rural_urban_class))


attr_burd4 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "burnett" & Year >= 1999)
g4 <- ggplot(attr_burd4, aes(x = Year, y = mean, color = Ethnicity))

attr_burd5 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "burnett") # burnett
g5 <- ggplot(attr_burd5, aes(x = Year, y = mean, color = Education))


attr_burd6 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" &
  rural_urban_class != "All" & Year >= 2000 & method == "burnett")
g6 <- ggplot(attr_burd6, aes(x = Year, y = mean, color = rural_urban_class))

attr_burd7 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "GBD" & Year >= 1999)
g7 <- ggplot(attr_burd7, aes(x = Year, y = mean, color = Ethnicity))

attr_burd8 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All" &
  method == "GBD") # burnett
g8 <- ggplot(attr_burd8, aes(x = Year, y = mean, color = Education))

attr_burd9 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" &
  rural_urban_class != "All" & Year >= 2000 & method == "GBD")
g9 <- ggplot(attr_burd9, aes(x = Year, y = mean, color = rural_urban_class))
## --set range---
min_y_scale <- min(c(
  attr_burd1$lower, attr_burd2$lower, attr_burd3$lower, attr_burd4$lower, attr_burd5$lower, attr_burd6$lower, attr_burd7$lower,
  attr_burd8$lower, attr_burd9$lower
))

max_y_scale <- max(c(
  attr_burd1$lower, attr_burd2$lower, attr_burd3$lower, attr_burd4$lower, attr_burd5$lower, attr_burd6$lower, attr_burd7$lower,
  attr_burd8$lower, attr_burd9$lower
))

if (TRUE) {
  g1 <- g1 + ylim(min_y_scale, max_y_scale)
  g2 <- g2 + ylim(min_y_scale, max_y_scale)
  g3 <- g3 + ylim(min_y_scale, max_y_scale)
  g4 <- g4 + ylim(min_y_scale, max_y_scale)
  g5 <- g5 + ylim(min_y_scale, max_y_scale)
  g6 <- g6 + ylim(min_y_scale, max_y_scale)
  g7 <- g7 + ylim(min_y_scale, max_y_scale)
  g8 <- g8 + ylim(min_y_scale, max_y_scale)
  g9 <- g9 + ylim(min_y_scale, max_y_scale)
} else {
  g1 <- g1 + scale_y_continuous(limits = c(0, NA))
  g2 <- g2 + scale_y_continuous(limits = c(0, NA))
  g3 <- g3 + scale_y_continuous(limits = c(0, NA))
  g4 <- g4 + scale_y_continuous(limits = c(0, NA))
  g5 <- g5 + scale_y_continuous(limits = c(0, NA))
  g6 <- g6 + scale_y_continuous(limits = c(0, NA))
}


# g6 <- g6 + scale_y_continuous(breaks= pretty_breaks())

plots <- list(g1, g2, g3, g4, g5, g6, g7, g8, g9)
rm(min_y_scale, max_y_scale)
rm(
  attr_burd1, attr_burd2, attr_burd3,
  g1, g2, g3, g4, g5, g6, g7, g8, g9
)
#----formatting------
# https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
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
    geom_line(size = 1.5) +
    xlab("Year") +
    # geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
    scale_colour_manual(
      values = group.colors,
      limits = force
    ) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE))
})

legend_plots <- lapply(plots[1:3], function(g) {
  g %>%
    get_legend() %>%
    as_ggplot()
})

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank()) 
})
## --- arrange plots----

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank())
})


lay <- rbind(
  c(NA, 13, NA, 14, NA, 15),
  c(10, 1, NA, 2, NA, 3),
  c(NA, NA, NA, NA, NA, NA),
  c(11, 4, NA, 5, NA, 6),
  c(NA, NA, NA, NA, NA, NA),
  c(12, 7, NA, 8, NA, 9),
  c(NA, 16, NA, 17, NA, 18)
)

#t1 <- textGrob("Age-adjusted mortality rate per 100,000", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

t2 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Race-Ethnicity", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0.5)
)
t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0.5)
)
t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Rurality", gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0.5)
)

t5 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Di", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
)

t6 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("GEMM", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
)

t7 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("GBD", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
)

# gs <- append(plots, list(t1, t2, legend_plot, t3, t4, t5, t6, t7))
#gs <- append(plots, list(t1, legend_plot, t2, t3, t4, t5, t6, t7))
gs <- append(plots, list(t5, t6, t7, t2, t3, t4))
gs <- append(gs, legend_plots)
# gs <- lapply(1:14, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.1, figure_width, blank_space, figure_width, blank_space, figure_width),
  heights = c(0.1, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 1),
  # widths = c(0.15, 0.1, figure_width, blank_space, figure_width, blank_space, figure_width),
  # heights = c(0.1, figure_hight, blank_space, figure_hight, blank_space, figure_hight, figure_hight),
  layout_matrix = lay
)

# g_combined <- grid.arrange(
#  grobs = gs,
#  widths = c(0.1, 0.1, figure_width , blank_space, 0.1, figure_width),
#  heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.6),
#  layout_matrix = lay
# )

as_ggplot(g_combined)
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI), "figureS8.png"), dpi = 300, g_combined, height = 9, width = 8)
