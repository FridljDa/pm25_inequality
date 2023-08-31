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
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice", "ggsci"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
suppressMessages({pkgload::load_all()})
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
  scenarioI <- "real"
  methodI <- "di_gee"
  min_ageI <- 25

  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, read_data) %>% rbindlist(use.names = TRUE, fill = TRUE)
all_burd <- file.path(summaryDir, "all_burd.csv") %>% read_data()
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
all_burd <- all_burd %>% filter(min_age == min_ageI)
#rm(file_list)
unique(attr_burd$Education)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----


attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable" & svi_bin == "All" &
           source == "National Vital Statistics System" & scenario == scenarioI &
           agr_by == "nation" & method == methodI & Year >= 2009 & measure3 == "value" & Ethnicity != "All, All Origins"
         & rural_urban_class == "All" & Education != 666
         )

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           source == "National Vital Statistics System" & agr_by == "nation" & svi_bin == "All" &
           Ethnicity != "All, All Origins"  & rural_urban_class == "All" & Education != 666)

## -- figure 3, attributable burden---
attr_burd1 <- attr_burd %>% filter(Education == "4-year college graduate or higher"  & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))
g1 <- g1 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd2 <- attr_burd %>% filter(Education == "Some college education but no 4-year college degree"  & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Ethnicity))
g2 <- g2 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd3 <- attr_burd %>% filter(Education == "High school graduate or lower" & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g3 <- ggplot(attr_burd3, aes(x = Year, y = mean, color = Ethnicity))
g3 <- g3 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

all_burd4 <- all_burd %>% filter(Education == "4-year college graduate or higher"  & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g4 <- ggplot(all_burd4, aes(x = Year, y = value, color = Ethnicity))

all_burd5 <- all_burd %>% filter(Education == "Some college education but no 4-year college degree"  & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g5 <- ggplot(all_burd5, aes(x = Year, y = value, color = Ethnicity))

all_burd6 <- all_burd %>% filter(Education == "High school graduate or lower" & Ethnicity != "All, All Origins"  & rural_urban_class == "All")
g6 <- ggplot(all_burd6, aes(x = Year, y = value, color = Ethnicity))


## --set range---
min1 <- min(c(attr_burd1$lower, attr_burd2$lower, attr_burd3$lower))
min2 <- min(c(all_burd4$value, all_burd5$value, all_burd6$value))

max1 <- max(c(attr_burd1$upper, attr_burd2$upper, attr_burd3$upper))
max2 <- max(c(all_burd4$value, all_burd5$value, all_burd6$value))

g1 <- g1 + ylim(0, max1)
g2 <- g2 + ylim(0, max1)
g3 <- g3 + ylim(0, max1)
g4 <- g4 + ylim(0, max2)
g5 <- g5 + ylim(0, max2)
g6 <- g6 + ylim(0, max2)

#g4 <- g4 +scale_y_continuous(limits = c(0, NA))
#g5 <- g5 +scale_y_continuous(limits = c(0, NA))
#g6 <- g6 +scale_y_continuous(limits = c(0, NA))

if(TRUE){
    #g1 <- g1 + ylim(min1, max1)
    #g2 <- g2 + ylim(min1, max1)
    #g3 <- g3 + ylim(min1, max1)
    #g4 <- g4 + ylim(min2, max2)
    #g5 <- g5 + ylim(min2, max2)
    #g6 <- g6 + ylim(min2, max2)
}else{
  #g1 <- g1 +scale_y_continuous(limits = c(0, NA))
  #g2 <- g2+scale_y_continuous(limits = c(0, NA))
  #g3 <- g3 +scale_y_continuous(limits = c(0, NA))

}


#g6 <- g6 + scale_y_continuous(breaks= pretty_breaks())

plots <- list(g1, g2, g3, g4, g5, g6)
rm(min1, min2, max1, max2)
rm(
  attr_burd1, attr_burd2, attr_burd3, all_burd4, all_burd5, all_burd6,
  g1, g2, g3, g4, g5, g6
)
#----formatting------
#https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
group.colors <- get_group_colors()
#group.colors <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6,8:10, 12)],
#                  RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2])
#group.colors[c(12,2)] <- group.colors[c(2,12)]

#names(group.colors) <- c("NH White",
#                         "Hispanic or Latino White",
#                         "Black American",
#                         "White",
#                         "Asian or Pacific Islander",
#                         "American Indian or Alaska Native"
#)
#group.colors <- group.colors[c(1:3,5)]

plots <- lapply(plots, function(g) {
  g +
    geom_line(size = 1.5) +
    xlab("Year") +
    scale_colour_manual(values=group.colors) +
    theme(legend.title = element_blank()) +
    guides(color=guide_legend(ncol=3,byrow=TRUE))
})
## ----get legends ---
#https://stackoverflow.com/questions/27803710/ggplot2-divide-legend-into-two-columns-each-with-its-own-title
toy_data <- tibble(x = seq_along(group.colors),
                   labels = names(group.colors)
)
toy_data$labels <- factor(toy_data$labels, levels = names(group.colors))
toy_plot <- ggplot(toy_data, aes(x, x, color = labels)) +
  geom_point() +
  scale_color_manual(values = group.colors) +
  theme(legend.title = element_blank()) +
  guides(color=guide_legend(ncol=3,byrow=TRUE))
legend_plot <- get_legend(toy_plot)
legend_plot <- as_ggplot(legend_plot)
rm(toy_data, toy_plot)

## --- arrange plots----

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank())

})

lay <- rbind(
  c(NA, NA, 13, NA,  14),
  c(10, 7, 1, NA,  4),
  c(NA, 7, NA, NA,  NA),
  c(11, 7, 2, NA,  5),
  c(NA, 7, NA, NA,  NA),
  c(12, 7, 3, NA,  6),
  c(NA, NA, 9, 9,  9)
)

t1 <- textGrob("Age-adjusted mortality per 100,000", rot = 90, gp = gpar(fontsize = 10), vjust = 1)
t2 <- textGrob("", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("4-year college graduate or higher", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)
t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Some college education \n but no 4-year college degree", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0.5)
)#"Some college education but no 4-year college degree"
t5 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("High school graduate or lower", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 0)
)

t6 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("PM2.5-attributable mortality", gp = gpar(fontsize = 10, fontface = "bold"))
)
t7 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("All-cause mortality", gp = gpar(fontsize = 10, fontface = "bold"))
)

gs <- append(plots, list(t1,  legend_plot, t3, t4, t5, t6, t7))
#gs <- lapply(1:14, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.25, 0.1, figure_width , blank_space,  figure_width),
  heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.6),
  layout_matrix = lay
)


as_ggplot(g_combined)
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI,"-",scenarioI), "figure3.png"), dpi = 300, g_combined, height = 9, width = 8)
ggsave(file.path(figuresDir, paste0(methodI,"-",scenarioI), "figure3.pdf"), dpi = 300, g_combined, height = 9, width = 8)
