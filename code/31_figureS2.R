#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "ggplot2", "scales", "grid", "cowplot",
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice", "ggpubr"
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
  summaryDir <- "/Users/default/Desktop/paper2021/data/15_summary"
  #summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/16_figures"
  
  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/15_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/16_figures"
  
  summaryDir <- "data/15_summary"
  figuresDir <- "data/16_figures"
  
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files("data/15_summary")
file_list <- file.path("data/15_summary", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable" &
           source == "National Vital Statistics System" & scenario == scenarioI)

## -- figure 3, attributable burden---
#TODO method di_gee/burnett
attr_burden_proportion_to_black <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           Region == "United States" & method == "di_gee" & measure3 == "value" & scenario == "real" &
           Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All") %>%
  select(Year, Ethnicity, mean) %>%
  #mutate(ethnicity_african_american = ifelse(Ethnicity == "Black American", 
  #       "Black")) %>%
  tidyr::pivot_wider(names_from = Ethnicity, values_from = mean) 

attr_burden_proportion_to_black <-attr_burden_proportion_to_black%>%
  mutate(across(`American Indian or Alaska Native`:`NH White`, 
                # ~ scales::label_percent(0.1)(1-.x / `Black American`)
                ~ 1-.x / `Black American`
  )
  )  %>%
  tidyr::pivot_longer(
    !Year,
    names_to = "Ethnicity",
    values_to = "proportion_to_black" # starts_with("proportion_to_african")
  ) %>%
  filter(!(Ethnicity %in% c("Hispanic or Latino White", "NH White") & Year < 2000)) %>%
  filter(!(Ethnicity == "White"& Year > 2000) ) %>%
  filter(Ethnicity != "Black American") 

g <- ggplot(attr_burden_proportion_to_black, aes(x = Year, y = proportion_to_black, color = Ethnicity)) +
  geom_line()+
  ylab("relative difference to Black American")+
  scale_y_continuous(labels = scales::percent)

plots <- list(g)

#----formatting------
group.colors <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6,8:10, 12)], 
                  RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2])
group.colors[c(12,2)] <- group.colors[c(2,12)] 
names(group.colors) <- c("NH White",
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
group.colors <- group.colors[c(1:2,4:6)]

plots <- lapply(plots, function(g) {
  g +
    geom_line(size = 1.5) +
    xlab("Year") +
    #geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
    scale_colour_manual(values=group.colors) +
    theme(legend.title = element_blank()) +
    guides(color=guide_legend(ncol=3,byrow=TRUE), linetype = "none")
  
})

legend_plot <- get_legend(plots[[1]])
legend_plot <- as_ggplot(legend_plot)

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank()) 
  
})

## --- arrange plots----
lay <- rbind(
  c(2),
  c(NA),
  c(1),
  c( 3)
)

t1 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Relative difference in PM2.5 attributable \n mortality to Black American", gp = gpar(fontsize = 10, fontface = "bold"))
)
t2 <- textGrob("%", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

gs <- append(plots, list(t1, legend_plot))

blank_space <- 0.05
figure_width <- 0.7
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(figure_width),
  heights = c(0.2, blank_space, figure_hight, 0.4),
  layout_matrix = lay
)

as_ggplot(g_combined)

ggsave(file.path("data/16_figures", paste0(methodI,"-",scenarioI), "figureS2.png"), dpi = 300, g_combined, height = 4, width = 8)
##numbers for capation
attr_burden_proportion_to_black %>%
  group_by(Ethnicity) %>%
  summarise(mean_percent = 100*mean(proportion_to_black),
            max_year = max (Year),
            min_year = min(Year))
