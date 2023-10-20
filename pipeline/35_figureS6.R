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
file_list <- file_list[grepl("attr_bur", file_list)]
file_list <- file_list[grepl("nation", file_list)]
file_list <- file.path(summaryDir, file_list)
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T, fill=TRUE)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
rm(file_list)

devtools::load_all()
theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable" &
           source == "National Vital Statistics System" & scenario == scenarioI)

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           Region == "United States" & method == "di_gee" & measure3 == "value" & scenario == "real" &
           Education == 666 & Ethnicity != "All, All Origins" & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" & rural_urban_class == "All") #%>%
  #select(Year, Ethnicity, mean)

attr_burden_proportion_to_black <- attr_burd %>%
  group_by(Year) %>%
  mutate(proportion_to_black = 1-mean/mean[Ethnicity == "Black American"]) %>%
  filter(Ethnicity != "Black American")

# Define the delta_method_product function here (as provided earlier)

# Your attr_burd data frame (replace this with your actual data)
# attr_burd <- ...

# Calculate the ratio and its confidence intervals
# Define the delta_method_product function here (as provided earlier)

# Your attr_burd data frame (replace this with your actual data)
# attr_burd <- ...

# Calculate the ratio and its confidence intervals
attr_burd_ratio_to_black <- attr_burd %>%
  group_by(Year) %>%
  mutate(
    mean_black = mean[Ethnicity == "Black American"],
    lb_black = lower[Ethnicity == "Black American"],
    ub_black = upper[Ethnicity == "Black American"]
  )

attr_burd_ratio_to_black <- attr_burd_ratio_to_black %>%
  rowwise() %>%
  mutate(
    delta_method_result = list(delta_method_product(
      mean_x = mean, lb_x = lower, ub_x = upper,
      mean_y = 1 / mean_black, lb_y = 1 / ub_black, ub_y = 1 / lb_black,
      alpha = 0.05
    ))
  )

attr_burd_ratio_to_black <- attr_burd_ratio_to_black %>%
  unnest_wider(delta_method_result, names_repair = "unique") %>%
  ungroup()

attr_burd_ratio_to_black <- attr_burd_ratio_to_black %>%
  rename(ratio_mean = mean...28,
         ratio_lower = lb,
         ratio_upper = ub)

attr_burd_ratio_to_black <- attr_burd_ratio_to_black%>%
  mutate(
    proportion_to_black = 1 - ratio_mean,
    lb_proportion_to_black = 1 - ratio_lower,
    ub_proportion_to_black = 1 - ratio_upper
  )

attr_burd_ratio_to_black <- attr_burd_ratio_to_black %>%
  filter(Ethnicity != "Black American")

# This will add columns 'proportion_to_black', 'lb_proportion_to_black', and 'ub_proportion_to_black' to attr_burd_ratio_to_black

#1- ....

g <- ggplot(attr_burd_ratio_to_black, aes(x = Year, y = proportion_to_black, color = Ethnicity)) +
  geom_line()+
  geom_ribbon(aes(ymin = lb_proportion_to_black, ymax = ub_proportion_to_black),
              linetype = 2, alpha = 0, show.legend = FALSE) +
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
    geom_line(linewidth = 1.5) +
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

ggsave(file.path("data/18_figures", paste0(methodI,"-",scenarioI, "-", min_ageI), "figureS6.png"), dpi = 300, g_combined, height = 4, width = 8)
##numbers for capation
attr_burden_proportion_to_black %>%
  group_by(Ethnicity) %>%
  summarise(mean_percent = 100*mean(proportion_to_black),
            max_year = max (Year),
            min_year = min(Year))
