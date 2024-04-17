#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggpubr)


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

file_list <- list.files("data/17_summary")
file_list <- file_list[grepl("attr_bur", file_list)]
file_list <- file_list[grepl("nation", file_list)]
file_list <- file.path("data/17_summary", file_list)
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T, fill=TRUE)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
all_burd <- file.path(summaryDir, "all_burd.csv") %>% fread()
all_burd <- all_burd %>% filter(min_age == min_ageI)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");

if(min_ageI == 65){
  quit()
}
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- all burden----

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & agr_by == "nation" &
    Ethnicity != "All, All Origins" & rural_urban_class == "All" & svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" & Education == 666) # %>%
# filter(Ethnicity != "White") #Year %in% c(2000, 2015) &

all_burd <- all_burd %>%
  select(Year, Ethnicity, value)

## -- attributable burden---
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &svi_bin == "All" & svi_bin1 == "All" & svi_bin2 == "All" & svi_bin3 == "All" & svi_bin4 == "All" &
    source == "National Vital Statistics System" & scenario == scenarioI & agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" &
    rural_urban_class == "All" & method %in% c("di_gee") & Ethnicity != "White")

attr_burd <- attr_burd %>%
  left_join(all_burd %>% mutate(Year = as.integer(Year)),
    by = c("Year", "Ethnicity")
  )

attr_burd1 <- attr_burd %>%
  filter(measure3 %in% c("prop. of overall burden")) %>%
  select(Year, Ethnicity, measure3, mean, value) %>%
  mutate(xlab = "%")
attr_burd2 <- attr_burd %>%
  filter(measure3 == "proportion of disparity to Black or African American attributable") %>%
  select(Year, Ethnicity, measure3, mean, value) %>%
  mutate(xlab = "%")

## calculate difference
attr_burd3 <- attr_burd %>%
  filter(measure3 %in% c("value")) %>%
  select(Year, Ethnicity, measure3, mean, value)

attr_burd3 <- attr_burd3 %>%
  group_by(Year) %>%
  # filter("NH White" %in% c(Ethnicity) & "Black or African American" %in% Ethnicity) %>%
  mutate(difference = mean[Ethnicity == "Black American"] - mean) %>%
  ungroup() %>%
  filter(Ethnicity != "Black American") %>%
  select(Year, Ethnicity, mean = difference, value) %>%
  mutate(
    measure3 = "absolute difference",
    xlab = "age-adjusted mortality rate"
  )

### ---- combine -----
# & Year %in% c(2000, 2015)
attr_burd_combined <- rbindlist(list(attr_burd1, attr_burd2, attr_burd3), use.names = TRUE)

attr_burd_combined <- attr_burd_combined %>%
  filter(Year %in% 2000:2015)

attr_burd_average <- attr_burd_combined %>%
  group_by(Ethnicity, measure3, xlab) %>%
  summarise(
    mean = mean(mean),
    value = mean(value)
  )

attr_burd_combined <-
  rbind(
    attr_burd_combined <- attr_burd_combined %>%
      filter(Year %in% c(2000, 2015)) %>%
      mutate(Year = as.factor(Year)),
    attr_burd_average <- attr_burd_average %>%
      mutate(Year = as.factor("Mean between 2000 and 2015"))
  )

attr_burd_combined_wide <- attr_burd_combined %>%
  filter(Year %in% c(2000, 2015)) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(mean, value),
    names_prefix = "year_",
    id_cols = c(Ethnicity, measure3, xlab)
  )

## ---plotting---
attr_burdens <- split(attr_burd_combined, attr_burd_combined$measure3)

plots <- lapply(attr_burdens, function(attr_burdens_i) {
  attr_burd_i_wide <- attr_burdens_i %>%
    pivot_wider(
      names_from = Year,
      values_from = c(mean, value),
      names_prefix = "year_",
      id_cols = c(Ethnicity, measure3)
    )

  g_i <- ggplot() +
    geom_segment(
      data = attr_burd_i_wide,
      aes(y = Ethnicity, yend = Ethnicity, x = mean_year_2000, xend = mean_year_2015)
    ) +
    geom_point(
      data = attr_burdens_i,
      aes(y = Ethnicity, x = mean, colour = Year, size = value) # ,size = value #TODO
    ) +
    scale_colour_manual(values = c("2000" = "#a3c4dc", "2015" = "#0e668b", "Mean between 2000 and 2015" = "#8b0000")) +
    #scale_shape_manual(values = c("2000" = 19, "2015" = 19, "Mean between 2000 and 2015" = 17)) + #shape = 17 "\u007C"
    scale_size_area() + # + scale_size_binned()
    # guides(size = "none") +
    labs(size = "All-cause mortality per 100,000") +
    xlab("")

  g_i
})

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

g1 <- plots[[1]]
g2 <- plots[[2]]
g3 <- plots[[3]]

g1 <- g1 +
  theme(legend.direction="horizontal")
legend <- get_legend(g1)

g1 <- g1 + theme(legend.position = "none")
g2 <- g2 + theme(legend.position = "none")
g3 <- g3 +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

g1 <- g1 +
  xlab("Age-adjusted mortality per 100,000")+
  xlim(0,NA)+
  ggtitle("Absolute difference to Black Americans\n in mortality attributable to PM2.5") +
  ylab("Race-Ethnicity") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 9)) #, face = "bold"
g1
#g2 <- g2 +
#  xlab("%")+
#  ggtitle("Percent of all-cause\n mortality attributable to PM2.5")+
#  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
#        axis.text.x = element_text(size = 9))

g3 <- g3 +
  xlab("Percent")+
  xlim(0,NA)+
  ggtitle("Percent of the all-cause mortality difference\n to Black Americans attributable to PM2.5")+
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 9))

##----arrange--
lay <- rbind(
  c(1,2),
  c(3,3)
)

gg_combined <- gridExtra::grid.arrange(g1, g3, legend, layout_matrix = lay,
                                       widths = c(1.7, 1),
                                       heights = c(1,1/3)) # , rel_heights = c(1/2, 1/2)
# gg_combined <- cowplot::plot_grid(g1, g3,g2, legend,  nrow = 2)
as_ggplot(gg_combined)

ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figure2.png"), dpi = 300, gg_combined, height = 6, width = 8)
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figure2.pdf"), dpi = 300, gg_combined, height = 6, width = 8)
