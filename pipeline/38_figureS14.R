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

  scenarioI <- "real"
  methodI <- "di_gee"
  min_ageI <- 25
}

file_list <- list.files("data/17_summary")
file_list <- file.path("data/17_summary", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
all_burd <- file.path(summaryDir, "all_burd.csv") %>% fread()
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
rm(file_list)

if(min_age == 65){
  quit()
}
theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo"); 

# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- all burden----

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & agr_by == "nation" &
    Ethnicity != "All, All Origins" & rural_urban_class == "All" & Education == 666) # %>%
# filter(Ethnicity != "White") #Year %in% c(2000, 2015) &

all_burd <- all_burd %>%
  select(Year, Ethnicity, overall_value)

## -- attributable burden---

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" &
    rural_urban_class == "All" & method %in% c("GBD","burnett") & Ethnicity != "White")

attr_burd <- attr_burd %>%
  filter(measure3 == "proportion of disparity to Black or African American attributable")

attr_burd <- attr_burd %>%
  left_join(all_burd %>% mutate(Year = as.integer(Year)),
    by = c("Year", "Ethnicity")
  )


attr_burd <- attr_burd %>%
  filter(measure3 == "proportion of disparity to Black or African American attributable") %>%
  select(Year, Ethnicity, method, measure3, mean, overall_value) %>%
  mutate(xlab = "%")

### ---- combine -----

attr_burd_combined <- attr_burd %>%
  filter(Year %in% 2000:2015)

attr_burd_average <- attr_burd_combined %>%
  group_by(Ethnicity, measure3, method, xlab) %>%
  summarise(
    mean = mean(mean),
    overall_value = mean(overall_value)
  )

attr_burd_combined <-
  rbind(
    attr_burd_combined <- attr_burd_combined %>%
      filter(Year %in% c(2000, 2015)) %>%
      mutate(Year = as.factor(Year)),
    attr_burd_average <- attr_burd_average %>%
      mutate(Year = as.factor("Mean between 2000 and 2015"))
  )

## ---plotting---
attr_burdens <- list(attr_burd_combined %>% filter(method == "GBD"),
                      attr_burd_combined %>% filter(method == "burnett"))

plots <- lapply(attr_burdens, function(attr_burdens_i) {
  attr_burd_i_wide <- attr_burdens_i %>%
    pivot_wider(
      names_from = Year,
      values_from = c(mean, overall_value),
      names_prefix = "year_",
      id_cols = c(Ethnicity, method)
    )

  g_i <- ggplot() +
    geom_segment(
      data = attr_burd_i_wide,
      aes(y = Ethnicity, yend = Ethnicity, x = mean_year_2000, xend = mean_year_2015)
    ) +
    geom_point(
      data = attr_burdens_i,
      aes(y = Ethnicity, x = mean, colour = Year, size = overall_value) # ,size = overall_value #TODO
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

g3 <- plots[[2]]

g1 <- g1 +
  theme(legend.direction="horizontal")
legend <- get_legend(g1)

g1 <- g1 + theme(legend.position = "none")

g3 <- g3 +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

g1 <- g1 +
  xlab("Percent")+
  xlim(0,NA)+
  ggtitle("GBD") +
  ylab("Race-Ethnicity") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 9)) #, face = "bold"
g1

#"Percent of the all-cause mortality difference\n to Black Americans attributable to PM2.5"

g3 <- g3 +
  xlab("Percent")+
  xlim(0,NA)+
  ggtitle("GEMM")+
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

ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI), "figureS14.png"), dpi = 300, gg_combined, height = 6, width = 8)
