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
  summaryDir <- "/Users/default/Desktop/paper2021/data/17_summary"
  #summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/18_figures"
  
  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/17_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/18_figures"
  
  summaryDir <- "/Volumes/fridljand/R/HIGH/data/17_summary"
  figuresDir <- "/Volumes/fridljand/R/HIGH/data/18_figures"
  
  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"
  
  min_ageI <- 25
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files("data/17_summary")
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable" &
           source == "National Vital Statistics System" & scenario == scenarioI &
           agr_by == "nation" & method == methodI & 
           measure3 == "value")

## -- figure 3, attributable burden---
attr_burd1 <- attr_burd %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" &
                                     Year >= 2000 & Ethnicity != "White, All Origins")
attr_burd2 <- attr_burd %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class != "All" &
                                     Year >= 2001 & Ethnicity != "White, All Origins")
attr_burd3 <- attr_burd %>% filter(Education != 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" &
                                     Year >= 2000 & Ethnicity != "White, All Origins") 
attr_burd4 <- attr_burd %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")
attr_burd5 <- attr_burd %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2001)
attr_burd6 <- attr_burd %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2001)

#cat
attr_burd1$cat <- "race-ethnicity"
attr_burd2$cat <- "race-ethnicity*rurality"
attr_burd3$cat <- "race-ethnicity*education"
attr_burd4$cat <- "education"
attr_burd5$cat <- "education*rurality"
attr_burd6$cat <- "rurality"

#make plots
attr_burd_subsets <- list(attr_burd1, attr_burd2, attr_burd3, attr_burd4, attr_burd5, attr_burd6)
attr_burd_subsets <- attr_burd_subsets %>% rbindlist()

attr_burd_subsets <- attr_burd_subsets %>%
  group_by(Year, cat) %>%
  summarise(CoV = sqrt(var(mean))/mean(mean)) %>%
  mutate(cat = as.factor(cat))


g <- ggplot(attr_burd_subsets, aes(x = Year, y = CoV, color = cat))
g <- g +
  geom_line(size = 1) +
  guides(color=guide_legend(ncol=3,byrow=TRUE))+
  theme(legend.title=element_blank()) +
  ylim(0, NA) +
  ylab("Coefficient of Variation (CoV)") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size=14.5))
g

# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path("data/18_figures", paste0(methodI,"-",scenarioI), "figureS8.png"), 
       dpi = 300, g, height = 4, width = 8)
