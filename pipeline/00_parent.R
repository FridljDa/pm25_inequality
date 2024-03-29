
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************
library(doParallel)
library(foreach)

parallel <- FALSE
if(parallel) registerDoParallel()

suppressMessages({pkgload::load_all()})
# create data directory, setwd

## ----------------directories of data--------------------------------------------------------------------------------

# create directory, where all downloaded and calculated data is stored
data.dir <- "data"
create_directory(data.dir)

# directory contains variables used in calculations, which several scripts might need
tmp.dir <- file.path(data.dir, "tmp")
dir.create(tmp.dir, recursive = T, showWarnings = F)

# directory for downloaded PM exposure data
exp.dir <- file.path(data.dir, "01_exposure")
dir.create(exp.dir, recursive = T, showWarnings = F)


# directory for downloaded TIGER/Line tract shape files
trac.dir <- file.path(data.dir, "02_tracts")
dir.create(trac.dir, recursive = T, showWarnings = F)

# this directory contains calculated year - census tract - pm level tuples
trac.exp.dir <- file.path(data.dir, "03_exp_tracts")
dir.create(trac.exp.dir, recursive = T, showWarnings = F)

exp.rr.dir <- file.path(data.dir, "04_exp_rr")
if (!file.exists(exp.rr.dir)) warning("The mrbrt_summary files from Cohen (2019) need to be downloaded")

# directory for downloaded demographic census data
dem.dir <- file.path(data.dir, "05_demog")
dir.create(dem.dir, recursive = T, showWarnings = F)

# directory for demographic data grouped by PM exposure and aggregated by county/hhs region/census region
dem.agr.dir <- file.path(data.dir, "06_dem.agr")
dir.create(dem.agr.dir, recursive = T, showWarnings = F)
agr_bys <- c("nation", "county", "STATEFP") # c("county","Census_Region","Census_division","hhs_region_number","STATEFP","nation") , "county", "nation"
#agr_bys <- c("nation", "county") #, "nation"

paf.dir <- file.path(data.dir, "07_gbd_paf")
dir.create(paf.dir, recursive = T, showWarnings = F)

total.burden.dir <- file.path(data.dir, "08_total_burden")
if (!file.exists(total.burden.dir)) warning("The total burden data from CDC wonder need to be downloaded")

total.burden.parsed.dir <- file.path(data.dir, "09_total_burden_parsed")
dir.create(total.burden.parsed.dir, recursive = T, showWarnings = F)
sources <- "nvss"

cdc.pop.dir <- file.path(data.dir, "10_cdc_population")
if (!file.exists(cdc.pop.dir)) warning("The population data from CDC wonder need to be downloaded")

pop.summary.dir <- file.path(data.dir, "12_population_summary")
dir.create(pop.summary.dir, recursive = T, showWarnings = F)

total.burden.parsed2.dir <- file.path(data.dir, "13_total_burden_rate")
dir.create(total.burden.parsed2.dir, recursive = T, showWarnings = F)

attr.burden.dir <- file.path(data.dir, "14_attr_burd")
dir.create(attr.burden.dir, recursive = T, showWarnings = F)

summary.dir <- file.path(data.dir, "17_summary")
dir.create(summary.dir, recursive = T, showWarnings = F)

figures.dir <- file.path(data.dir, "18_figures")
dir.create(figures.dir, recursive = T, showWarnings = F)


#------ paths of scripts--------
script_vector <- list.files("pipeline")
script_vector <- file.path("pipeline", script_vector)
script_vector <- setdiff(script_vector, "pipeline/00_parent.R")
# Use grep to find strings that end with ".R"
script_vector <- grep("\\.R$", script_vector, value = TRUE)
# Use grep to find strings that contain "figure"
figure_scripts_list <- grep("figure", script_vector, value = TRUE)

# Use grepl to find strings that do not contain "figure"
no_figure_scripts_list_full <- script_vector[!grepl("figure", script_vector)]

#------ running scripts of data pipeline--------
#[1] "pipeline/01_mrbrt_rr.R"
#[2] "pipeline/02_download_meta.R"
#[3] "pipeline/03_meta_cross.R"
#[4] "pipeline/04_download_cens.R"
#[5] "pipeline/05_read1990.R"
#[6] "pipeline/06_cross_walk.R"
#[7] "pipeline/07_interp.R"
#[8] "pipeline/08_download_other.R" #c(9,10,11)
#[9] "pipeline/09_ass_trac.R"
#[10] "pipeline/10_ass_trac_AKHI.R"
#[11] "pipeline/11_aggregate.R"
#[18] "pipeline/17_popsum_educ.R"
#[20] "pipeline/19_add_rate_totburd.R"
#[22] "pipeline/21_calc_attr_burd_di.R"
#[24] "pipeline/23_sum_higher_geog_level_age_adjust.R"
#[25] "pipeline/24_proportions_attr_burd.R"
#[26] "pipeline/24_sum_higher_geog_level_age_adjust_total_burden.R"
#[27] "pipeline/25_summary_attr_total_burd.R"

no_figure_scripts_list <- no_figure_scripts_list_full[c(11, 22, 24, 25, 26)] #3,14,#c(15, 20, 22, 24, 25, 26, 27)

#no_figure_scripts_list <- no_figure_scripts_list_full[c(26)]#no_figure_scripts_list <- no_figure_scripts_list_full
#24, 25, 27
#
args <- commandArgs(trailingOnly = T)
if (rlang::is_empty(args)) {
  #years <- c(2000, 2010, 2016:2011, 2009:2001, 1999:1990)
  years <- c(2015)
}else{
  years <- args[1]
}


agr_bys <- c("county") #, "county" "county", , "nation", "STATEFP"
#years <- 2016
# years <- 1998 #,1990,1991
source <- "nvss"
for (agr_by in agr_bys) {
  #use foreach instead of a for loop
  foreach(year=years) %do% { #dopar #par
    args <- paste(
      year, # 1
      data.dir = "data", # 2
      tmp.dir = "data/tmp", # 3
      exp.dir = "data/01_exposure", # 4
      trac.dir = "data/02_tracts", # 5
      exp.rr.dir = "data/04_exp_rr", # 6
      trac.exp.dir = "data/03_exp_tracts", # 7
      dem.dir = "data/05_demog", # 8
      dem.agr.dir ="data/06_dem.agr", # 9
      agr_by = agr_by, # 10
      paf.dir = "data/07_gbd_paf", # 11
      total.burden.dir = "data/08_total_burden", # 12
      total.burden.parsed.dir = "data/09_total_burden_parsed", # 13
      source = source, # 14
      cdc.pop.dir = "data/10_cdc_population", # 15
      pop.summary.dir = "data/12_population_summary", # 16
      total.burden.parsed2.dir ="data/13_total_burden_rate", # 17
      attr.burden.dir ="data/14_attr_burd", # 18
      summaryHigherDir = "data/15_sum_higher_geog_level", #19
      propOfAttrBurdDir = "data/16_prop_of_attr_burd" #20
    )
    for(no_figure_script in no_figure_scripts_list){
     # run_script(script = no_figure_script, args = args)
    }
    #cat("Year: ", year, "agr_by: ", agr_by)
  }
}

if(parallel) stopImplicitCluster()
#------ running scripts of figures--------
scenario <- "real"
method <- "di_gee"
args <- paste(
  tmp.dir, # 1
  dem.dir, # 2
  dem.agr.dir, # 3
  pop.summary.dir, # 4
  total.burden.parsed2.dir, # 5
  attr.burden.dir, # 6
  summary.dir = "data/17_summary", # 7
  figures.dir = "data/18_figures", # 8
  exp.rr.dir, # 9
  scenario, # 10
  method, # 11
  propOfAttrBurdDir = "data/16_prop_of_attr_burd", # 12
  min_age = 25 #13
)

#run_script(script = "pipeline/25_summary_attr_total_burd.R", args = args)
run_script(script = "pipeline/27_summary_other_pm.R", args = args)

args <- paste(
  tmp.dir, # 1
  dem.dir, # 2
  dem.agr.dir, # 3
  pop.summary.dir, # 4
  total.burden.parsed2.dir, # 5
  attr.burden.dir, # 6
  summary.dir = "data/17_summary", # 7
  figures.dir = "data/18_figures_65+", # 8
  exp.rr.dir, # 9
  scenario, # 10
  method, # 11
  propOfAttrBurdDir = "data/16_prop_of_attr_burd", # 12
  min_age = 25 #13
)
for(figure_script in figure_scripts_list) run_script(script = figure_script, args = args)
