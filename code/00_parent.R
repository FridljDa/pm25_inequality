
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))


# memory.limit(size=1800)

# runtime configuration
# run cripts from command line depending on OS
if (Sys.info()["sysname"] == "Darwin") {
  runscript <- function(script, args = "") {
    system(paste("Rscript", script, args))
  }
} else if (Sys.info()["sysname"] == "Windows") {
  memory.limit(size = 500000)

  exec <- paste0("C:/Program Files/R/R-", R.Version()$major, ".", R.Version()$minor, "/bin/Rscript.exe")
  exec <- shQuote(exec)
  runscript <- function(script, args = "") {
    system(paste(exec, "--vanilla", script, args))
  }
} else {
  runscript <- function(script, args = "") {
    system(paste("Rscript", script, args))
  }
  # print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

## ----------------directories--------------------------------------------------------------------------------
# create data directory, setwd
code_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# code_dir <- "/g/huber/users/fridljand/R/HIGH/code"
h_root <- dirname(code_dir)
setwd(h_root)

# create directory, where all downloaded and calculated data is stored
data.dir <- file.path(h_root, "data")
dir.create(data.dir, recursive = T, showWarnings = F)


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
agr_bys <- c("nation", "STATEFP", "county") # c("county","Census_Region","Census_division","hhs_region_number","STATEFP","nation") , "county", "nation"
agr_bys <- c("county")

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

summary.dir <- file.path(data.dir, "15_summary")
dir.create(summary.dir, recursive = T, showWarnings = F)

figures.dir <- file.path(data.dir, "16_figures")
dir.create(figures.dir, recursive = T, showWarnings = F)


# paths of scripts
mrbrtRR.script <- file.path(code_dir, "01_mrbrt_rr.R")
download.meta.script <- file.path(code_dir, "02_download_meta.R")
meta.cross.script <- file.path(code_dir, "03_meta_cross.R")
download.cens.script <- file.path(code_dir, "04_download_cens.R")
read1990.script <- file.path(code_dir, "05_read1990.R")
cross.walk.script <- file.path(code_dir, "06_cross_walk.R")
interp_script <- file.path(code_dir, "07_interp.R")
download.other.script <- file.path(code_dir, "08_download_other.R")
assignTract.script <- file.path(code_dir, "09_ass_trac.R")
assignTractAKHI.script <- file.path(code_dir, "10_ass_trac_AKHI.R")
cens_agr_script <- file.path(code_dir, "11_aggregate.R")
rural.urban.script <- file.path(code_dir, "11_rural_urban_class.R")
paf.script <- file.path(code_dir, "12_paf_gbd.R")
read.nvs.findrepl.script <- file.path(code_dir, "13_nvss_findrepl.R")
read.total.burden.nvs.script <- file.path(code_dir, "14_read_tot_nvss.R")
pop.summary.script <- file.path(code_dir, "15_popsum.R")
pop.summary.educ.script <- file.path(code_dir, "16_popsum_educ.R")
add.rate.tot.burd <- file.path(code_dir, "18_add_rate_totburd.R")
calc.attr.burd1.script <- file.path(code_dir, "19_calc_attr_burd_gbd.R")
calc.attr.burd2.script <- file.path(code_dir, "20_calc_attr_burd_gemm.R")
calc.attr.burd3.script <- file.path(code_dir, "21_calc_attr_burd_di.R")
summary.script <- file.path(code_dir, "22_summary.R")
summary.other.script <- file.path(code_dir, "23_summary_other.R")
figure1.script <- file.path(code_dir, "25_figure1.R")
figure2.script <- file.path(code_dir, "26_figure2.R")
figure3.script <- file.path(code_dir, "27_figure3.R")
figure4.script <- file.path(code_dir, "28_figure4.R")
figure5.script <- file.path(code_dir, "29_figure5.R")

figureS1.script <- file.path(code_dir, "30_figureS1.R")
figureS2.script <- file.path(code_dir, "31_figureS2.R")
figureS3.script <- file.path(code_dir, "32_figureS3.R")
figureS4.script <- file.path(code_dir, "33_figureS4.R")
figureS6.script <- file.path(code_dir, "34_figureS6.R")
figureS7.script <- file.path(code_dir, "35_figureS7.R")
figureS4.script <- file.path(code_dir, "36_figureS4.R")

#--------parameters of code-------------------
args <- paste(tmp.dir, exp.rr.dir)
# runscript(script=mrbrtRR.script, args = args)


years <- c(1990, 2000, 2010, 2016:2011, 2009:2001, 1999:1991)

years <- c(2016)
# years <- c(2000,1990,1991) #,1990,1991
for (agr_by in agr_bys) {
  for (source in sources) {
    for (year in years) {
      args <- paste(
        year, # 1
        data.dir, # 2
        tmp.dir, # 3
        exp.dir, # 4
        trac.dir, # 5
        exp.rr.dir, # 6
        trac.exp.dir, # 7
        dem.dir, # 8
        dem.agr.dir, # 9
        agr_by, # 10
        paf.dir, # 11
        total.burden.dir, # 12
        total.burden.parsed.dir, # 13
        source, # 14
        cdc.pop.dir, # 15
        pop.summary.dir, # 16
        total.burden.parsed2.dir, # 17
        attr.burden.dir # 18
      )
      # runscript(script = download.meta.script, args = args)
      # runscript(script = meta.cross.script, args = args)
      if (year %in% c(2000, 2009:2016)) {
        # runscript(script = download.cens.script, args = args)
      } else if (year == 1990) {
        #  runscript(script = read1990.script, args = args)
      } else {
        #  runscript(script = interp_script, args = args)
      }
      if (year %in% c(1990, 2000)) {
        #   runscript(script = cross.walk.script, args = args)
      }

      #  runscript(script = download.other.script, args = args)

      #  runscript(script=assignTract.script, args  = args)

      #    runscript(script = assignTractAKHI.script, args = args)
      #     runscript(script = rural.urban.script, args = args)
      #   runscript(script = cens_agr_script, args = args)

      #      runscript(script = paf.script, args = args)
      #         runscript(script = read.nvs.findrepl.script, args = args)
      #      runscript(script = read.total.burden.nvs.script, args = args)
      #    runscript(script=pop.summary.script, args = args)
      #   runscript(script=pop.summary.educ.script, args = args)

        runscript(script = add.rate.tot.burd, args = args)
      # runscript(script = calc.attr.burd1.script, args = args)
      #  runscript(script = calc.attr.burd2.script, args = args)
        runscript(script = calc.attr.burd3.script, args = args)
         print(paste(year, agr_by))
    }
  }
}

scenario <- "real"
method <- "di_gee"
args <- paste(
  tmp.dir, # 1
  dem.dir, # 2
  dem.agr.dir, # 3
  pop.summary.dir, # 4
  total.burden.parsed2.dir, # 5
  attr.burden.dir, # 6
  summary.dir = "data/15_summary", # 7
  figures.dir = "data/16_figures", # 8
  exp.rr.dir, # 9
  scenario, # 10
  method # 11
)

runscript(script = summary.script, args = args)
#runscript(script = summary.other.script, args = args)

#run all figures
#figure_scripts_list <- list.files(code_dir)
#figure_scripts_list <- file.path(code_dir, figure_scripts_list[grepl("figure", figure_scripts_list)])
#for(figure_script in figure_scripts_list) runscript(script = figure_script, args = args)