#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "truncnorm", "triangle",
  "matrixStats"
)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
censDir <- args[8]
dem_agrDir <- args[9]
agr_by <- args[10]
source <- args[14]
totalBurdenParsed2Dir <- args[17]
attr_burdenDir <- args[18]

if (rlang::is_empty(args)) {
  year <- 2010
  agr_by <- "nation"
  source <- "nvss"
  
  tmpDir <-  "data/tmp"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  pafDir <- "data/07_gbd_paf"
  totalBurdenParsed2Dir <-"data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
}

if (agr_by == "county") {
  quit()
}

attr_burdenDir <- file.path(attr_burdenDir, agr_by, source)
dir.create(attr_burdenDir, recursive = T, showWarnings = F)
attr_burdenDir <- file.path(attr_burdenDir, paste0("attr_burd_alt_", toString(year), ".csv"))
# http://web.stanford.edu/~mburke/papers/burke_et_al_wildfire_pnas_2021.pdf
# https://github.com/burke-lab/wildfire-map-public/blob/main/work/14_figure3.R

if (!file.exists(attr_burdenDir)) {
  tic(paste("calculated attributable burden alternative way", year, agr_by, source))
  #----read some data-----
  total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
    fread() 

  total_burden <- total_burden %>%
    dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education","rural_urban_class", "source", "measure1", "measure2", "label_cause"))) %>%
    summarise(
      value = sum(value),
      min_age = min(min_age),
      max_age = max(max_age)
    )

  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  files <- list.files(file.path(dem_agrDir, agr_by, year))
  pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
  pm_summ <- pm_summ %>% left_join(meta, by = "variable")
  pm_summ <- pm_summ %>% filter(min_age >= 25)
  pm_summ <- pm_summ %>% mutate(min_age = min(min_age), max_age = max(max_age))
  pm_summ <- pm_summ %>% filter(scenario == "real")
  
  pm_summ <- pm_summ %>% mutate_at(c("rural_urban_class","Education"), as.factor)
  total_burden <- total_burden %>% mutate_at(c("rural_urban_class","Education"), as.factor)
    
  pm_summ <- pm_summ %>%
    dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education","rural_urban_class","scenario", "pm", "min_age", "max_age"))) %>%
    dplyr::summarize(pop_size = sum(pop_size))

  rm(meta, files)
  ## --- summarize pm exposure ----
  pm_summ <- pm_summ %>%
    pivot_wider(
      names_from = pm,
      values_from = pop_size,
      values_fill = 0
    ) %>%
    as.data.frame()

  pm_summ_var <- pm_summ[, c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education","rural_urban_class", "min_age", "max_age", "scenario")]
  pm_summ_pop <- data.matrix(pm_summ[, !names(pm_summ) %in% c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "rural_urban_class","min_age", "max_age", "scenario")])
  pm_summ_pop <- t(scale(t(pm_summ_pop), center = FALSE, scale = rowSums(pm_summ_pop)))

  ## ---paf calculations----
  # TODO age?
  # 29 https://www.pnas.org/content/115/38/9592

  # burnett mortality for ncd+lri
  burnett_gemm <- function(X, theta, alpha, mu, v) {
    X <- pmax(0, X - 2.4)
    one <- log(1 + (X / alpha))
    two <- 1 / (1 + exp(-(X - mu) / v))
    Y <- exp(theta * one * two)
    return(Y)
  }

  #https://github.com/mszyszkowicz/DataGEMM
  #GEMM Calculator (PNAS)_ab.xlsx, GEMM fit parameters, se theta
  #compare with Fig S7 in Supporting information
  thetas <- c(0.1430 - 2 * 0.01807, 0.1430, 0.1430 + 2 * 0.01807)
  paf_burnett <- pm_summ_pop %*% outer(
    colnames(pm_summ_pop) %>% as.numeric(),
    thetas,
    function(pm, theta) {
      # burnett_gemm(pm, 0.1430, 1.6, 15.5, 36.8) - 1 #TODO
      1 - 1 / burnett_gemm(pm, theta, 1.6, 15.5, 36.8)
    }
  )

  paf_burnett <- cbind(pm_summ_var,
    lower = paf_burnett[,1],
    mean = paf_burnett[,2],
    upper = paf_burnett[,3],
    method = "burnett"
  )

  attr_burden_burnett <- inner_join(
    total_burden %>% dplyr::filter(label_cause == "ncd_lri"),
    paf_burnett,
    by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education","rural_urban_class")
    #by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "rural_urban_class","Education")
  )


  # 32 https://pubmed.ncbi.nlm.nih.gov/29962895/
  ## get the epa beta
  ## using the different parametric distributions in the EPA documentation
  set.seed(5)
  expa <- rtruncnorm(1000, a = 0, mean = 1.42, sd = 0.89)
  expc <- rtruncnorm(1000, a = 0, mean = 1.2, sd = 0.49)
  expd <- triangle::rtriangle(1000, 0.1, 1.6, 0.95)
  expe <- rtruncnorm(1000, a = 0, mean = 2, sd = 0.61)
  expg <- rtruncnorm(1000, a = 0, mean = 1, sd = 0.19)
  expi <- rtruncnorm(1000, a = 0, b = 2.273, mean = 1.25, sd = 0.53)
  expj <- rweibull(1000, 2.21, 1.41)

  betas <- c(expa, expc, expd, expe, expg, expi, expj) / 100

  paf_epa <- pm_summ_pop %*% outer(
    colnames(pm_summ_pop) %>% as.numeric(),
    betas,
    function(pm, beta) {
      1 - exp(-beta * pm) # probably the right one
      # (exp(beta * pm) - 1)
    }
  )

  paf_epa <- cbind(pm_summ_var,
    lower = matrixStats::rowQuantiles(paf_epa, probs = .025),
    mean = rowMeans(paf_epa),
    upper = matrixStats::rowQuantiles(paf_epa, probs = .975),
    method = "EPA"
  )

  attr_burden_epa <- inner_join(
    total_burden %>% dplyr::filter(label_cause == "all-cause"),
    paf_epa,
    by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "rural_urban_class","Education")
  )

  attr_burden <- rbind(attr_burden_epa, attr_burden_burnett) %>%
    mutate(
      mean = value * mean,
      lower = value * lower,
      upper = value * upper,
      attr = "attributable",
      value = NULL, label_cause = NULL,
      min_age = min(min_age.x, min_age.y),
      max_age = max(max_age.x, max_age.y),
      min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL
    )
  fwrite(attr_burden, attr_burdenDir)
  toc()
}
