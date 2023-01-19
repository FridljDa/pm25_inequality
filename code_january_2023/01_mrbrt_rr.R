#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/03/2020
# Purpose: calculate RR from MR-BRT
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))
set.seed(0)
# load packages, install if missing

library(magrittr)
library(MALDIquant)
library(ggplot2)
library(dplyr)
library(tictoc)
library(tidyr)
library(Rmisc)

options(tidyverse.quiet = TRUE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)
tmpDir <- args[1]
exp_rrDir <- args[2]

plotsDir <- file.path(exp_rrDir, "plots")
dir.create(plotsDir, recursive = T, showWarnings = F)

mrbrtDir <- file.path(exp_rrDir, "mrbrt")

n<-30 #number of effect samples, <=1000
m<-30 #number of TMREL samples, <= 1000
## --------either load data or write it---------
# write useful overview over causes
causes_agesDir <- file.path(tmpDir, "causes_ages.csv")

if (file.exists(causes_agesDir)) {
  causes_ages <- read.csv(causes_agesDir)
} else {
  # Chronic obstructive pulmonary disease, ? ,lower respiratory infections, ?, type 2 diabetes
  causes_all_ages <- c("resp_copd", "lri", "neo_lung", "t2_dm")
  causes_age_specific <- c("cvd_ihd", "cvd_stroke")

  age_ids <- seq.int(25, 95, 5)

  causes_ages <- data.frame(
    label_cause = rep(causes_age_specific, each = length(age_ids)),
    age_group_id = rep(age_ids, times = length(causes_age_specific))
  )

  causes_ages <- data.frame(
    label_cause = causes_all_ages,
    age_group_id = rep("all ages", each = length(causes_all_ages))
  ) %>%
    rbind(causes_ages)

  write.csv(causes_ages, causes_agesDir, row.names = FALSE)
}

# tmrels <- runif(1000, min = 2.4, max = 5.9)
tmrels <- file.path(mrbrtDir, "tmrel_draws.csv") %>%
  read.csv() %>%
  unlist() 

tmrels <- tmrels[1:m]
## ----------calculation---------

tic("Calculated RR from MR-BRT for all causes")
apply(causes_ages, 1, function(cause_age) {
  label_cause <- cause_age[1]
  age_group_id <- cause_age[2]

  file_name <- ifelse(age_group_id == "all ages",
    paste0(label_cause, ".csv"),
    paste0(label_cause, "_", age_group_id, ".csv")
  )

  exp_rrDirX <- file.path(exp_rrDir, file_name)

  if (!file.exists(exp_rrDirX)) {
    tic(paste("Calculated RR from MR-BRT for", label_cause, "age group:", age_group_id))

    mrbrtDirX <- file.path(mrbrtDir, file_name)
    exp_mrbrt <- read.csv(mrbrtDirX) 
    exposure_spline <- exp_mrbrt$exposure_spline
    exp_mrbrt <- exp_mrbrt[, paste0("draw_",(0:(n-1)))]
    
    ## --interpolate mrbrt data
    # X values of points to interpolate from known data
    aim <- exposure_spline[exposure_spline < 30]
    aim <- c(aim, seq(10, 30, 0.1)) %>%
      unique() %>%
      sort()

    exp_mrbrt_interp <- apply(exp_mrbrt, 2, function(col){
      interp <- approx(exposure_spline,
                       col,
                       xout = aim
      )
      return(interp$y)
    })
    
    exp_mrbrt_interp <- as.matrix(exp_mrbrt_interp)
    rownames(exp_mrbrt_interp) <-aim
    #exp_mrbrt_interp <- cbind(label = label_cause,
    #                          exposure_spline = aim,
    #                          exp_mrbrt_interp)

    ## --calculate RR
    exp_rr_interp <- lapply(tmrels, function(tmrel){
      tmrel_pos <- match.closest(tmrel, aim)
      exp_rr_interp<- apply(exp_mrbrt_interp, 2, function(col){
        tmrel_mrbrt <- col[tmrel_pos]
        col <- col/tmrel_mrbrt
      })
      
      exp_rr_interp <- apply(exp_rr_interp, 1:2, function(x) max(x,1))
    })
    
    exp_rr_interp <- do.call(cbind, exp_rr_interp) %>% as.data.frame
    
    exp_rr_interp <- tibble::rownames_to_column(exp_rr_interp, "exposure_spline")
    write.csv(exp_rr_interp, exp_rrDirX, row.names = FALSE)
    toc()
  }

  exp_rr <- read.csv(exp_rrDirX)
  plotDirX <- file.path(plotsDir, paste0(label_cause, "_", age_group_id, ".png"))

  #https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals
  if (!file.exists(plotDirX) && TRUE) {
    exp_rr_ci <- apply(exp_rr, 1, function(row){
      val <- row[-1]
      return(c(row[1], 
               mean = mean(val),
               lower = (quantile(val,p=.025) %>% unname),
               upper = (quantile(val,p=.975))%>% unname))
    })
    exp_rr_ci <- t(exp_rr_ci)
    exp_rr_ci <- as.data.frame(exp_rr_ci)
    exp_rr_ci$value <- "rr"
    
    exp_mrbrt <- read.csv(file.path(mrbrtDir, file_name)) %>%
      select(exposure_spline, mean, lower, upper) %>%
      filter(exposure_spline <= 30)
    exp_mrbrt$value <- "MR-BRT"
    
    
    g<-ggplot(data=rbind(exp_rr_ci, exp_mrbrt), 
              aes(x=exposure_spline, y=mean, color = value)) + 
          geom_point() + 
          geom_line()+
      geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.15) #, color = "red"
    g
    ggsave(plotDirX)
  }
  
  NA
})
toc()
NA
