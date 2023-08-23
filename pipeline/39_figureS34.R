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
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "MALDIquant",
  "dplyr", "tidyr", "matrixStats", "truncnorm", "triangle"#, "directlabels"
)
#library(directlabels)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
totalBurdenDir <- args[5]
summaryDir <- args[7]
figuresDir <- args[8]
exp_rrDir <- args[9]
min_ageI <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/17_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/18_figures"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/13_total_burden_rate"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  
  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/17_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/18_figures"
  totalBurdenDir <- "/g/huber/users/fridljand/R/HIGH/data/13_total_burden_rate"
  exp_rrDir <- "/g/huber/users/fridljand/R/HIGH/data/04_exp_rr"
  tmpDir <- "/g/huber/users/fridljand/R/HIGH/data/tmp"
  
  summaryDir <-"/Volumes/fridljand/R/HIGH/data/17_summary"
  figuresDir <- "/Volumes/fridljand/R/HIGH/data/18_figures"
  totalBurdenDir <- "/Volumes/fridljand/R/HIGH/data/13_total_burden_rate"
  exp_rrDir <- "/Volumes/fridljand/R/HIGH/data/04_exp_rr"
  tmpDir <-  "/Volumes/fridljand/R/HIGH/data/tmp"
  
  min_ageI <- 25
  exp_rrDir <- "data/04_exp_rr"
  totalBurdenDir <- "data/13_total_burden_rate"
  summaryDir <-"data/17_summary"
  figuresDir <- "data/18_figures"
  tmpDir <-  "data/tmp"
}
theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
#--- reading total burden data---
files <- list.files(file.path(totalBurdenDir, "nation", "nvss"))
total_burden <- lapply(files, function(file) {
  total_burden <- fread(file.path(totalBurdenDir, "nation", "nvss", file))
  total_burden <- total_burden %>%
    filter(Gender.Code == "A" & measure1 == "Deaths" & measure2 == "absolute number" & source == "nvss" &
      Education == 666 & Race == "All" & Hispanic.Origin == "All Origins" & rural_urban_class == 666) %>%
    mutate(
      age_group_id = c(0,seq(25, 95, 5))[
        findInterval(
          max_age,
          c(0,seq(25, 95, 5)),
          left.open =  F
        ) 
      ]
    ) %>%
    group_by(Year, label_cause, age_group_id) %>%
    dplyr::summarise(value = sum(value))
  return(total_burden)
}) %>% rbindlist()
rm(files)

total_burden <- total_burden %>% filter(min_age == min_ageI)
total_burden <- total_burden %>% filter(Year == 2016)
total_burden <- total_burden %>% mutate(value = value *100000/323100000) #Population in 2016
## --- GBD estimate----
gbd <- function(pms) {
  example_exp_rr <- file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv()
  pm_levels <- example_exp_rr$exposure_spline
  causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()

  attr_burd <- apply(causes_ages, 1, function(cause_age) {
    # subset rows of total burden
    label_causeX <- cause_age[["label_cause"]]
    age_group_idX <- cause_age[["age_group_id"]]
    total_burden <- total_burden %>% filter(label_cause == label_causeX)
    if (age_group_idX != "all ages") {
      total_burden <- total_burden %>% filter(age_group_id == as.numeric(cause_age[["age_group_id"]]))
    }
    total_burden <- total_burden %>%
      group_by(Year, label_cause) %>%
      dplyr::summarise(overall_value = sum(value))


    exp_rr <- ifelse(age_group_idX == "all ages",
      paste0(label_causeX, ".csv"),
      paste0(label_causeX, "_", age_group_idX, ".csv")
    ) %>%
      file.path(exp_rrDir, .) %>%
      fread()

    pm_matched <- sapply(pms, function(x) pm_levels[match.closest(x, pm_levels)])

    exp_rr <- as.matrix(exp_rr[, -1])
    rownames(exp_rr) <- pm_levels
    exp_rr <- exp_rr[as.character(pm_matched), ]
    exp_paf <- apply(exp_rr, 1:2, function(x) (x - 1) / x)
    exp_paf <- data.frame(pm = row.names(exp_paf) %>% as.numeric(), exp_paf)
    exp_paf <- exp_paf %>%
      pivot_longer(
        cols = colnames(exp_paf) %>% grep("draw", ., value = TRUE),
        names_to = "draw",
        values_to = "paf"
      )

    attr_burd <- merge(total_burden, exp_paf) %>%
      mutate(attr = paf * overall_value)
    return(attr_burd)
  }) %>% rbindlist()

  attr_burd <- attr_burd %>%
    group_by(Year, pm, draw) %>%
    summarise(attr = sum(attr)) %>%
    dplyr::group_by(Year, pm) %>%
    dplyr::summarise(
      mean = mean(attr),
      lower = quantile(attr, p = .025),
      upper = quantile(attr, p = .975),
      method = "GBD"
    )
  return(attr_burd)
}

# gbd_curve <- gbd(X)

## ---burnett estimate----
# 29 https://www.pnas.org/content/115/38/9592
burnett <- function(pms) {
  total_burden <- total_burden %>%
    dplyr::filter(label_cause == "ncd_lri") %>%
    group_by(Year) %>%
    dplyr::summarise(overall_value = sum(value))
  # burnett mortality for ncd+lri
  burnett_gemm <- function(X, theta, alpha, mu, v) {
    X <- pmax(0, X - 2.4)
    one <- log(1 + (X / alpha))
    two <- 1 / (1 + exp(-(X - mu) / v))
    Y <- exp(theta * one * two)
    return(Y)
  }

  thetas <- c(0.1430 - 2 * 0.01807, 0.1430, 0.1430 + 2 * 0.01807)
  paf_burnett <- outer(
    pms,
    thetas,
    function(pm, theta) {
      1 - 1 / burnett_gemm(pm, theta, 1.6, 15.5, 36.8)
    }
  )
  paf_burnett <- data.frame(
    pm = pms,
    lower = paf_burnett[, 1],
    mean = paf_burnett[, 2],
    upper = paf_burnett[, 3]
  )

  attr_burd <- merge(total_burden, paf_burnett) %>%
    mutate(
      mean = overall_value * mean,
      lower = overall_value * lower,
      upper = overall_value * upper,
      overall_value = NULL,
      method = "GEMM"
    )
  return(attr_burd)
}

## ---di estimate----
di <- function(pms) {
  total_burden <- total_burden %>%
    dplyr::filter(label_cause == "all-cause") %>%
    group_by(Year) %>%
    dplyr::summarise(overall_value = sum(value))
  
  rates <- c(0.0071, 0.0073, 0.0075)
  paf_di <- outer(
    pms,
    rates,
    function(pm, rate) {
      pmax(0, pm-5) * rate
    }
  )
  paf_di <- data.frame(
    pm = pms,
    lower = paf_di[, 1],
    mean = paf_di[, 2],
    upper = paf_di[, 3]
  )
  
  attr_burd <- merge(total_burden, paf_di) %>%
    mutate(
      mean = overall_value * mean,
      lower = overall_value * lower,
      upper = overall_value * upper,
      overall_value = NULL,
      method = "Di"
    )
  return(attr_burd)
}

### ----epa----
epa <- function(pms) {
  total_burden <- total_burden %>%
    dplyr::filter(label_cause == "all-cause") %>%
    group_by(Year) %>%
    dplyr::summarise(overall_value = sum(value))

  expa <- rtruncnorm(1000, a = 0, mean = 1.42, sd = 0.89)
  expc <- rtruncnorm(1000, a = 0, mean = 1.2, sd = 0.49)
  expd <- triangle::rtriangle(1000, 0.1, 1.6, 0.95)
  expe <- rtruncnorm(1000, a = 0, mean = 2, sd = 0.61)
  expg <- rtruncnorm(1000, a = 0, mean = 1, sd = 0.19)
  expi <- rtruncnorm(1000, a = 0, b = 2.273, mean = 1.25, sd = 0.53)
  expj <- rweibull(1000, 2.21, 1.41)

  betas <- c(expa, expc, expd, expe, expg, expi, expj) / 100

  paf_epa <- outer(
    pms,
    betas,
    function(pm, beta) {
      1 - exp(-beta * pm) # probably the right one
    }
  )
  paf_epa <- data.frame(
    pm = pms,
    lower = matrixStats::rowQuantiles(paf_epa, probs = .025),
    mean = rowMeans(paf_epa),
    upper = matrixStats::rowQuantiles(paf_epa, probs =.975)
  )

  attr_burd <- merge(total_burden, paf_epa) %>%
    mutate(
      mean = overall_value * mean,
      lower = overall_value * lower,
      upper = overall_value * upper,
      overall_value = NULL,
      method = "EPA"
    )
  return(attr_burd)
}
### ---plot---
pms <- seq(0, 30, by = 0.5)
data <- rbind(
  gbd(pms),
  burnett(pms),
  #epa(pms),
  di(pms)
)

#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines
g1 <- ggplot(data, aes(x = pm, y = mean, color = method)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)+
  #geom_dl(aes(label = method), method = list(dl.combine("last.points")), cex = 0.8) 
  #geom_text(aes(label = method), position = "dodge")
  #geom_text("a")
  theme(
    legend.title = element_blank(),
    legend.position = c(0.3, 0.88),
    legend.text = element_text(size = 14),
    legend.background = element_rect(fill = "transparent")
  ) +
  xlab("Annual average PM2.5 in μg/m^3") +
  ylab("Mortality from PM2.5 per 100,000 per year") 
g1
#
g2 <- ggplot(data, aes(x = pm, y = mean, color = method)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.2, show.legend = FALSE)+
  annotate("text", x = 26, y = 123, label = "Burnett", size = 8, colour = hue_pal()(3)[1])+
  annotate("text", x = 26, y = 255, label = "EPA", size = 8, colour = hue_pal()(3)[2])+
  #annotate("text", x = 22, y = 80, label = "DI", size = 8, colour = hue_pal()(3)[2])+
  annotate("text", x = 26, y = 55, label = "GBD", size = 8, colour = hue_pal()(3)[3])+
  xlab("Annual average PM2.5 in μg/m^3") +
  ylab("Mortality from PM2.5 per 100,000 per year")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=20))
g2
ggsave(file.path(figuresDir, "figureS6.png"),plot = g2, dpi = 300, g2, height = 9, width = 8)


g3 <- ggplot(data, aes(x = pm, y = mean, color = method)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)+
  xlab("Annual average PM2.5 in μg/m^3") +
  ylab("Mortality from PM2.5 per 100,000 per year") + 
  geom_text(data = subset(data, pm == max(pms)), aes(label = method, colour = method, x = Inf, y = mean), hjust = -.1,  size=6) + #vjust=-.5,
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,5,1,1), "lines")) +
  theme(text = element_text(size=15))

# Code to turn off clipping
gt <- ggplotGrob(g3)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
#gt <- grid.draw(gt)
ggsave(file.path(figuresDir,  "figureS34.png"), dpi = 300, gt, height = 9, width = 8)
