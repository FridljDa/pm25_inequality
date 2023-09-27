#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
#rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat",  "readxl", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

dataDir <- args[2]
tmpDir <- args[3]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  tmpDir <- "data/tmp"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  dataDir <- "data"
}

####---- create causes find and replace data.frame
causesDir <- file.path(totalBurdenParsedDir, "causes.csv")
if (file.exists(causesDir)) quit()

# https://www150.statcan.gc.ca/n1/pub/82-003-x/2013007/article/11852/tbl/appb-eng.htm
#----for years 1990 to 1998 ----
# Create separate data frames for each "to" value
#Ischemic heart disease
df_cvd_ihd <- data.frame(
  replacecolumns = "label_cause",
  from = 414, #410:414
  to = "cvd_ihd"
)

df_cvd_stroke <- data.frame(
  replacecolumns = "label_cause",
  from = c(434, 432, 430),
  to = "cvd_stroke"
)

df_neo_lung <- data.frame(
  replacecolumns = "label_cause",
  from = c(161, 162),
  to = "neo_lung"
)

df_resp_copd <- data.frame(
  replacecolumns = "label_cause",
  from = c(490:496),
  to = "resp_copd"
)

df_lri <- data.frame(
  replacecolumns = "label_cause",
  from = 519,
  to = "lri"
)

df_t2_dm <- data.frame(
  replacecolumns = "label_cause",
  from = 250,
  to = "t2_dm"
)

df_ncd_lri <- data.frame(
  replacecolumns = "label_cause",
  from = c(140:242, 244:259, 270:279, 282:285, 296:319, 324:380, 383:459, 470:478, 490:611, 617:629, 680:759, 519),
  to = "ncd_lri"
)

# Row bind all the data frames together
causes_1990_to_1998 <- rbind(df_cvd_ihd, df_cvd_stroke, df_neo_lung, df_resp_copd, df_lri, df_t2_dm, df_ncd_lri)
rm(df_cvd_ihd, df_cvd_stroke, df_neo_lung, df_resp_copd, df_lri, df_t2_dm, df_ncd_lri)

causes_1990_to_1998 <- merge(data.frame(Year = 1990:1998), causes_1990_to_1998)

#----- for 1999 to 2016-----
# Data frame for cvd_ihd
df_cvd_ihd <- data.frame(
  replacecolumns = "label_cause",
  from = c("I20", "I21", "I22", "I23", "I24", "I25"),
  to = "cvd_ihd"
)

# Data frame for cvd_stroke
df_cvd_stroke <- data.frame(
  replacecolumns = "label_cause",
  from = c("G45", "G46", "I61", "I62", "I63", "I65", "I66", "I67", "I68", "I69"),
  to = "cvd_stroke"
)

# Data frame for neo_lung
df_neo_lung <- data.frame(
  replacecolumns = "label_cause",
  from = c("C33", "C34", "D02", "D14", "D38"),
  to = "neo_lung"
)

# Data frame for resp_copd
df_resp_copd <- data.frame(
  replacecolumns = "label_cause",
  from = c("J41", "J42", "J43", "J44"),
  to = "resp_copd"
)

# Data frame for lri
df_lri <- data.frame(
  replacecolumns = "label_cause",
  from = c("A48", "A70", "B97", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J20", "J21", "P23", "U04"),
  to = "lri"
)

# Data frame for t2_dm
df_t2_dm <- data.frame(
  replacecolumns = "label_cause",
  from = "E11",
  to = "t2_dm"
)

# Data frame for ncd_lri
df_ncd_lri <- data.frame(
  replacecolumns = "label_cause",
  from = c(
    sprintf("C%02d", 0:97), sprintf("D%02d", 0:48), sprintf("D%02d", 55:89),
    sprintf("E%02d", 3:7), sprintf("E%02d", 10:16), sprintf("E%02d", 20:34),
    sprintf("E%02d", 65:88), sprintf("F%02d", 1:99), sprintf("F%02d", 1:99),
    sprintf("G%02d", 6:98), sprintf("H%02d", 0:61), sprintf("H%02d", 68:93),
    sprintf("I%02d", 0:99), sprintf("J%02d", 30:98), sprintf("K%02d", 0:92),
    sprintf("N%02d", 0:64), sprintf("N%02d", 75:98), sprintf("L%02d", 0:98),
    sprintf("M%02d", 0:99), sprintf("Q%02d", 0:99),
    # lower respitory infections
    sprintf("J%02d", 9:22)
  ),
  to = "ncd_lri"
)

# Access the data frames using their names, e.g., causes_list$cvd_ihd
causes_1999_to_2016 <- rbind(df_cvd_ihd, df_cvd_stroke, df_neo_lung, df_resp_copd, df_lri, df_t2_dm, df_ncd_lri)
rm(df_cvd_ihd, df_cvd_stroke, df_neo_lung, df_resp_copd, df_lri, df_t2_dm, df_ncd_lri)

causes_1999_to_2016 <- merge(data.frame(Year = 1999:2016), causes_1999_to_2016)
causes <- rbind(causes_1990_to_1998, causes_1999_to_2016)
write.csv(causes, causesDir, row.names = FALSE)
