<!-- badges: start -->
  [![R-CMD-check](https://github.com/FridljDa/pm25_inequality/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FridljDa/pm25_inequality/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# PM2.5-attributable-mortality-analysis

Repo supporting ["TODO publication"](https://www-pnas-org.stanford.idm.oclc.org/content/118/2/e2011048118).

# Using results

The main estimates generated in the paper are available at `data/15_summary`. T

# Data

Many of the input data are automatically downloaded in the script. Where it was not possible, directions for accessing the raw data are included along with the download links provided below.

## Included in repo

* data/crosswalk_[YYYY]_[YYYY].csv: cross-walk files for census boundaries from the [Longitudinal Tract Database](https://s4.ad.brown.edu/Projects/Diversity/researcher/bridging.htm).
* data/NCHSURCodes2013.xlsx classification for urbanisation level on county level from [here](https://www.cdc.gov/nchs/data_access/urban_rural.htm)
* data/rural_urban_class.csv urbanisation classification from NCHSURCodes2013.xlsx crosswalked to county boundary versions of 1990, 2000, 2010 using crosswalk_[YYYY]_[YYYY].csv. The classes 1,2 were aggregated into one, similarly 3,4 and 5,6.
* data/08_svi county level look up table for social-vulnerability level from [here](https://www.atsdr.cdc.gov/placeandhealth/svi/index.html).
* data/standartpopulation.xlsx manually typed from "Table VIII. United States standard population" in the [National Vital Statistics Reports Volume 57, Number 14](https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf)
* data/nhgis0002_ds120_1990_tract.csv population data at census tract level for 1990 [Ipums](https://data2.nhgis.org/) (Year: 1990; Geographic level: Census Tract (by State--County); Dataset: 1990 Census: STF 1 - 100% Data; NHGIS code: 1990_STF1; NHGIS ID: ds120; Tables: Race by Sex by Age, Universe: Persons, Source code: NP12, NHGIS code:  ET4)    
* IHME_GBD_2019_TMRLT_Y2021M01D05.csv GBD Reference Life Table used for optional calculation of Years of Life Lost
* ihme_fips.rda from [here](https://github.com/mkiang/narcan/blob/master/data/ihme_fips.rda.)
* data/04_exp_rr calculated from MR-BRT files.

* data/05_demog/meta/cens_meta_[YYYY].csv TODO
* data/05_demog/meta_down/cens_meta_[YYYY].csv TODO and translated via cross_bridge/cross_meta files
* data/05_demog/cross_bridge/cross_meta_[YYYY].csv TODO
* data/05_demog/[YYYY] census tract level population counts of year [YYYY]
* data/08_total_burden/mort[YYYY].csv restricted use mortality counts were obtained from US National Vital Statistics System (NVSS) covering all deaths occurring within the United States. The usage of the restricted-use data set for this study was approved by the Division of Vital Statistics at the Center for Disease Control and Prevention. Not shared due to privacy concerns and data-usage restrictions.

* data/09_total_burden_parsed/causes.csv look up table for causes and ICD codes considered in this study.
* data/09_total_burden_parsed/findreplace.csv TODO
* data/09_total_burden_parsed/[county/nation/STATEFP] TODO
* data/10_cdc_population/[nation/STATEFP] TODO
* data/11_ethn_educ_population population counts on national level of the Population 18 Years and Over, by Age, Sex, Race, and Hispanic Origin for the years TODO manually downloaded from [census](https://www2.census.gov/programs-surveys/demo/tables/educational-attainment/) Educational Attainment
* data/12_population_summary parsed and summarized population counts based on data/05_demog, data/10_cdc_population, data/11_ethn_educ_population
* data/13_total_burden_parsed2 TODO mortality rates based on data/08_total_burden and data/12_population_summary
* data/14_attr_burden TODO based on data/06_dem.agr, data/13_total_burden_parsed2,
* data/15_summary high-level summary of TODO
* data/17_summary
* data/18_figures

## Not included in repo
* data/01_exposure/[YYYY].h5 PM2.5 exposure estimates for contiguous US sourced from [here](ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/). Too large to include on Github.
* data/01_exposure/epa PM2.5 exposure measurements for Alaska and Hawaii sourced from [here](https://aqs.epa.gov/aqsweb/airdata/download_files.html). Too large to include on Github.  
* data/02_tract tract_shape files in .rds sourced using directly [tigris](https://cran.r-project.org/web/packages/tigris/index.html) (1990) or inderictly [tidycensus::get_decennial](https://walker-data.com/tidycensus/reference/get_decennial.html) (2000:2008, 2010) or [tidycensus::get_acs](https://github.com/walkerke/tidycensus/blob/master/man/get_acs.Rd) (2009,2011:2016). Too large to include on Github.   
* data/03_exp_tracts csv tables with census tract geographic identifier and assigned PM2.5 exposure based on data/01_exposure and data/02_tract. Too large to include on Github.
* data/04_exp_rr/mrbrt MR-BRT files for GBD-CRF (see [1](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30752-2/fulltext), [2](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30752-2/fulltext#supplementaryMaterial)) privately shared by IHME team (not primary CRF in main analysis). Private, so not shared on Github.
* data/05_demog/[YYYY]_in_[ZZZZ] census tract level population counts of year [YYYY] translated into census tracts of year [ZZZZ] using data/crosswalk_[YYYY]_[ZZZZ].csv. Too large to include on Github.
* data/06_dem.agr population counts by exposure level and county. Too large to include on Github.
* data/07_gbd_paf TODO

* data/15_sum_higher_geog_level
* data/16_prop_of_attr_burd
* data/16_sum_higher_geog_level_total

* data/tmp

### Necessary starting data folder structure to reproduce

```
data
├── 01_exposure
│   ├── 1990.h5
│   ├── 1991.h5
│   ├── 1992.h5
│   ├── 1993.h5
│   ├── ...
│   └── epa
│       ├── annual_conc_by_monitor_1990.csv
│       ├── annual_conc_by_monitor_1991.csv
│       ├── annual_conc_by_monitor_1992.csv
│       ├── ...
├── 02_tracts
│   ├── 1990
│   │   ├── tracts_1990_AK.rds
│   │   ├── tracts_1990_AL.rds
│   │   ├── ...
│   ├── 1991
│   └── ...
├── 03_exp_tracts
│   ├── 1990
│   │   ├── exp_trac_1990_AK.csv
│   │   ├── exp_trac_1990_AL.csv
│   │   ├── exp_trac_1990_AR.csv
│   │   ├── ...
│   ├── 1991
│   │   ├── exp_trac_2013_AK.csv
│   │   ├── exp_trac_2013_AL.csv
│   │   ├── exp_trac_2013_AR.csv
│   └── epa_tmp
│       ├── trac_loc_1990_AK.csv
│       ├── trac_loc_1990_HI.csv
│       ├── ...
├── 04_exp_rr
│   ├── cvd_ihd_25.csv
│   ├── cvd_ihd_30.csv
│   ├── cvd_ihd_35.csv
│   ├── cvd_ihd_40.csv
│   ├── ...
│   ├── lri.csv
│   ├── mrbrt
│   │   ├── bw.csv
│   │   ├── cvd_ihd_25.csv
│   │   ├── cvd_ihd_30.csv
│   │   ├── cvd_ihd_35.csv
│   │   ├── ...
├── 05_demog
│   ├── 1990
│   │   ├── census_1990_AK.csv
│   │   ├── census_1990_AL.csv
│   │   ├── census_1990_AR.csv
│   ├── 1991
│   ├── cross_bridge
│   │   ├── cross_meta_1990.csv
│   │   ├── cross_meta_2000.csv
│   │   ├── cross_meta_2009.csv
│   │   ├── ...
│   ├── meta
│   │   ├── cens_meta_1990.csv
│   │   ├── cens_meta_1991.csv
│   │   ├── cens_meta_1992.csv
│   │   ├── ...
│   └── meta_down
│       ├── cens_meta_1990.csv
│       ├── cens_meta_1991.csv
│       ├── cens_meta_1992.csv
│       ├── ...
├── 06_dem.agr
│   ├── county
│   │   └── 1990
│   │       ├── cens_agr_1990_AK.csv
│   │       ├── cens_agr_1990_AL.csv
│   │       ├── cens_agr_1990_AR.csv
│   │       ├── ...
├── 07_gbd_paf
│   ├── STATEFP
│   │   ├── 1990
│   │   │   ├── paf_1990_1.csv
│   │   │   ├── paf_1990_10.csv
│   │   │   ├── paf_1990_11.csv
│   │   │   ├── ...
├── 08_svi
│   ├── SVI_2000_US_county.csv
│   ├── SVI_2010_US_county.csv
│   ├── SVI_2014_US_county.csv
│   ├── SVI_2016_US_county.csv
│   ├── SVI_2018_US_county.csv
│   ├── SVI_2020_US_county.csv
│   └── svi_lookup_table.csv
├── 09_total_burden_parsed
│   ├── STATEFP
│   │   └── nvss
│   │       ├── total_burden_nvss_1990.csv
│   │       ├── total_burden_nvss_1991.csv
│   │       ├── total_burden_nvss_1992.csv
│   │       ├── ...
│   └── findreplace.csv
├── 10_cdc_population
│   ├── STATEFP
│   │   ├── Bridged-Race Population Estimates 1990-2016 (1).txt
│   │   ├── Bridged-Race Population Estimates 1990-2016 (10).txt
│   │   ├── Bridged-Race Population Estimates 1990-2016 (11).txt
│   │   ├── ...
│   └── nation
│       ├── Bridged-Race Population Estimates 1990-2016 (1).txt
│       ├── Bridged-Race Population Estimates 1990-2016 (2).txt
│       ├── ...
├── 11_ethn_educ_population
│   ├── table-1-02 (1).xlsx
│   ├── table-1-02 (2).xlsx
│   ├── table-1-02.xlsx
│   ├── ...
├── 12_population_summary
│   ├── nation
│   │   ├── pop_sum_1990.csv
│   │   ├── pop_sum_1991.csv
│   │   ├── pop_sum_1992.csv
│   │   ├── ...
│   ├── pop_cdc_nation.csv
│   ├── pop_nation.csv
│   └── pop_race_educ_nation.csv
├── 14_attr_burd
│   └── county
│       └── nvss
│           ├── attr_burd_di_1990.csv
│           ├── attr_burd_di_1991.csv
│           ├── attr_burd_di_1992.csv
│           ├── attr_burd_di_1993.csv
│           ├── ...
├── 15_sum_higher_geog_level
│   ├── STATEFP
│   │   ├── attr_burden_age_adjusted_1991.csv
│   │   ├── attr_burden_age_adjusted_1992.csv
│   │   ├── attr_burden_age_adjusted_1993.csv
│   │   ├── ...
│   ├── county
│   │   ├── attr_burden_age_adjusted_1990.csv
│   │   ├── attr_burden_age_adjusted_1991.csv
│   │   ├── attr_burden_age_adjusted_1992.csv
│   │   ├── ...
│   └── nation
│       ├── attr_burden_age_adjusted_1991.csv
│       ├── attr_burden_age_adjusted_1992.csv
│       ├── attr_burden_age_adjusted_1993.csv
│       ├── ...
├── 16_prop_of_attr_burd
│   └── nation
│       ├── attr_burden_prop_1992.csv
│       ├── attr_burden_prop_1993.csv
│       ├── attr_burden_prop_1994.csv
│       ├── ...
├── 16_sum_higher_geog_level_total
│   ├── STATEFP
│   │   ├── total_burden_age_adjusted_1990.csv
│   │   ├── total_burden_age_adjusted_1991.csv
│   │   ├── total_burden_age_adjusted_1992.csv
│   │   ├── ...
│   ├── county
│   │   ├── total_burden_age_adjusted_1990.csv
│   │   ├── total_burden_age_adjusted_1991.csv
│   │   ├── total_burden_age_adjusted_1992.csv
│   │   ├── total_burden_age_adjusted_1993.csv
│   │   ├── ...
│   └── nation
│       ├── total_burden_age_adjusted_1990.csv
│       ├── total_burden_age_adjusted_1991.csv
│       ├── total_burden_age_adjusted_1992.csv
│       ├── ...
├── 17_summary
│   ├── all_burd.csv
│   ├── attr_burd_STATEFP_1.csv
│   ├── attr_burd_STATEFP_2.csv
│   ├── attr_burd_STATEFP_5.csv
│   ├── attr_burd_STATEFP_6.csv
│   ├── attr_burd_nation_1.csv
│   ├── attr_burd_nation_2.csv
│   ├── attr_burd_nation_3.csv
│   ├── attr_burd_nation_4.csv
│   ├── attr_burd_nation_7.csv
│   ├── attr_burd_nation_8.csv
│   ├── county
│   │   ├── all_burd.csv
│   │   ├── attr_burd_1.csv
│   │   ├── attr_burd_2.csv
│   │   ├── attr_burd_3.csv
│   │   ├── attr_burd_4.csv
│   │   ├── attr_burd_5.csv
│   │   ├── attr_burd_6.csv
│   │   ├── attr_burd_7.csv
│   │   └── attr_burd_8.csv
│   └── pm_summary.csv
├── 18_figures
│   ├── di_gee-real-25
│   │   ├── figure1.pdf
│   │   ├── figure1.png
│   │   ├── figure2.pdf
│   │   ├── ...
├── IHME_GBD_2019_TMRLT_Y2021M01D05.csv
├── NCHSURCodes2013.xlsx
├── crosswalk_1990_2010.csv
├── crosswalk_2000_2010.csv
├── final_findreplace.csv
├── ihme_fips.rda
├── rural_urban_class.csv
├── standartpopulation.xlsx
└── tmp
    ├── causes_ages.csv
    ├── counties_2000.RData
    ├── counties_2009.RData
    ├── ...
```
# R Packages needed

R packages required for replications are:
- vctrs
- bit64
- censusapi
- cdcfluview
- data.table
- dplyr
- ggplot2
- magrittr
- matrixStats
- MALDIquant
- RCurl
- readxl
- triangle
- sf
- sp
- stringr
- testthat
- tictoc
- truncnorm
- tidyverse
- viridis
- hrbrthemes
- rlang
- stats
- xlsx
- ggpubr
- ggExtra
- tigris
- tmap
- BiocManager
- rhdf5


Users can run the following one-off command to install the most recent versions of these packages:
```
install.packages(c("vctrs", "bit64", "cdcfluview", "censusapi", "data.table", "dplyr", "ggplot2", "magrittr", "matrixStats", "MALDIquant", "RCurl", "readxl", "triangle", "sf", "sp", "stringr", "testthat", "tictoc", "truncnorm",  "viridis", "hrbrthemes", "rlang", "stats", "xlsx", "ggpubr", "ggExtra", "tigris", "tmap", "BiocManager"), dependencies = T)
BiocManager::install("rhdf5")
```

## sessionInfo()

```
R version 4.1.2 (2021-11-01)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.5.2

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] BiocManager_1.30.16 compiler_4.1.2      tools_4.1.2        
[4] rstudioapi_0.13    

R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.2

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] splines   datasets  stats     graphics  grDevices utils     methods   base     

other attached packages:
 [1] RSelenium_1.7.5   BAMMtools_2.1.7   ape_5.3           Hmisc_4.3-0       ggplot2_3.3.2     Formula_1.2-3     survival_3.1-8   
 [8] lattice_0.20-38   imputeTS_3.0      signal_0.7-6      openxlsx_4.1.4    velox_0.2.0       gdata_2.18.0      raster_3.3-13    
[15] readr_1.3.1       stringr_1.4.0     tidyr_1.1.2       dplyr_1.0.2       ncdf4_1.17        geosphere_1.5-10  rgdal_1.5-18     
[22] rgeos_0.5-2       sp_1.4-4          sf_0.8-0          data.table_1.13.2

loaded via a namespace (and not attached):
  [1] backports_1.2.0     plyr_1.8.5          usethis_1.5.1       digest_0.6.27       htmltools_0.4.0     fansi_0.4.1        
  [7] magrittr_1.5        checkmate_1.9.4     memoise_1.1.0       cluster_2.1.0       remotes_2.1.0       xts_0.11-2         
 [13] askpass_1.1         forecast_8.10       tseries_0.10-47     prettyunits_1.1.1   colorspace_1.4-1    rvest_0.3.5        
 [19] pan_1.6             xfun_0.11           callr_3.5.1         crayon_1.3.4        jsonlite_1.7.1      lme4_1.1-25        
 [25] zoo_1.8-6           glue_1.4.2          gtable_0.3.0        pkgbuild_1.1.0      weights_1.0         semver_0.2.0       
 [31] quantmod_0.4-15     jomo_2.6-10         scales_1.1.1        stinepack_1.4       DBI_1.1.0           ggthemes_4.2.0     
 [37] Rcpp_1.0.5          htmlTable_1.13.3    units_0.6-5         foreign_0.8-72      htmlwidgets_1.5.1   httr_1.4.1         
 [43] gplots_3.0.1.1      RColorBrewer_1.1-2  acepack_1.4.1       ellipsis_0.3.1      mice_3.6.0          pkgconfig_2.0.3    
 [49] XML_3.98-1.20       nnet_7.3-12         tidyselect_1.1.0    rlang_0.4.8         munsell_0.5.0       tools_3.6.1        
 [55] cli_2.1.0           generics_0.1.0      audio_0.1-7         devtools_2.2.1      broom_0.7.2         yaml_2.2.0         
 [61] binman_0.1.1        processx_3.4.4      knitr_1.26          fs_1.3.1            zip_2.0.4           caTools_1.17.1.3   
 [67] purrr_0.3.4         mitml_0.3-7         nlme_3.1-142        xml2_1.2.2          compiler_3.6.1      rstudioapi_0.11    
 [73] curl_4.3            e1071_1.7-3         testthat_3.0.0      tibble_3.0.4        statmod_1.4.35      stringi_1.5.3      
 [79] ps_1.4.0            desc_1.2.0          Matrix_1.2-18       classInt_0.4-2      nloptr_1.2.2.2      urca_1.3-0         
 [85] vctrs_0.3.4         pillar_1.4.6        lifecycle_0.2.0     lmtest_0.9-37       bitops_1.0-6        wdman_0.2.4        
 [91] R6_2.5.0            latticeExtra_0.6-28 KernSmooth_2.23-16  gridExtra_2.3       sessioninfo_1.1.1   codetools_0.2-16   
 [97] boot_1.3-23         MASS_7.3-51.4       gtools_3.8.1        assertthat_0.2.1    pkgload_1.1.0       openssl_1.4.1      
[103] rprojroot_1.3-2     withr_2.3.0         fracdiff_1.5-1      parallel_3.6.1      hms_0.5.2           quadprog_1.5-8     
[109] grid_3.6.1          rpart_4.1-15        timeDate_3043.102   class_7.3-15        minqa_1.2.4         TTR_0.23-5         
[115] base64enc_0.1-3
```
