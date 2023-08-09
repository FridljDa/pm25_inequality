#fips code handling


if(FALSE){

  # Read rural and urban classification data
  df <- suppressMessages(
    read_excel(file.path(dataDir, "NCHSURCodes2013.xlsx"), .name_repair = "universal") %>%
      transmute(FIPS.code, rural_urban_class = ..2013.code)
  )

  #---- Read population summary data for 2010-----
  meta <- file.path(censDir, "meta", paste0("cens_meta_", 2010, ".csv")) %>%
    fread() %>%
    filter(Gender.Code == "A" & Race == "All" & Hispanic.Origin == "All Origins" & Education == 666)

  pop.sum <- lapply(list.files(file.path(censDir, 2010)), function(file) {
    fread(file.path(censDir, 2010, file))
  }) %>%
    rbindlist() %>%
    filter(variable %in% meta$variable) %>%
    mutate(FIPS.code = paste0(state, str_pad(county, 3, pad = "0")) %>% as.integer()) %>%
    group_by(FIPS.code) %>%
    summarise(pop_size = sum(pop_size))

  # Read crosswalk data
  crosswalk90 <- read.csv(file.path(dataDir, paste0("crosswalk_", 1990, "_2010.csv"))) %>%
    select(trtidFrom = trtid90, trtidTo = trtid10) %>%
    mutate(fromYear = 1990)

  crosswalk00 <- read.csv(file.path(dataDir, paste0("crosswalk_", 2000, "_2010.csv"))) %>%
    select(trtidFrom = trtid00, trtidTo = trtid10) %>%
    mutate(fromYear = 2000)

  crosswalk <- rbind(crosswalk00, crosswalk90) %>%
    transmute(
      #extract county fips code
      countyFrom = trtidFrom %>% str_pad(., 11, pad = "0") %>% substr(., 0, 5) %>% as.integer(),
      countyTo = trtidTo %>% str_pad(., 11, pad = "0") %>% substr(., 0, 5) %>% as.integer(),
      fromYear
    ) %>%
    distinct()

  # Further processing of crosswalk
  # filter for the county with the largest population in 2010
  crosswalk <- crosswalk %>%
    left_join(pop.sum, by = c("countyTo" = "FIPS.code")) %>%
    group_by(fromYear, countyFrom) %>%
    filter(pop_size == max(pop_size)) %>%
    mutate(pop_size = NULL)

  #fill in counties which were not merge with the same one
  filler <- merge(
    data.frame(
      countyFrom = setdiff(df$FIPS.code, crosswalk$countyFrom),
      countyTo = setdiff(df$FIPS.code, crosswalk$countyFrom)
    ),
    data.frame(fromYear = c(1990, 2000, 2010))
  )

  crosswalk <- rbind(crosswalk, filler) %>% distinct()
}
