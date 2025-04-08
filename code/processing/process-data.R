###
# Data Processing Script
# Murphy John
# last modified: 2025-04-06
# Description: This code processes and compiles the raw data files into one
# working rds file.
###

# Setup ========================================================================

# load packages
library(dplyr)

# load data

## reported sfr data
raw_sfr <- read.csv(here::here(
  "data/raw/ReportedTickborneDisease_2016-2019_US_County-of-Residence.csv"
  ))

## 2018 social vulnerability index data
raw_svi <- read.csv(here::here(
  "data/raw/SVI_2018_US_County.csv"
))

## 2016-2019 avg temp data
raw_avg_temp <- read.csv(here::here(
  "data/raw/AvgTemp_2016-2019_US_County.csv"
), skip=4, header=TRUE)

## 2016-2019 max temp data
raw_max_temp <- read.csv(here::here(
  "data/raw/MaxTemp_2016-2019_US_County.csv"
), skip=4, header=TRUE)

# 2016 precipitation data
raw_precip16 <- read.csv(here::here(
  "data/raw/Precipitation_2016_US_County.csv"
))

# 2017 precipitation data
raw_precip17 <- read.csv(here::here(
  "data/raw/Precipitation_2017_US_County.csv"
))

# 2018 precipitation data
raw_precip18 <- read.csv(here::here(
  "data/raw/Precipitation_2018_US_County.csv"
))

## 2013 rucc data
raw_rucc <- read.csv(here::here(
  "data/raw/ruralurbancodes2013.csv"
))

## veterinary care accessibility score data
raw_vcas <- read.csv(here::here(
  "data/raw/VCAS.csv"
))

## health shortage areas data
raw_hsa <- read.csv(here::here(
  "data/raw/HealthShortageAreas_US_County.csv"
))

# Data processing ===========================================================

# clean sfr data
## keep only fips, state, county, SFR
## remove the word "County"/"Parish" from elements in "County" col (for merging)
## remove hawaii and alaska rows
dat_sfr <- raw_sfr %>%
  select(
    fips=FIPS,
    state=State.Name,
    county=County.Name,
    sfr_count=Spotted.fever.rickettsiosis
  ) %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b")
    ) %>%
  filter(
    !state %in% c("Alaska", "Hawaii")
  )

# clean svi data
## remove irrelevant columns
## capitalize only first letter(s) of state names (for merging)
## remove alaska and hawaii
## manually change the tilde n in Dona Ana, New Mexico
## data codes NAs as -999. change these to NA
dat_svi <- raw_svi %>%
  select(
    fips=FIPS,
    state=STATE,
    state_abr=ST_ABBR,
    county=COUNTY,
    population=E_TOTPOP,
    theme1=RPL_THEME1,
    theme2=RPL_THEME2,
    theme3=RPL_THEME3,
    theme4=RPL_THEME4,
    theme_all=RPL_THEMES
  ) %>%
  mutate(
    state=stringr::str_to_title(state),
    county=stringr::str_replace(county, "Doña Ana", "Dona Ana"),
    across(where(is.numeric), ~ na_if(., -999))
  ) %>%
  filter(
    !state %in% c("Alaska", "Hawaii")
  )

# clean avg temp data
## keep only relevant columns
## remove the word "County" from elements in the county col
dat_avg_temp <- raw_avg_temp %>%
  select(
    county=Name,
    state=State,
    avg_temp=Value
  ) %>%
  filter(county != "Washington, D.C.") %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b"),
    avg_temp = as.numeric(avg_temp)
  )

# clean max temp data
## keep only relevant columns
## remove the word "County" from elements in the county col
dat_max_temp <- raw_max_temp %>%
  select(
    county=Name,
    state=State,
    max_temp=Value
  ) %>%
  filter(county != "Washington, D.C.") %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b"),
    max_temp = as.numeric(max_temp)
  )

# merge and clean precipitation data
dat_precip_merge <- raw_precip16 %>%
  left_join(raw_precip17, by = "ID") %>%
  left_join(raw_precip18, by = "ID") %>%
  # remove washington dc
  filter(ID != "MD-511")

dat_precip <- dat_precip_merge %>%
  select(
    id = ID,
    county = Name.x,
    state = State.x,
    precip2016 = Value.x,
    precip2017 = Value.y,
    precip2018 = Value
  ) %>%
  mutate(
    precip2016 = as.numeric(precip2016),
    precip2017 = as.numeric(precip2017),
    precip2018 = as.numeric(precip2018)
  ) %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b"),
    # compute average over the 3 years
    avg_precip = rowMeans(
      select(., precip2016, precip2017, precip2018), na.rm = TRUE)
  ) %>%
  # drop precip years cols
  select(
    -precip2016, -precip2017, -precip2018
  )

# clean hsa data
## select only relevant cols
## clean up hsa value labels
## ## remove the word "County" from elements in the county col
## remove AK and HI
dat_hsa <- raw_hsa %>%
  select(
    county=county_name,
    state_abr= state_abbr,
    hsa_value = value
  ) %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b"),
    hsa_value = case_when(
      hsa_value == "None of county is shortage area" ~ 0,
      hsa_value == "Part of county is shortage area" ~ 1,
      hsa_value == "Whole county is shortage area" ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !state_abr %in% c("AK", "HI")
  )

# clean rucc data
## select only relevant cols
## remove AK and HI
dat_rucc <- raw_rucc %>%
  select(
    fips=FIPS,
    state_abr=State,
    county=County_Name,
    rucc_value=RUCC_2013,
    rucc_description=Description
  ) %>%
  mutate(
    county = stringr::str_remove(county, "\\s*(County|Parish)\\b"),
  ) %>%
  filter(
    !state_abr %in% c("AK", "HI")
  )

# clean vcas data
## select only relevant cols
## remove AK and HI
dat_vcas <- raw_vcas %>%
  select(
    fips=FIPS,
    county=COUNTY,
    state=STATE_NAME,
    vcas_value=index_score,
    dog_pop=population_dog_county
  ) %>%
  filter(
    !state %in% c("Alaska", "Hawaii")
  )

# Merge data ===================================================================

# merge all data into one dataframe
dat <- dat_svi %>%
  left_join(dat_sfr, by= c("fips", "state", "county")) %>%
  left_join(dat_avg_temp, by=c("county", "state")) %>%
  left_join(dat_max_temp, by=c("county", "state")) %>%
  left_join(dat_precip, by=c("county", "state")) %>%
  left_join(dat_hsa, by=c("county", "state_abr")) %>%
  left_join(dat_rucc, by=c("fips", "state_abr", "county")) %>%
  left_join(dat_vcas, by=c("fips", "county", "state")) %>%
  # compute sfr rates
  # multiple rates by 1000 to work with small rates
  # create dog_pop_ratio variable
  mutate(
    dog_pop_ratio = dog_pop / population,
    rate_sfr = sfr_count / population,
    rate_sfr_1000 = rate_sfr * 1000
  )

labelled::var_label(dat) <- c(
  "FIPS code",
  "State",
  "State Abbreviation",
  "County",
  "Population",
  "SVI Theme 1",
  "SVI Theme 2",
  "SVI Theme 3",
  "SVI Theme 4",
  "SVI Overall",
  "SFR Count",
  "Average Temperature",
  "Maximum Temperature",
  "ID",
  "Average Precipitation",
  "Health Shortage Area",
  "Rural Urban Continuum Code",
  "RUCC Description",
  "Veterinary Care Accessibility Score",
  "Dog Population",
  "Dog to Population Ratio",
  "SFR Rate",
  "SFR Rate times 1000"
)

# Save as rds ==================================================================
saveRDS(dat, here::here("data/processed/clean-data.rds"))

# END OF SCRIPT ================================================================