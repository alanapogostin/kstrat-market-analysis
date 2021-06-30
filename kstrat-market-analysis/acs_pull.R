install.packages("tidyverse")
install.packages("tidycensus")
library(tidyverse)
library(tidycensus)
library(acssf)
### Code Inspo & References
# https://github.com/FurmanCenter/small-mid-cities-blog-1/blob/master/analysis.Rmd
# https://walker-data.com/tidycensus/articles/basic-usage.html

### Census API Key
# Input your census API key below to access the census data. You can sign-up for a key here:https://api.census.gov/data/key_signup.html
census_api_key("c32dfbf7d25fe9558fd11bf021780457970f96ff", install = TRUE)

# List out all of the GEOIDs in " "'s below. 
geoids <- c("09007541100", "09007541200", "09007541300", "09007541401", "09007541402", "09007541500", "09007541600", "09007541700", "09007542000", "09007542200", "09007542000", "09006802000")
v19 <- load_variables(2019, "acs5", cache = TRUE)
# Pull in census variables
census_2019 <- get_acs(
  geography = "tract",
  state = "CT",
  county = "007",
  year = 2019,
  survey = "acs5",
  output = "wide",
  geometry = FALSE,
  moe_sum = FALSE,
  variables = c(
    pop_num = "B01003_001E", # pop_num,
    pop_race_asian_num = "B03002_006E", # pop_race_asian_num
    pop_race_black_num = "B03002_004E", # pop_race_black_num
    pop_race_hisp_num = "B03002_012E", # pop_race_hisp_num
    pop_race_white_num = "B03002_003E", # pop_race_white_num
    pop_pov_num ="B17001_002E", # pop_pov_num
    unit_occ_num = "B25002_002E", # unit_occ_num
    unit_occ_own_num = "B25003_002E", # unit_occ_own_num
    unit_occ_rent_num = "B25003_003E", # unit_occ_rent_num
    unit_rent_cash_num = "B25063_002E", # unit_rent_cash_num
    unit_num = "B25002_001E", # unit_num
    unit_vac_num = "B25002_003E", # unit_vac_num
    rent_gross_med = "B25064_001E", # rent_gross_med
    hh_inc_med = "B25119_001E", # hh_inc_med
    rent_burden_sev_num = "B25070_010E", # rent_burden_sev_num
    aggregate_hh_income = "B19025_001E", # aggregate_hh_income
    pop_pov_denom_num = "B17001_001E", # pop_pov_denom_num
    aggregate_hh_income_est = "B19025_001E", # aggregate_hh_income_est
    hh_income_count_est = "B19001_001E", # hh_income_count_est summary variable
    total_units = "B25024_001E", # total_units
    total_1_det = "B25024_002E", # total_1_det
    total_1_att = "B25024_003E", # total_1_att
    total_units_rent = "B25032_013E", # 	total_units_rent
    units_rent_1_det = "B25032_014", # units_rent_1_det
    units_rent_1_att = "B25032_015E", # units_rent_1_att
    homeowner_nonHisp_white_est = "B25003H_002E", # homeowner_nonHisp_white_est
    count_nonHisp_white_est = "B25003H_001E", # count_nonHisp_white_est
    homeowner_Black_est = "B25003B_002E", # homeowner_Black_est
    count_Black_est = "B25003B_001E", # count_Black_est
    homeowner_Asian_est = "B25003D_002E", # homeowner_Asian_est
    count_Asian_est = "B25003D_001E", # count_Asian_est
    homeowner_Latino_est = "B25003I_002E", # homeowner_Latino_est
    count_Latino_est = "B25003I_001E" # count_Latino_est
  )) %>% census_2019 %>%
  filter(GEOID %in% geoids)
