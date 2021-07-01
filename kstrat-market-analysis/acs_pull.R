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
geoids_obs <- c("09007541100", "09007541200", "09007541300", "09007541401", "09007541402", "09007541500", "09007541600", "09007541700", "09007542000", "09007542200", "09007542000", "09006802000")
state_obs <- "CT"
county_obs <- "007"
year_obs <- 2019

acs5sub <- load_variables(2015, "acs5/subject", cache = TRUE)
v19 <- load_variables(2019, "acs5", cache = TRUE)

# Pull in census variables
census_2019 <- get_acs(
  geography = "tract",
  state = state_obs,
  county = county_obs, 
  year = year_obs,
  survey = "acs5",
  variables = c(
    pop_num = "B01003_001", # pop_num,
    race_white = 'B02001_002',
    race_asian = 'B02001_005',
    race_black = 'B02001_003',
    race_hisp = 'B03002_012',
    med_income = 'B06011_001',
    pop_pov_num ="B17001_002", # pop_pov_num
    unit_occ_num = "B25002_002", # unit_occ_num
    unit_occ_own_num = "B25003_002", # unit_occ_own_num
    unit_occ_rent_num = "B25003_003", # unit_occ_rent_num
    unit_rent_cash_num = "B25063_002", # unit_rent_cash_num
    unit_num = "B25002_001", # unit_num
    unit_vac_num = "B25002_003", # unit_vac_num
    rent_gross_med = "B25064_001", # rent_gross_med
    hh_inc_med = "B25119_001", # hh_inc_med
    rent_burden_sev_num = "B25070_010", # rent_burden_sev_num
    aggregate_hh_income = "B19025_001", # aggregate_hh_income
    pop_pov_denom_num = "B17001_001", # pop_pov_denom_num
    aggregate_hh_income_est = "B19025_001", # aggregate_hh_income_est
    hh_income_count_est = "B19001_001", # hh_income_count_est summary variable
    total_units = "B25024_001", # total_units
    total_1_det = "B25024_002", # total_1_det
    total_1_att = "B25024_003", # total_1_att
    total_units_rent = "B25032_013", # 	total_units_rent
    units_rent_1_det = "B25032_014", # units_rent_1_det
    units_rent_1_att = "B25032_015", # units_rent_1_att
    homeowner_nonHisp_white_est = "B25003H_002", # homeowner_nonHisp_white_est
    count_nonHisp_white_est = "B25003H_001", # count_nonHisp_white_est
    homeowner_Black_est = "B25003B_002", # homeowner_Black_est
    count_Black_est = "B25003B_001", # count_Black_est
    homeowner_Asian_est = "B25003D_002", # homeowner_Asian_est
    count_Asian_est = "B25003D_001", # count_Asian_est
    homeowner_Latino_est = "B25003I_002", # homeowner_Latino_est
    count_Latino_est = "B25003I_001" # count_Latino_est
  )) %>%
  filter(GEOID %in% geoids) 

# Creating indicators
census_19_indicators <- census_2019 %>%
  select(-moe) %>%
  pivot_wider(id_cols = c("GEOID", "NAME"), names_from = variable, values_from = estimate) %>%
  transmute(GEOID, NAME, pop_num,
            med_income,
            med_rent,
            unemp,
            minority = 1 - (race_white/race_pop),
            college = (educ_college + educ_grad)/educ_pop,
            poverty_rate = poverty/poverty_pop, # do we want to use total pop as the denominator instead? 
            poverty_category = case_when(poverty_rate < .01 ~ "low poverty", 
                                         poverty_rate >= .01 & poverty_rate < .02~ "mid poverty",
                                         poverty_rate >= .02 ~ "high poverty"),
            unemp_rate = unemp/na_if(labor_force, 0),
            pop_race_asian_pct = race_asian / na_if(total_pop, 0), 
            pop_race_black_pct = race_black / na_if(total_pop, 0),
            pop_race_hisp_pct  = race_hisp / na_if(total_pop, 0),
            pop_race_white_pct = race_white / na_if(total_pop, 0)
  )
write.csv(census_2019, "~/Desktop/kstrat/MarketAnalysis/census_2019_middletown.csv")