install.packages("tidyverse")
install.packages("tidycensus")
library(tidyverse)
library(tidycensus)
library(acssf)
library(purr)
### Code Inspo & References
# https://github.com/FurmanCenter/small-mid-cities-blog-1/blob/master/analysis.Rmd
# https://walker-data.com/tidycensus/articles/basic-usage.html

### Census API Key
# Input your census API key below to access the census data. You can sign-up for a key here:https://api.census.gov/data/key_signup.html
census_api_key("c32dfbf7d25fe9558fd11bf021780457970f96ff", install = TRUE)

# List out all of the GEOIDs in " "'s below. 
geoids_obs <- c("09007541100", 
                "09007541200", 
                "09007541300", 
                "09007541401", 
                "09007541402", 
                "09007541500", 
                "09007541600", 
                "09007541700", 
                "09007542000", 
                "09007542200", 
                "09007542000", 
                "09006802000")

study_area <- c("09007541600",
                "09007541700",
                "09006802000"
                )

state_obs <- "CT"
county_obs <- "007"



# Use this to find codes
v19 <- load_variables(2019, "acs5", cache = TRUE)

year <- c("2000", "2010", "2019")
arg_list <- list(x = year)
arguments <- cross_df(arg_list)
# Pull in census variables

census_demographic_2019 <- get_acs(
  geography = "tract",
  state = state_obs,
  county = county_obs, 
  year = year,
  survey = "acs5",
  variables = c(
    "total_pop" = "B01003_001", # pop_num,
    "race_white" = 'B03002_003',
    "race_asian" = 'B03002_006',
    "race_black" = 'B03002_004',
    "race_hisp" = 'B03002_012',
    "two_or_more_races" = "B02001_008",
    "educ_less_hs" = "B06009_002",
    "educ_hs" = "B06009_003",
    "educ_college" = 'B06009_005',
    "educ_grad" = 'B06009_006',
    "median_age" = "B01002_001",
    acs_vars("B01001_{c(3:6, 27:30)*}E"), 
    acs_vars("B01001_{c(20:25, 44:49)*}E"), 
    acs_vars("B01001_{c(7:19, 31:43)*}E"),
    "speak_other_lang" = "B06007_006", # Check this indicator
    "speak_only_eng" = "B06007_002" # Check this indicator
    )) %>%
  select(-moe) %>%
  pivot_wider(id_cols = c("GEOID", "NAME"), names_from = variable, values_from = estimate) %>%
  transmute(GEOID, 
            NAME, 
            total_pop,
            two_or_more_races,
            two_or_more_races_pct = two_or_more_races/na_if(total_pop, 0),
            race_asian,
            pop_race_asian_pct = race_asian / na_if(total_pop, 0), 
            race_black,
            pop_race_black_pct = race_black / na_if(total_pop, 0),
            race_hisp,
            pop_race_hisp_pct  = race_hisp / na_if(total_pop, 0),
            race_white,
            pop_race_white_pct = race_white / na_if(total_pop, 0),
            educ_hs, 
            educ_college, 
            educ_grad, 
            median_age,
            "pop_u18_num" = acs_est_sum("B01001_{c(3:6, 27:30)*}"),
            "pop_65p_num" = acs_est_sum("B01001_{c(20:25, 44:49)*}"),
            "pop_18_64_num" = acs_est_sum("B01001_{c(7:19, 31:43)*}"),
            speak_other_lang,
            speak_other_lang_pct = speak_other_lang / na_if(total_pop, 0),
            speak_only_eng,
            speak_only_eng_pct = speak_only_eng / na_if(total_pop, 0)
            ) %>%
  filter(GEOID %in% geoids_obs)

#Economic Characteristics
census_demographic_2019 <- get_acs(
  geography = "tract",
  state = state_obs,
  county = county_obs, 
  year = 2019,
  survey = "acs5",
  variables = c(
    med_income = 'B06011_001',
    pop_pov_num ="B17001_002", # pop_pov_num
    unemp = 'B27011_008',
    unit_occ_num = "B25002_002", # unit_occ_num
    unit_occ_own_num = "B25003_002", # unit_occ_own_num
    unit_occ_rent_num = "B25003_003", # unit_occ_rent_num
    unit_rent_cash_num = "B25063_002", # unit_rent_cash_num
    unit_num = "B25002_001", 
    unit_vac_num = "B25002_003", 
    med_rent = "B25064_001", 
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
    units_rent_1_att = "B25032_015"
  )) %>%
  filter(GEOID %in% geoids) 

# Creating indicators
# census_19_indicators <- census_2019 %>%
#   select(-moe) %>%
#   pivot_wider(id_cols = c("GEOID", "NAME"), names_from = variable, values_from = estimate) %>%
#   select(!"GEOID", !"NAME") %>%
#   summarize() %>%
#   transmute(GEOID, NAME, 
#             total_pop,
#             med_income,
#             med_rent,
#             unemp,
#             minority = 1 - (race_white/total_pop),
#             college = (educ_college + educ_grad)/total_pop,
#             poverty_rate = pov_pop_num/total_pop, # do we want to use total pop as the denominator instead? 
#             unemp_rate = unemp/na_if(labor_force, 0),
#             pop_race_asian_pct = race_asian / na_if(total_pop, 0), 
#             pop_race_black_pct = race_black / na_if(total_pop, 0),
#             pop_race_hisp_pct  = race_hisp / na_if(total_pop, 0),
#             pop_race_white_pct = race_white / na_if(total_pop, 0)
#   )

  


#write.csv(census_2019, "~/Desktop/kstrat/MarketAnalysis/census_2019_middletown.csv")