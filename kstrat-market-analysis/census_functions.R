#******************** Demographic Functions ***********************
census_demographics <- function(year, county, state){census_demographic_2000 <- get_acs(
  geography = "tract",
  state = state,
  county = county, 
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
  )}
ny_19 <- census_demographics(2019, county =  c("003", "007", "031"), "CT")


demographic_indicators <- function(df, tracts){
  df %>%
  filter(GEOID %in% tracts) %>%
  select("total_pop", "two_or_more_races", "race_asian", "race_black", "race_hisp", "race_white", "educ_hs", "educ_college", "educ_grad", "median_age", "pop_u18_num", "pop_65p_num", "pop_18_64_num", "speak_other_lang", "speak_other_lang_pct", "speak_only_eng", "speak_only_eng_pct")%>%
  summarize_all(sum) %>%
  transmute(census_year = "2019",
            geography = "Middletown",
            total_pop,
            two_or_more_races_pct = two_or_more_races/na_if(total_pop, 0),
            pop_race_asian_pct = race_asian / na_if(total_pop, 0), 
            pop_race_black_pct = race_black / na_if(total_pop, 0),
            pop_race_hisp_pct  = race_hisp / na_if(total_pop, 0),
            pop_race_white_pct = race_white / na_if(total_pop, 0),
            educ_college, 
            educ_grad, 
            median_age,
            pop_u18_pct = pop_u18_num / na_if(total_pop, 0), 
            pop_65p_pct = pop_65p_num /na_if(total_pop, 0),
            pop_18_64_pct = pop_65p_num /na_if(total_pop, 0),
            speak_other_lang_pct = speak_other_lang / na_if(total_pop, 0),
            speak_only_eng_pct = speak_only_eng / na_if(total_pop, 0)
  )}

#*****************************Economic Characteristics********************
census_economic <- function(year, county){get_acs(
  geography = "tract",
  county = county, 
  year = year,
  survey = "acs5",
  variables = c(
    "total_pop" = "B01003_001", # pop_num,
    #pop_pov_denom_num = "B17001_001", # pop_pov_denom_num
    "med_income" = 'B06011_001',
    "med_hh_inc" = "B25119_001", 
    "pop_pov_num" ="B17001_002", # pop_pov_num
    "unemp" = 'B27011_008',
    "labor_force" = "B27011_002",
    "aggregate_hh_income" = "B19025_001", # aggregate_hh_income
    "aggregate_hh_income_est" = "B19025_001", # aggregate_hh_income_est
    "hh_income_count_est" = "B19001_001" # hh_income_count_est summary variable
  )) %>%
  select(-moe) }

economic_indicators <- function(df, tracts) {
  df %>%
  select(!"NAME")%>%
  filter(GEOID %in% tracts) %>%
  group_by("GEOID") %>%
  summarise(total_pop = sum(pop_pov_num),
            med_income = sum(med_income),
            pop_pov_num = sum(pop_pov_num),
            unemp = sum(unemp),
            labor_force = sum(labor_force)
  ) %>%
  transmute(med_income, med_income, pop_pov_num, 
            pov_rate = pop_pov_num/total_pop,
            unemp,
            labor_force,
            unemp_rate = unemp/labor_force,
  ) }

# ******************Housing Functions *********************

# ********************* Housing Charateristics ******************
census_housing <- function(year, county){
  get_acs(
  geography = "tract",
  state = state_obs,
  county = county, 
  year = year,
  survey = "acs5",
  variables = c(
    "total_pop" = "B01003_001", # pop_num,
    "unit_num" = "B25002_001", 
    "unit_vac_num" = "B25002_003",
    "unit_occ_rent_num" = "B25003_003", # unit_occ_rent_num
    "unit_occ_own_num" = "B25003_002", # unit_occ_own_num
    "med_rent" = "B25064_001", 
    "unit_rent_cash_num" = "B25063_002",
    "unit_occ_num" = "B25002_002", # unit_occ_num
    "total_units" = "B25024_001", # total_units
    "total_1_det" = "B25024_002", # total_1_det
    "total_1_att" = "B25024_003", # total_1_att
    "total_units_rent" = "B25032_013", # 	total_units_rent
    "units_rent_1_det" = "B25032_014", # units_rent_1_det
    "units_rent_1_att" = "B25032_015",
    acs_vars("B25070_{c(7:10)*}E"))) %>%
  select(-moe) %>%
  pivot_wider(id_cols = c("GEOID", "NAME"), names_from = variable, values_from = estimate)%>%
  filter(GEOID %in% geoids) %>%
  mutate("rent_burd_pop" = acs_est_sum("B25070_{c(7:10)*}"))
}


housing_indicators <- function(df, tracts){
  transmute(GEOID, NAME, med_rent,
            unit_num, 
            unit_occ_perc = unit_occ_num / unit_num,
            unit_vac_perc = unit_vac_num / unit_num,
            unit_occ_own_perc = unit_occ_own_num/unit_occ_num,
            unit_occ_rent_perc = unit_occ_rent_num/unit_occ_num,
            rent_burd = rent_burd_pop/unit_rent_cash_num)
  }
