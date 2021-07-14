
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
