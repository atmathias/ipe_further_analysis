library(tidyverse)
library(srvyr)
library(supporteR)

source("R/composite_indicators.R")
source("R/make_weights.R")

# question/choices codes and labels
df_questions <- readxl::read_excel("inputs/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  filter(!is.na(Question)) %>% 
  mutate(question_code = as.character(QuestionID),
         question_label = as.character(Question),
         question_name = as.character(name)
  ) %>% 
  select(question_code, question_label, question_name)

df_choices <- readxl::read_excel("inputs/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  mutate(choice_code = as.character(AnswerID),
         choice_label = as.character(Answer)) %>% 
  select(choice_code, choice_label)

choice_label_lookup <- setNames(object = df_choices$choice_label, nm = df_choices$choice_code)

# dap
dap <- read_csv("inputs/r_dap_ipe.csv")

df_questions_dap <- df_questions %>% 
  filter(question_name %in% dap$variable)

# clean HH data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"
data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_hh_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA") %>% 
  mutate(strata = paste0(settlement, "_refugee")) 
  
  
# write_csv(x = df_hh_data, file = "outputs/hh.csv")
# clean data
data_path <- "inputs/combined_ipe_verif_data.csv"

df_combined_verification <- readr::read_csv(file =  data_path, na = "NULL") %>% 
  filter(AnonymizedGrp %in% df_hh_data$anonymizedgroup) %>% 
  left_join(df_hh_data, by = c("AnonymizedGrp" = "anonymizedgroup")) %>% 
  rename(any_of(setNames(df_questions_dap$question_code, df_questions_dap$question_name)))  %>%  
  mutate(across(.cols = any_of(df_questions_dap$question_name), .fns = ~as.character(.x))) %>% 
  mutate(today = as_date(today)) %>% 
  filter(today >= as_date("2021-10-01"), today <= as_date("2022-11-30")) %>%
  filter(settlement != "Kampala") %>% 
  filter(settlement %in% c("Adjumani") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Imvepi") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30"))  |
           settlement %in% c("Bidibidi") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Palorinya") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Kyaka Ii") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Rhino") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Kyangwali") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Nakivale") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Rwamwanja") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Palabek") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) |
           settlement %in% c("Kiryandongo") & (today >= as_date("2022-07-01")& today <= as_date("2022-07-31")) |
           settlement %in% c("Palabek") & (today >= as_date("2022-10-01")& today <= as_date("2022-10-31")) |
           settlement %in% c("Lobule") & (today >= as_date("2022-02-01")& today <= as_date("2022-02-28")) |
           settlement %in% c("Oruchinga") & (today >= as_date("2021-11-01")& today <= as_date("2021-11-30"))) %>% 
rename(difficulty_walking = "14", difficulty_lifting = "15", difficulty_selfcare = "16", difficulty_seeing = "26", 
         difficulty_hearing = "27", difficulty_remembering = "28", difficulty_communicating = "29", difficulty_emotions = "30",
       hh_member_with_chronic_condition = "31",hh_member_with_chronic_condition_access_healthcare = "32", child_currently_not_living_with_you = "2",
         children_working = "123", avg_time_child_working_payment = "53", avt_working_hh = "54", hh_member_worked_past7days = "40",
         where_children_are_living = "3", children_5_17_years_working_to_support_hh_for_payment = "123", child_work_involve = "55",
         children_supporting_household_chores = "124", gender = progres_sexname, most_commonly_hh_need_rank_1 = "92",
         most_commonly_hh_need_rank_2 = "104", most_commonly_hh_need_rank_3 = "105", children_attending_school = "118") %>% 
rename(spent_savings = "61", bought_food_on_credit_or_borrowed_money_for_food = "63", reduced_essential_non_food_expenditures_such_as_education_health = "64",
         borrowed_money_to_cover_basic_needs_health_rent = "65", sold_household_goods = "66", sold_productive_assets_or_means_of_transport = "67", sold_sanitary_materials = "68",
         changed_accommodation_to_reduce_expenditures = "69",  female_members_under_18_got_married = "70", 
         consume_seed_stock_held_for_next_season = "71", harvested_immature_crops = "72", sold_house_or_land = "74",
         rented_out_the_house = "75", sold_more_non_productive_animals_than_usual = "76", sold_last_female_animals = "77",
         sent_hh_members_to_eat_elsewhere = "78", accepted_high_risk_illegal_exploitative_temporary_jobs = "79",
         engaged_in_transactional_and_survival_sex = "80", sent_household_members_to_beg = "81", sent_children_to_work = "82",
         withdrew_children_from_school = "83", relied_on_less_preferred__less_expensive_food = "84",
         borrowed_food_or_relied_on_help = "85", reduced_numbers_of_meals_eaten_per_day = "86", reduced_portion_size_of_meals = "87",
         restricted_consumption_of_adults_for_children = "88") %>% 
  filter(!is.na(settlement)) %>%  
  relocate(uuid, .after = relation_to_hoh) %>% 
   select(c(businessunitname:uuid)) %>% 
  mutate(across(where(is.character)))

 # population figures
df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")

# make composite indicator ------------------------------------------------

df_with_composites <- df_combined_verification_and_sample_data %>%
  create_composites_verification() %>% 
  mutate(settlement = progres_coalocationlevel2name,
    strata = paste0(settlement, "_refugee"))


# more indicators
new_indicators <- df_with_composites %>% 
  mutate(int.hoh_single_female = ifelse(gender %in% c("Female") & relation_to_hoh %in% c("head_of_household") &
                                        progres_maritalstatusname %in% c("Single", "Divorced", "Separated", "Widowed"), 
                                      "yes", "no"),
        int.hh_with_child_outside_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1694) ~ "yes",
                                                     progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1695) ~ "no",
                                                     TRUE ~ NA_character_),
        int.hh_with_disabled_member =  case_when(difficulty_seeing %in% c(1706 , 1707)|difficulty_hearing %in% c(1706 , 1707)|
                                              difficulty_walking %in% c(1706 , 1707)|difficulty_remembering %in% c(1706 , 1707)|
                                              difficulty_selfcare %in% c(1706 , 1707)|difficulty_communicating %in% c(1706 , 1707)~ "yes_disability", 
                                              difficulty_seeing %in% c(1704 , 1705)|difficulty_hearing %in% c(1704 , 1705)|
                                              difficulty_walking %in% c(1704 , 1705)|difficulty_remembering %in% c(11704 , 1705)|
                                              difficulty_selfcare %in% c(1704 , 1705)|difficulty_communicating %in% c(1704 , 1705)~
                        "no_disability", TRUE ~ NA_character_),
        int.hoh_child = ifelse(progres_age <= 17 & relation_to_hoh %in% c("head_of_household"), "yes", "no"),
        int.hh_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & children_5_17_years_working_to_support_hh_for_payment %in% c(1694) ~ "yes",
                                                    progres_relationshiptofpname %in% c("Focal Point") &  children_5_17_years_working_to_support_hh_for_payment %in% c(1695) ~ "no",
                                                    TRUE ~ NA_character_),
        int.hh_with_child_outside_of_home_by_location = case_when(progres_relationshiptofpname %in% c("Focal Point") & 
                                              where_children_are_living == 1699 ~ "Under care of another family in Uganda",
                                              progres_relationshiptofpname %in% c("Focal Point") & 
                                              where_children_are_living == 1700 ~ "Under care of another relative",
                                              progres_relationshiptofpname %in% c("Focal Point") & 
                                              where_children_are_living == 1701 ~ "Under care of another family-country of origin",
                                              progres_relationshiptofpname %in% c("Focal Point") & 
                                              where_children_are_living == 1702 ~ "Living alone independently in another location",
                                              progres_relationshiptofpname %in% c("Focal Point") & 
                                              where_children_are_living == 1703 ~ "Living in a third country ",
                                              TRUE ~ NA_character_),
        int.hh_children_worked_Hhchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & children_supporting_household_chores %in% c(1694) ~ "yes",
                                                  progres_relationshiptofpname %in% c("Focal Point") & children_supporting_household_chores %in% c(1695) ~ "no",
                                                  TRUE ~ NA_character_),
        int.hh_children_dangerous_work_conditions = case_when(
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1763) ~ "1763",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1764) ~ "1764",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1765) ~ "1765",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1766) ~ "1766",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1767) ~ "1767",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1768) ~ "1768",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1769) ~ "1769",
                                              progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1770) ~ "1770",
                                              TRUE ~ NA_character_),
        int.lcsi_cat = case_when((spent_savings %in% c(1816, 1817)|bought_food_on_credit_or_borrowed_money_for_food %in% c(1816, 1817)|
                                  borrowed_money_to_cover_basic_needs_health_rent %in% c(1816, 1817)|sold_household_goods %in% c(1816, 1817)|
                                  sold_sanitary_materials %in% c(1816, 1817)|changed_accommodation_to_reduce_expenditures %in% c(1816, 1817)|
                                  sold_more_non_productive_animals_than_usual %in% c(1816, 1817)|relied_on_less_preferred__less_expensive_food %in% c(1816, 1817)|
                                  borrowed_food_or_relied_on_help %in% c(1816, 1817)) ~ "hh_lcsi_stress",
                                 (spent_savings %in% c( 1818, 1819, 1820)|bought_food_on_credit_or_borrowed_money_for_food %in% c( 1818, 1819, 1820)|
                                    borrowed_money_to_cover_basic_needs_health_rent %in% c( 1818, 1819, 1820)|sold_household_goods %in% c( 1818, 1819, 1820)|
                                    sold_sanitary_materials %in% c( 1818, 1819, 1820)|changed_accommodation_to_reduce_expenditures %in% c( 1818, 1819, 1820)|
                                    sold_more_non_productive_animals_than_usual %in% c( 1818, 1819, 1820)|relied_on_less_preferred__less_expensive_food %in% c( 1818, 1819, 1820)|
                                    borrowed_food_or_relied_on_help %in% c( 1818, 1819, 1820)) ~ "hh_lcsi_none",
                               (reduced_essential_non_food_expenditures_such_as_education_health %in% c(1816, 1817)|sold_productive_assets_or_means_of_transport %in% c(1816, 1817)|consume_seed_stock_held_for_next_season %in% c(1816, 1817)|
                                  harvested_immature_crops %in% c(1816, 1817)| rented_out_the_house %in% c(1816, 1817)|sent_hh_members_to_eat_elsewhere %in% c(1816, 1817)|
                                  sent_children_to_work %in% c(1816, 1817)|withdrew_children_from_school %in% c(1816, 1817)|
                                  reduced_numbers_of_meals_eaten_per_day %in% c(1816, 1817)|reduced_portion_size_of_meals %in% c(1816, 1817)|
                                  restricted_consumption_of_adults_for_children %in% c(1816, 1817))~ "hh_lcsi_crisis",
                               (reduced_essential_non_food_expenditures_such_as_education_health %in% c(1818, 1819, 1820)|sold_productive_assets_or_means_of_transport %in% c(1818, 1819, 1820)|consume_seed_stock_held_for_next_season %in% c(1818, 1819, 1820)|
                                 harvested_immature_crops %in% c(1818, 1819, 1820)| rented_out_the_house %in% c(1818, 1819, 1820)|sent_hh_members_to_eat_elsewhere %in% c(1818, 1819, 1820)|
                                 sent_children_to_work %in% c(1818, 1819, 1820)|withdrew_children_from_school %in% c(1818, 1819, 1820)|
                                 reduced_numbers_of_meals_eaten_per_day %in% c(1818, 1819, 1820)|reduced_portion_size_of_meals %in% c(1818, 1819, 1820)|
                                 restricted_consumption_of_adults_for_children %in% c(1818, 1819, 1820))~ "hh_lcsi_none",
                               (female_members_under_18_got_married %in% c(1816, 1817)|74 %in% c(1816, 1817)|sold_last_female_animals %in% c(1816, 1817)|accepted_high_risk_illegal_exploitative_temporary_jobs %in% c(1816, 1817)|
                                  engaged_in_transactional_and_survival_sex %in% c(1816, 1817)|sent_household_members_to_beg %in% c(1816, 1817))~ 
                                 "hh_lcsi_emergency",
                               (female_members_under_18_got_married %in% c(1818, 1819, 1820)|74 %in% c(1818, 1819, 1820)|sold_last_female_animals %in% c(1818, 1819, 1820)|accepted_high_risk_illegal_exploitative_temporary_jobs %in% c(1818, 1819, 1820)|
                                  engaged_in_transactional_and_survival_sex %in% c(1818, 1819, 1820)|sent_household_members_to_beg %in% c(1818, 1819, 1820))~ 
                                 "hh_lcsi_none", TRUE ~ NA_character_)
        
  ) %>%
   group_by(uuid) %>% 
   summarise(
       int.hh_disabled = paste(int.hh_with_disabled_member, collapse = " : "),
       int.child_outside_of_home = paste(int.hh_with_child_outside_of_home, collapse = " : "),
       int.single_female = paste(int.hoh_single_female, collapse = " : "),
       int.child_hoh = paste(int.hoh_child, collapse = " : "),
       int.child_worked_for_employment = paste(int.hh_children_worked_forpayment, collapse = " : "),
       int.hh_child_outside_of_home_by_location = paste(int.hh_with_child_outside_of_home_by_location, collapse = " : "),
       int.hh_child_worked_Hhchores = paste(int.hh_children_worked_Hhchores, collapse = " : "),
       int.hh_child_dangerous_work_conditions = paste(int.hh_children_dangerous_work_conditions, collapse = " : "),
       int.lcsi_category = paste(int.lcsi_cat, collapse = " : ")
     ) %>% 
  mutate(i.hh_with_disabled_member =  case_when(str_detect(string = int.hh_disabled, pattern = "yes_disability") ~ "yes_disability",
                                 str_detect(string = int.hh_disabled, pattern = "no_disability") ~ "no_disability",
                                 str_detect(string = int.hh_disabled, pattern = NA_character_) ~ NA_character_),
        i.hh_with_child_outside_of_home = case_when(str_detect(string = int.child_outside_of_home, pattern = "yes") ~ "yes",
                            str_detect(string = int.child_outside_of_home, pattern = "no") ~ "no",
                            str_detect(string = int.child_outside_of_home, pattern = NA_character_) ~ NA_character_),
        i.hoh_single_female = ifelse(str_detect(string = int.single_female, pattern = "yes"), "yes", "no"),
        i.hoh_child = ifelse(str_detect(string = int.child_hoh, pattern = "yes"), "yes", "no"),
        i.hh_children_worked_forpayment = case_when(str_detect(string = int.child_worked_for_employment, pattern = "yes") ~ "yes",
                                  str_detect(string = int.child_worked_for_employment, pattern = "no") ~ "no",
                                  str_detect(string = int.child_worked_for_employment, pattern = NA_character_) ~ NA_character_),
        i.hh_with_child_outside_of_home_by_location = case_when(str_detect(string = int.hh_child_outside_of_home_by_location, pattern = "Under care of another family in Uganda") ~ "Under care of another family in Uganda",
                                  str_detect(string = int.hh_child_outside_of_home_by_location, pattern = "Under care of another relative") ~ "Under care of another relative",
                                  str_detect(string = int.hh_child_outside_of_home_by_location, pattern = "Under care of another family-country of origin") ~ "Under care of another family-country of origin",
                                  str_detect(string = int.hh_child_outside_of_home_by_location, pattern = "Living alone independently in another location") ~ "Living alone independently in another location",
                                  str_detect(string = int.hh_child_outside_of_home_by_location, pattern = "Living in a third country") ~ "Living in a third country",
                                  str_detect(string = int.hh_child_outside_of_home_by_location, pattern = NA_character_) ~ NA_character_),
        i.hh_children_worked_Hhchores = case_when(str_detect(string = int.hh_child_worked_Hhchores, pattern = "yes") ~ "yes",
                                  str_detect(string = int.hh_child_worked_Hhchores, pattern = "no") ~ "no",
                                  str_detect(string = int.hh_child_worked_Hhchores, pattern = NA_character_) ~ NA_character_),
        i.hh_children_dangerous_work_conditions = case_when(str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1763") ~ "1763",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1764") ~ "1764",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1765") ~ "1765",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1766") ~ "1766",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1767") ~ "1767",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1768") ~ "1768",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1769") ~ "1769",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = "1770") ~ "1770",
                                    str_detect(string = int.hh_child_dangerous_work_conditions, pattern = NA_character_) ~ NA_character_),
        i.lcsi_cat = case_when(str_detect(string = int.lcsi_category, pattern = "hh_lcsi_stress") ~ "hh_lcsi_stress",
                               str_detect(string = int.lcsi_category, pattern = "hh_lcsi_crisis") ~ "hh_lcsi_crisis",
                               str_detect(string = int.lcsi_category, pattern = "hh_lcsi_emergency") ~ "hh_lcsi_emergency",
                               str_detect(string = int.lcsi_category, pattern = "hh_lcsi_none") ~ "hh_lcsi_none",
                               str_detect(string = int.lcsi_category, pattern = NA_character_) ~ NA_character_)
        
  ) %>% 
            
            
            
  select(-c(starts_with("int.")))

# merge data  and main household data
df_hh_data_merged <- df_hh_data %>% 
  left_join(new_indicators, by = c("uuid"))

write_csv(x =df_hh_data_merged, file = "outputs/household.csv")

 # write_csv(df_with_composites, file = "outputs/compo.csv")

# create weights ----------------------------------------------------------

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_with_composites, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_with_composites %>% 
  left_join(ref_weight_table, by = "strata")


# set up design object ----------------------------------------------------


ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)


# analysis ----------------------------------------------------------------
# columns without data (income_from_work_past_30_days, 
#                       engage_in_activities_because_not_enough_money_for_basic_needs)

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   
                                                   input_dap = dap )
# merge analysis

combined_analysis <- df_main_analysis
# write_csv(x = df_main_analysis, file = "outputs/analysis.csv")
# add labels
full_analysis_labels <- combined_analysis %>%
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         select_type = "select_one") %>%
  mutate(variable_code = recode(variable, !!!setNames(df_questions_dap$question_code, df_questions_dap$question_name)),
         variable_label = recode(variable, !!!setNames(df_questions_dap$question_label, df_questions_dap$question_name)),
         variable_val_label = recode(variable_val, !!!choice_label_lookup))

# convert to percentage
full_analysis_long <- full_analysis_labels %>%
  mutate(`mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>%
  select(`Question code`= variable_code, 
         `Question`= variable,
         `Question label`= variable_label,
         variable, 
         `choices/options` = variable_val, 
         `choices/options label` = variable_val_label, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_ipe_verification_further_analysis_sev.csv"), na="")


# other analysis
write_csv(x = df_ref_with_weights, file = "outputs/jjjj.csv")

df_dep_ratio_with_weights <- df_ref_with_weights %>% 
  summarise(
    overall_dependency_ratio = sum(i.age_dependant)/sum(i.age_independent)
  )

df_dependency_ration <- df_with_composites %>% 
    summarise(
      overall_dependency_ratio = sum(i.age_dependant)/sum(i.age_independent))

regional_dependency_ratio = df_with_composites %>%
mutate(int.age_dependant = ifelse(progres_age %in% c(0:14) | progres_age %in% c(65:100), 1, 0),
         int.age_independent = ifelse(progres_age %in% c(15:64), 1, 0)) %>%
  group_by(region) %>% 
  summarise(regional_dependency_ratio = sum(int.age_dependant)/sum(int.age_independent))
    
settlement_dependency_ratio = df_with_composites %>%
  mutate(int.age_dependant = ifelse(progres_age %in% c(0:14) | progres_age %in% c(65:100), 1, 0),
         int.age_independent = ifelse(progres_age %in% c(15:64), 1, 0)) %>%
  group_by(settlement) %>% 
  summarise(settlement_dependency_ratio = sum(int.age_dependant)/sum(int.age_independent))


# lcsi
















