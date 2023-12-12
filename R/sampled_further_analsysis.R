library(tidyverse)
library(srvyr)
library(lubridate)
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

# clean sampled data
data_path <- "inputs/combined_ipe_verif_data.csv"

df_verification_data <- readr::read_csv(file =  data_path, na = "NULL") 


# clean HH data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"
data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_hh_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA") %>% 
  left_join(df_verification_data, by = c("anonymizedgroup" = "AnonymizedGrp")) %>%  
  group_by(anonymizedgroup) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(strata = paste0(settlement, "_refugee")) %>%  
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
  mutate(across(where(is.character))) %>% 
  rename(gender = progres_sexname) %>% 
  select(c(today:gender))
 
# loop_mental health (mh)
  mental_health_loop <- readxl::read_excel(path = data_path, sheet = "mental_health", na = "NA") %>% 
  rename(gender = individual_sex) %>% 
  mutate(across(where(is.character), str_to_lower))

# add more indicators (convert to household)
df_mh_loop_hh_indicators <- mental_health_loop %>% 
  rename(uuid = "_submission__uuid") %>% 
  mutate(int.hh_member_mh_state = case_when(feel_so_afraid %in%c("all_of_the_time", "most_of_the_time")|
                            feel_so_angry %in%c("all_of_the_time", "most_of_the_time")|
                            feel_so_uninterested_in_things %in%c("all_of_the_time", "most_of_the_time")|
                            feel_so_hopeless %in%c("all_of_the_time", "most_of_the_time")|
                            feel_so_severely_upset_about_bad_things_that_happened %in%c("all_of_the_time", "most_of_the_time")|
                            often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("all_of_the_time", "most_of_the_time") ~
                            "yes_mental_illness", 
                            feel_so_afraid %in%c("none_of_the_time")|
                              feel_so_angry %in%c("none_of_the_time")|
                              feel_so_uninterested_in_things %in%c("none_of_the_time")|
                              feel_so_hopeless %in%c("none_of_the_time")|
                              feel_so_severely_upset_about_bad_things_that_happened %in%c("none_of_the_time")|
                              often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("none_of_the_time") ~
                              "no_mental_illness",  TRUE ~ NA_character_)) %>% 
  group_by(uuid) %>% 
  summarise(
    int.hh_mh_entries = paste(int.hh_member_mh_state, collapse = " : ")
  ) %>% 
  mutate(i.hh_mh =  case_when(str_detect(string = int.hh_mh_entries, 
                                         pattern = "mental_illness_yes") ~ "mental_illness_yes",
                              str_detect(string = int.hh_mh_entries, 
                                         pattern = "mental_illness_mild") ~ "mental_illness_mild",
                              str_detect(string = int.hh_mh_entries, 
                                         pattern = "none") ~ "none")) %>% 
  select(-c(starts_with("int.")))

# add mh_hh_indicators to hh data
df_hh_mh_data_merged <- df_hh_data %>% 
  left_join(df_mh_loop_hh_indicators, by = "uuid")

# make composite indicator hh ------------------------------------------------

df_with_composites_sampled <- df_hh_mh_data_merged %>%
  create_composites_sampled() %>% 
  mutate(strata = paste0(settlement, "_refugee"))

# make composite for mental health individual data
df_with_composites_mh <- mental_health_loop %>%
  create_composites_mental_health() 

# population figures
df_ref_pop_sampled <- read_csv("inputs/refugee_population_ipe.csv")

# create weights ----------------------------------------------------------

# refugee weights main
ref_weight_table_sampled <- make_refugee_weight_table(input_df_ref = df_with_composites_sampled, 
                                              input_refugee_pop = df_ref_pop_sampled)
df_ref_with_weights <- df_with_composites_sampled %>% 
  left_join(ref_weight_table_sampled, by = "strata")

loop_support_data <- df_ref_with_weights %>% select(uuid, region, settlement, i.hoh_by_gender, strata, weights)


# set up design object main _analysis ----------------------------------------------------

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)

# analysis ----------------------------------------------------------------
# main analysis
df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap %>% filter(level %in% c("Household"))) %>% 
  mutate(level = "Household")
  
# mental health -----------------------------------------------------------

df_mental_health_data <- loop_support_data %>% 
  inner_join(df_with_composites_mh, by = c("uuid" = "_submission__uuid") ) 

# set up design object
ref_svy_mental_health_loop <- as_survey(.data = df_mental_health_data, strata = strata, weights = weights)
# analysis
df_analysis_mental_health_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_mental_health_loop,
                                                                 input_dap = dap %>% 
                                                                   filter(level %in% c("Individual"))
) %>% 
  mutate(level = "Individual")


# merge analysis
combined_analysis <- bind_rows(df_main_analysis, df_analysis_mental_health_loop) 


# convert to percentage
full_analysis_long <- combined_analysis %>%
  mutate(`mean/pct` = as.numeric(`mean/pct`),
         `mean/pct` =  `mean/pct`*100,
         `mean/pct` = round(`mean/pct`, digits = 2)) %>% 
  select(variable, variable_val, `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val,
         level) %>% 
  filter(!variable %in% c("i.hoh_by_gender"))

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_ipe_sampled_further_analysis_sev.csv"), na="")

 #further analysis analysis


# NFI analysis
# water container
df_overall_median_water_container_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.water_container_category)) %>% 
  mutate(i.number_water_container = as.numeric(i.number_water_container)) %>% 
  group_by(i.water_container_category) %>% 
  summarise(
    overall_disaggregation_median  = median(i.number_water_container, na.rm = TRUE))

df_regional_median_water_container_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.water_container_category)) %>% 
  mutate(i.number_water_container = as.numeric(i.number_water_container)) %>% 
  group_by(region, i.water_container_category) %>% 
  summarise(
    regional_disaggregation_median  = median(i.number_water_container, na.rm = TRUE))

df_settlement_median_water_container_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.water_container_category)) %>% 
  mutate(i.number_water_container = as.numeric(i.number_water_container)) %>% 
  group_by(settlement, i.water_container_category) %>% 
  summarise(
    regional_disaggregation_median  = median(i.number_water_container, na.rm = TRUE) %>% 
    round(0))


# tarpaulin
df_overall_median_tarpaulin_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.tarpaulin_category)) %>% 
  mutate(i.number_tarpaulin = as.numeric(i.number_tarpaulin)) %>% 
  group_by(i.tarpaulin_category) %>% 
  summarise(
    overall_disaggregation_median  = median(i.number_tarpaulin, na.rm = TRUE))

df_regional_median_tarpaulin_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.tarpaulin_category)) %>% 
  mutate(i.number_tarpaulin = as.numeric(i.number_tarpaulin)) %>% 
  group_by(region, i.tarpaulin_category) %>% 
  summarise(
    regional_disaggregation_median  = median(i.number_tarpaulin, na.rm = TRUE))

df_settlement_median_tarpaulin_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.tarpaulin_category)) %>% 
  mutate(i.number_tarpaulin = as.numeric(i.number_tarpaulin)) %>% 
  group_by(settlement, i.tarpaulin_category) %>% 
  summarise(
    settlement_disaggregation_median  = median(i.number_tarpaulin, na.rm = TRUE))


# solar_lamp
df_overall_median_solar_lamp_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.solar_lamp_category)) %>% 
  mutate(i.number_solar_lamp = as.numeric(i.number_solar_lamp)) %>% 
  group_by(i.solar_lamp_category) %>% 
  summarise(
    overall_disaggregation_median  = median(i.number_solar_lamp, na.rm = TRUE))

df_regional_median_solar_lamp_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.solar_lamp_category)) %>% 
  mutate(i.number_solar_lamp = as.numeric(i.number_solar_lamp)) %>% 
  group_by(region, i.solar_lamp_category) %>% 
  summarise(
    regional_disaggregation_median  = median(i.number_solar_lamp, na.rm = TRUE))

df_settlelement_median_solar_lamp_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.solar_lamp_category)) %>% 
  mutate(i.number_solar_lamp = as.numeric(i.number_solar_lamp)) %>% 
  group_by(settlement, i.solar_lamp_category) %>% 
  summarise(
    settlelement_disaggregation_median  = median(i.number_solar_lamp, na.rm = TRUE))


# kitchen set
df_overall_median_kitchen_set_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.kitchen_set_category)) %>% 
  mutate(i.number_kitchen_set = as.numeric(i.number_kitchen_set)) %>% 
  group_by(i.kitchen_set_category) %>% 
  summarise(
    overall_disaggregation_median  = median(i.number_kitchen_set, na.rm = TRUE))

df_regional_median_kitchen_set_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.kitchen_set_category)) %>% 
  mutate(i.number_kitchen_set = as.numeric(i.number_kitchen_set)) %>% 
  group_by(region, i.kitchen_set_category) %>% 
  summarise(
    regional_disaggregation_median  = median(i.number_kitchen_set, na.rm = TRUE))

df_settlelement_median_kitchen_set_per_hh_size <- df_with_composites_sampled %>% 
  filter(!is.na(i.kitchen_set_category)) %>% 
  mutate(i.number_kitchen_set = as.numeric(i.number_kitchen_set)) %>% 
  group_by(settlement, i.kitchen_set_category) %>% 
  summarise(
    settlelement_disaggregation_median  = median(i.number_kitchen_set, na.rm = TRUE))


median_analysis <- bind_rows(df_overall_median_kitchen_set_per_hh_size, df_regional_median_kitchen_set_per_hh_size,
                             df_settlelement_median_kitchen_set_per_hh_size)


write_csv(x = median_analysis, file = "outputs/median.csv")
df_settlelement_median_kitchen_set_per_hh_size <- df_with_composites_sampled %>%
  mutate(
  i.kitchen_set_category = case_when((hh_size > 0 & hh_size <4)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_1_and_3_HH_size",
                                     (hh_size > 3 & hh_size <7)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_4_and_6_HH_size",
                                     (hh_size > 6 & hh_size <10)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_7_and_9_HH_size",
                                     (hh_size > 9)& (kitchen_set_cond %in%c("good", "moderate")) ~ "10_or_more_HH_size"),
i.number_kitchen_set = ifelse(i.kitchen_set_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                            "10_or_more_HH_size"), kitchen_set_num, NA_character_)) %>% 
  filter(!is.na(i.kitchen_set_category)) %>% 
  mutate(
i.number_kitchen_set = as.numeric(i.number_kitchen_set),
i.median_number_kitchen_set = median(i.number_kitchen_set, na.rm = TRUE))




