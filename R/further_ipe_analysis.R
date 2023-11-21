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

# clean data
data_path <- "inputs/combined_ipe_verif_data.csv"

df_combined_verification_and_sample_data <- readr::read_csv(file =  data_path, na = "NULL") %>% 
  filter(AnonymizedGrp %in% df_hh_data$anonymizedgroup) %>% 
  left_join(df_hh_data, by = c("AnonymizedGrp" = "anonymizedgroup")) %>% 
  rename(any_of(setNames(df_questions_dap$question_code, df_questions_dap$question_name)))  %>%  
  mutate(across(.cols = any_of(df_questions_dap$question_name), .fns = ~as.character(.x))) %>% 
  mutate(today = as_date(today)) %>% 
  filter(today >= as_date("2021-10-01"), today <= as_date("2022-11-30")) %>%
  filter(settlement != "Kampala") %>% 
  mutate(settlement = case_when(settlement %in% c("Adjumani") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Adjumani",
                                settlement %in% c("Imvepi") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Imvepi",
                                settlement %in% c("Bidibidi") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Bidibidi",
                                settlement %in% c("Palorinya") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Palorinya",
                                settlement %in% c("Kyaka Ii") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Kyaka Ii",
                                settlement %in% c("Rhino") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Rhino",
                                settlement %in% c("Kyangwali") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Kyangwali",
                                settlement %in% c("Nakivale") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Nakivale",
                                settlement %in% c("Rwamwanja") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Rwamwanja",
                                settlement %in% c("Palabek") & (today >= as_date("2022-03-01")& today <= as_date("2022-06-30")) ~ "Palabek",
                                settlement %in% c("Kiryandongo") & (today >= as_date("2022-07-01")& today <= as_date("2022-07-31")) ~ "Kiryandongo",
                                settlement %in% c("Palabek") & (today >= as_date("2022-10-01")& today <= as_date("2022-10-31")) ~ "Palabek",
                                settlement %in% c("Lobule") & (today >= as_date("2022-02-01")& today <= as_date("2022-02-28")) ~ "Lobule",
                                settlement %in% c("Oruchinga") & (today >= as_date("2021-11-01")& today <= as_date("2021-11-30")) ~ "Oruchinga",
                                TRUE ~ NA_character_)) %>% 
  rename(difficulty_walking = "14", difficulty_lifting = "15", difficulty_selfcare = "16", difficulty_seeing = "26", 
         difficulty_hearing = "27", difficulty_remembering = "28", difficulty_communicating = "29", difficulty_emotions = "30", 
         medical_condition_lasted_3_months = "31", been_to_hospital_for_chronic_medical = "32", child_currently_not_living_with_you = "2",
         children_working = "123", avt_working = "53", avt_working_hh = "54") %>% 
  filter(!is.na(settlement)) %>% 
  select(c(businessunitname:settlement)) %>% 
  mutate(across(everything(), as.character))


# population figures
df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")

# make composite indicator ------------------------------------------------

df_with_composites <- df_combined_verification_and_sample_data %>%
  create_composites_verification() %>% 
  mutate(settlement = progres_coalocationlevel2name,
    strata = paste0(settlement, "_refugee"))

write_csv(df_with_composites, file = "outputs/compo.csv")

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
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "ipe_verification_further_analysis_sev.csv"), na="")

