# creating composite indicators -------------------------------------------

create_composites_verification <- function(input_df) {
  input_df %>% 
    # demographic indicators
    mutate(region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                                       settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                       settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", 
                                                         "Rhino") ~ "West Nile"),
                 i.hoh_by_gender = ifelse(progres_relationshiptofpname %in% c("Focal Point"), progres_sexname, NA_character_),
                  i.hoh_single_female = ifelse(progres_sexname %in% c("Female") & progres_relationshiptofpname %in% c("Focal Point") &
                                                progres_maritalstatusname %in% c("Single", "Divorced", "Separated", "Widowed"), 
                                               "yes", "no"),
                  i.hoh_child = ifelse(progres_age <= 17 & progres_relationshiptofpname %in% c("Focal Point"), "yes", "no"),
                  i.disability_age_group = case_when(progres_age %in% c(5:12) ~ "age_5_12",
                                                     progres_age %in% c(13:18) ~ "age_13_18",
                                                     progres_age %in% c(19:24) ~ "age_19_24",
                                                     progres_age %in% c(25:59) ~ "age_25_59",
                                                     progres_age %in% c(60:100) ~ "age_greater_59"),
                 i.hh_member_disability_by_age_group_and_gender =  case_when(difficulty_seeing %in% c(1706 : 1707)|difficulty_hearing %in% c(1706 : 1707)|
                                                       difficulty_walking %in% c(1706 : 1707)|difficulty_remembering %in% c(1706 : 1707)|
                                                       difficulty_selfcare %in% c(1706 : 1707)|difficulty_communicating %in% c(1706 : 1707)~ "yes_disability", 
                                                       difficulty_seeing %in% c(1704 : 1705)|difficulty_hearing %in% c(1704 : 1705)|
                                                       difficulty_walking %in% c(1704 : 1705)|difficulty_remembering %in% c(11704 : 1705)|
                                                       difficulty_selfcare %in% c(1704 : 1705)|difficulty_communicating %in% c(1704 : 1705)~
                                                       "no_disability", TRUE ~ NA_character_),
                  
                  i.hh_with_disabled_member =  case_when(difficulty_seeing %in% c(1706 : 1707)|difficulty_hearing %in% c(1706 : 1707)|
                                                           difficulty_walking %in% c(1706 : 1707)|difficulty_remembering %in% c(1706 : 1707)|
                                                           difficulty_selfcare %in% c(1706 : 1707)|difficulty_communicating %in% c(1706 : 1707)~ "yes_disability", 
                                                         difficulty_seeing %in% c(1704 : 1705)|difficulty_hearing %in% c(1704 : 1705)|
                                                           difficulty_walking %in% c(1704 : 1705)|difficulty_remembering %in% c(11704 : 1705)|
                                                           difficulty_selfcare %in% c(1704 : 1705)|difficulty_communicating %in% c(1704 : 1705)~
                                                           "no_disability", TRUE ~ NA_character_),
                i.hoh_disability = ifelse(progres_relationshiptofpname %in% c("Focal Point") & i.hh_with_disabled_member %in% c("yes_disability"),
                                            "yes_disability", "no_disability"), 
                    # health indicators
                  # i.hh_member_with_chronic_condition = case_when(medical_condition_lasted_3_months == "1694" ~ "yes",  
                                                                 # medical_condition_lasted_3_months == "1695" ~ "no",  TRUE ~ NA_character_),
                    i.chronic_illness_age_group = case_when(progres_age %in% c(0:2) ~ "age_0_2",
                                                     progres_age %in% c(3:5) ~ "age_3_5",
                                                     progres_age %in% c(6:12) ~ "age_6_12",
                                                     progres_age %in% c(13:18) ~ "age_13_18",
                                                     progres_age %in% c(19:24) ~ "age_19_24",
                                                     progres_age %in% c(25:59) ~ "age_25_59",
                                                     progres_age %in% c(60:100) ~ "age_greater_59"),

                   # i.hh_member_with_chronic_condition_by_age_group = ifelse(31 %in% c(1694), "yes", "no"),

                   # i.hh_member_with_chronic_condition_access_healthcare = case_when(32 == 1708 ~ "yes",
                                                                                  # 32 == 1709 ~ "no",  TRUE ~ NA_character_),
                  # i.hh_member_with_chronic_condition_access_healthcare_by_age_group = case_when(been_to_hospital_for_chronic_medical == 1708 ~ "yes",
                                                                          # been_to_hospital_for_chronic_medical == 1709 ~ "no",
                                                                          # TRUE ~ NA_character_),
                  # livelihoods indicators
                  
                   i.hh_member_occupation_pastyear_age_group = case_when(progres_age %in% c(1:4) ~ "age_1_4",
                                                                        progres_age %in% c(5:11) ~ "age_5_11",
                                                                        progres_age %in% c(12:17)~ "age_12_17",
                                                                        progres_age %in% c(18:25) ~ "age_18_25",
                                                                        progres_age %in% c(26:59) ~ "age_26_59",
                                                                        progres_age %in% c(60:100) ~ "age_greater_59"),
                   # protection
                  # i.hh_with_child_outsid_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1694) ~ "yes",
                                                      # progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1695) ~ "no",
                                                      # TRUE ~ NA_character_),
           
                   i.hh_with_child_outside_of_home_by_location = case_when(progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1699) ~ "Under care of another family in Uganda (foster family)",
                                                                   progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1700) ~ "Under care of another relative (kinship care arrangement) in Uganda",
                                                                   progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1701) ~ "Under care of another family in his/her country of origin",
                                                                   progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1702) ~ "Living alone independently in another location",
                                                                   progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1703) ~ "Living in a third country (not Uganda nor country of origin)",
                                                                   progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c(1668) ~ "dk",
                                                                       TRUE ~ NA_character_),
           
                   i.hh_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & children_5_17_years_working_to_support_hh_for_payment %in% c(1694) ~ "yes",
                                                              progres_relationshiptofpname %in% c("Focal Point") &  children_5_17_years_working_to_support_hh_for_payment %in% c(1695) ~ "no",
                                                              TRUE ~ NA_character_),
                  i.avg_time_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1759) ~ "Between 1 and 13 hours",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1760) ~ "Between 14 and 42 hours",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1761) ~ "more than 42",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(2473) ~ "No kid working",
                                                                    TRUE ~ NA_character_),
                i.hh_children_worked_Hhchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & children_supporting_household_chores %in% c(1694) ~ "yes",
                                                          progres_relationshiptofpname %in% c("Focal Point") & children_supporting_household_chores %in% c(1695) ~ "no",
                                                          TRUE ~ NA_character_),
                i.avg_time_children_worked_HHchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1759, 2488) ~ "Between 1 and 14 hours",
                                                        progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1760, 2489) ~ "Between 14 and 27 hours",
                                                        progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(2490) ~ "Between 28 and 42 hours",
                                                        progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1761, 2491) ~ "More than 43",
                                                        progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(2473, 2492) ~ "No kid working",
                                                                TRUE ~ NA_character_),
                i.hh_children_dangerous_work_conditions = case_when(
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1763) ~ "1763",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1764) ~ "1764",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1765) ~ "1765",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1766) ~ "1766",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1767) ~ "1767",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1768) ~ "1768",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1769) ~ "1769",
                                                        progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1770) ~ "1770",
                                                        TRUE ~ NA_character_)) %>%
  
             
      select(-c(starts_with("int.")))
}
 # creating composites sampled

create_composites_sampled <- function(input_df) {
  input_df %>%
    mutate(region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                              settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                              settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya",
                                                "Rhino") ~ "West Nile"),
      # wash
    i.total_water_volume_per_person = case_when(calc_total_volume_per_person < 15 ~ "less_than_15L_per_person_per_day",
                                               calc_total_volume_per_person == 50 ~ "15L_per_person_per_day",
                                               calc_total_volume_per_person > 15 ~ "more_than_15L_per_person_per_day"),
    # shelter and nfi
    int.sleeping_mat_num_average = !is.na(sleeping_mat_num)/hh_size,
    i.sleeping_mat_num_average = case_when(int.sleeping_mat_num_average < 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                         int.sleeping_mat_num_average == 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "1_per_person",
                                         int.sleeping_mat_num_average > 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "above_1_per_person"),

    int.blanket_num_average = !is.na(blanket_num)/hh_size,
    i.blanket_num_average = case_when(int.blanket_num_average < 1 & blanket_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                     int.blanket_num_average == 1 & blanket_cond %in% c("good", "moderate") ~ "1_per_person",
                                     int.blanket_num_average > 1 & blanket_cond %in% c("good", "moderate") ~ "above_1_per_person"),

    int.mosquito_net_num_average = !is.na(mosquito_net_num)/hh_size,
    i.mosquito_net_num_average = case_when(int.mosquito_net_num_average < 1 & mosquito_net_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                      int.mosquito_net_num_average == 1 & mosquito_net_cond %in% c("good", "moderate") ~ "1_per_person",
                                      int.mosquito_net_num_average > 1 & mosquito_net_cond %in% c("good", "moderate") ~ "above_1_per_person"),

    i.water_container_category = case_when((hh_size > 0 & hh_size <4)& jerry_can_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
                                            (hh_size > 3 & hh_size <7)& jerry_can_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
                                            (hh_size > 6 & hh_size <10)&jerry_can_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
                                            (hh_size > 9)&jerry_can_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    i.number_water_container = ifelse(i.water_container_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                                            "10_or_more_HH_size"), jerry_can_num, NA_character_),

    i.tarpaulin_category = case_when((hh_size > 0 & hh_size <4)& tarpaulin_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
                                            (hh_size > 3 & hh_size <7)& tarpaulin_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
                                            (hh_size > 6 & hh_size <10)&tarpaulin_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
                                            (hh_size > 9)&tarpaulin_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    i.number_tarpaulin = ifelse(i.tarpaulin_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                                          "10_or_more_HH_size"), tarpaulin_num, NA_character_),

    i.solar_lamp_category = case_when((hh_size > 0 & hh_size <4)& solar_lamp_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
                                      (hh_size > 3 & hh_size <7)& solar_lamp_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
                                      (hh_size > 6 & hh_size <10)&solar_lamp_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
                                      (hh_size > 9)& solar_lamp_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    i.number_solar_lamp = ifelse(i.solar_lamp_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                             "10_or_more_HH_size"), solar_lamp_num, NA_character_),

    i.kitchen_set_category = case_when((hh_size > 0 & hh_size <4)& kitchen_set_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
                                      (hh_size > 3 & hh_size <7)& kitchen_set_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
                                      (hh_size > 6 & hh_size <10)&kitchen_set_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
                                      (hh_size > 9)& kitchen_set_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    i.number_kitchen_set = ifelse(i.kitchen_set_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                              "10_or_more_HH_size"), kitchen_set_num, NA_character_),

    int.monthly_meb_2001 = 440000/5,
    int.monthly_expenditure = calc_monthly_expenditure/hh_size,

    i.hh_avg_exp_vs_meb = ifelse(int.monthly_expenditure >= int.monthly_meb_2001,
                                   "monthly_expenditure_greater_than_meb", "monthly_expenditure_less_than_meb"),

    i.fcs = (cereal_grain_root_fcs*2 + pulses_fcs*3 + vegetables_fcs*1 + fruits_fcs*1 + meat_fcs*4 + milk_products_fcs*4 +
                                                                                  sugar_fcs*0.5 + oil_fats_fcs*0.5),
    i.fcs_category = case_when(i.fcs <= 21 ~ "Poor",
                          i.fcs <= 35 ~ "Borderline",
                          i.fcs <= 112 ~ "Acceptable")

    ) %>%

    select(-c(starts_with("int.")))
}
# mental health 

   create_composites_mental_health <- function(input_df) {
     input_df %>%
     mutate(i.mental_health_age_category = case_when(
                            individual_age > 11 & individual_age < 18 ~ "between_12_and_17_years",
                            individual_age > 17 & individual_age < 26 ~ "between_18_and_25_years",
                            individual_age > 25 & individual_age < 60 ~ "between_26_and_59_years",
                            individual_age > 59 ~ "greater_than_59_years",
                                               TRUE ~ NA_character_),

         i.hh_member_mh_by_age_group_and_gender = ifelse(feel_so_afraid %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_angry %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_uninterested_in_things %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_hopeless %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_severely_upset_about_bad_things_that_happened %in%c("all_of_the_time", "most_of_the_time")|
                             often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("all_of_the_time", "most_of_the_time"),
                            "mental_illness_yes", NA))
       # group_by(uuid) %>%



}

