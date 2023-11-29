# creating composite indicators -------------------------------------------

create_composites_verification <- function(input_df) {
  input_df %>% 
    # demographic indicators
    mutate(region = case_when(settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                              settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", 
                                                         "Rhino") ~ "West Nile"),
                 i.hoh_by_gender = ifelse(relation_to_hoh %in% c("head_of_household"), gender, NA_character_),
                  i.hoh_single_female = ifelse(gender %in% c("Female") & relation_to_hoh %in% c("head_of_household") &
                                                progres_maritalstatusname %in% c("Single", "Divorced", "Separated", "Widowed"), 
                                               "yes", "no"),
                  i.hoh_child = ifelse(progres_age <= 17 & relation_to_hoh %in% c("head_of_household"), "yes", "no"),
                  i.disability_age_group = case_when((progres_age >=5 & progres_age <13) ~ "age_5_12",
                                                     (progres_age >12 & progres_age <19) ~ "age_13_18",
                                                     (progres_age >18 & progres_age <25) ~ "age_19_24",
                                                     (progres_age >=25 & progres_age <60) ~ "age_25_59",
                                                     (progres_age > 59)  ~ "age_greater_59",
                                                     TRUE ~ NA_character_),
                 i.hh_member_disability_by_age_group_and_gender =  case_when(difficulty_seeing %in% c(1706,1707)|difficulty_hearing %in% c(1706 , 1707)|
                                                       difficulty_walking %in% c(1706 , 1707)|difficulty_remembering %in% c(1706 , 1707)|
                                                       difficulty_selfcare %in% c(1706 , 1707)|difficulty_communicating %in% c(1706 , 1707)~ "yes_disability", 
                                                       difficulty_seeing %in% c(1704 , 1705)|difficulty_hearing %in% c(1704 , 1705)|
                                                       difficulty_walking %in% c(1704 , 1705)|difficulty_remembering %in% c(11704 , 1705)|
                                                       difficulty_selfcare %in% c(1704 , 1705)|difficulty_communicating %in% c(1704 , 1705)~
                                                       "no_disability", TRUE ~ NA_character_),
                  
                  i.hh_with_disabled_member =  case_when(difficulty_seeing %in% c(1706 , 1707)|difficulty_hearing %in% c(1706 , 1707)|
                                                           difficulty_walking %in% c(1706 , 1707)|difficulty_remembering %in% c(1706 , 1707)|
                                                           difficulty_selfcare %in% c(1706 , 1707)|difficulty_communicating %in% c(1706 , 1707)~ "yes_disability", 
                                                         difficulty_seeing %in% c(1704 , 1705)|difficulty_hearing %in% c(1704 , 1705)|
                                                           difficulty_walking %in% c(1704 , 1705)|difficulty_remembering %in% c(11704 , 1705)|
                                                           difficulty_selfcare %in% c(1704 , 1705)|difficulty_communicating %in% c(1704 , 1705)~
                                                           "no_disability", TRUE ~ NA_character_),
                i.hoh_disability = case_when(relation_to_hoh %in% c("head_of_household") & i.hh_with_disabled_member %in% c("yes_disability")~ "yes_disability", 
                                            relation_to_hoh %in% c("head_of_household") & i.hh_with_disabled_member %in% c("no_disability")~ "no_disability", 
                                                                            TRUE ~ NA_character_),
                    # health indicators
                                                                                    # medical_condition_lasted_3_months == "1695" ~ "no",  TRUE ~ NA_character_),
                    i.chronic_illness_age_group = case_when((progres_age >= 0 & progres_age <= 2) ~ "age_0_2",
                                                            (progres_age >= 3 & progres_age <=5) ~ "age_3_5",
                                                            (progres_age >=6 & progres_age <=12) ~ "age_6_12",
                                                            (progres_age >=13 & progres_age <=18) ~ "age_13_18",
                                                            (progres_age >=19 & progres_age <=24) ~ "age_19_24",
                                                            (progres_age >=25 & progres_age <=59) ~ "age_25_59",
                                                            (progres_age >= 59)  ~ "age_greater_59",
                                                            TRUE ~ NA_character_),
           
               i.hh_member_with_chronic_condition_by_age_group = case_when(hh_member_with_chronic_condition == 1694 ~ "yes", 
                                                                        hh_member_with_chronic_condition == 1695 ~ "no",
                                                                        TRUE ~  NA_character_),
                  
                 i.hh_member_with_chronic_condition = hh_member_with_chronic_condition,

                   # i.hh_member_with_chronic_condition_by_age_group = ifelse(31 %in% c(1694), "yes", "no"),

        i.hh_member_with_chronic_condition_access_healthcare_by_age_group = case_when(hh_member_with_chronic_condition_access_healthcare 
                                                                        == 1708 ~ "yes",  
                                                                        hh_member_with_chronic_condition_access_healthcare 
                                                                        == 1709 ~ "no", TRUE ~ NA_character_),
                   
                  # livelihoods indicators
                  
                   i.hh_member_occupation_age_group = case_when((progres_age >0 & progres_age <5) ~ "age_1_4",
                                                                (progres_age >4 & progres_age <12) ~ "age_5_11",
                                                                (progres_age >11 & progres_age <18) ~ "age_12_17",
                                                                (progres_age >17 & progres_age <26) ~ "age_18_25",
                                                                (progres_age >24 & progres_age <60) ~ "age_26_59",
                                                                (progres_age > 59)  ~ "age_greater_59",
                                                                TRUE ~ NA_character_),
           
                   i.hh_member_occupation_by_age_group_and_gender = main_occupation_past_year,
           
           i.hh_member_worked_past7days_age_group = case_when((progres_age >= 0 & progres_age <= 2) ~ "age_0_2",
                                                              (progres_age >= 3 & progres_age <=5) ~ "age_3_5",
                                                              (progres_age >=6 & progres_age <12) ~ "age_6_12",
                                                              (progres_age >=13 & progres_age <18) ~ "age_13_18",
                                                              (progres_age >19 & progres_age <24) ~ "age_19_24",
                                                              (progres_age >25 & progres_age <59) ~ "age_25_59",
                                                              (progres_age > 59)  ~ "age_greater_59",
                                                              TRUE ~ NA_character_),
                   
           i.hh_member_worked_past7days_by_age_group_and_gender =  hh_member_worked_past7days,
           
                   # protection
                   i.hh_with_child_outside_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1694) ~ "yes",
                                                              progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1695) ~ "no",
                                                              TRUE ~ NA_character_),

                   i.hh_with_child_outside_of_home_by_location = ifelse(i.hh_with_child_outside_of_home %in%c("yes"), 
                                                                        where_children_are_living, NA_character_),
             
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
                                                        TRUE ~ NA_character_),
    i.age_dependant = ifelse((progres_age >= 0 & progres_age <= 14) | (progres_age >= 65 & progres_age <= 100), 1, 0),
    i.age_independent = ifelse((progres_age >= 15 & progres_age <= 64), 1, 0),
    
    i.lcsi_cat = case_when((spent_savings %in% c(1816, 1817)|bought_food_on_credit_or_borrowed_money_for_food %in% c(1816, 1817)|
                           borrowed_money_to_cover_basic_needs_health_rent %in% c(1816, 1817)|sold_household_goods %in% c(1816, 1817)|
                           sold_sanitary_materials %in% c(1816, 1817)|changed_accommodation_to_reduce_expenditures %in% c(1816, 1817)|
                           sold_more_non_productive_animals_than_usual %in% c(1816, 1817)|relied_on_less_preferred__less_expensive_food %in% c(1816, 1817)|
                           borrowed_food_or_relied_on_help %in% c(1816, 1817)) ~ "hh_lcsi_stress",
                          (reduced_essential_non_food_expenditures_such_as_education_health %in% c(1816, 1817)|sold_productive_assets_or_means_of_transport %in% c(1816, 1817)|consume_seed_stock_held_for_next_season %in% c(1816, 1817)|
                           harvested_immature_crops %in% c(1816, 1817)| rented_out_the_house %in% c(1816, 1817)|sent_hh_members_to_eat_elsewhere %in% c(1816, 1817)|
                           sent_children_to_work %in% c(1816, 1817)|withdrew_children_from_school %in% c(1816, 1817)|
                           reduced_numbers_of_meals_eaten_per_day %in% c(1816, 1817)|reduced_portion_size_of_meals %in% c(1816, 1817)|
                           restricted_consumption_of_adults_for_children %in% c(1816, 1817))~ "hh_lcsi_crisis",
                         (female_members_under_18_got_married %in% c(1816, 1817)|74 %in% c(1816, 1817)|sold_last_female_animals %in% c(1816, 1817)|accepted_high_risk_illegal_exploitative_temporary_jobs %in% c(1816, 1817)|
                           engaged_in_transactional_and_survival_sex %in% c(1816, 1817)|sent_household_members_to_beg %in% c(1816, 1817))~ 
                          "hh_lcsi_emergency", TRUE ~ "hh_lcsi_none"),
    
  # i.lcsi_hhs 
  # i.top_priority_needs = 
  
  i.children_attending_school = 118,
  
  i.school_going_age_group = case_when((progres_age >=3 & progres_age <=5) ~ "age_3_5",
                                     (progres_age >=6 & progres_age <=12) ~ "age_6_12",
                                     (progres_age >=13 & progres_age <=18) ~ "age_13_18",
                                     (progres_age >=19 & progres_age <=24) ~ "age_19_24"),
  
  i.children_attending_school_by_age_group_and_gender = i.school_going_age_group) %>% 
  
                                     
      select(-c(starts_with("int.")))
      
}
 # creating composites sampled

create_composites_sampled <- function(input_df) {
  input_df %>%
    mutate(region = case_when(settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                              settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya",
                                                "Rhino") ~ "West Nile"),
      # wash
    i.total_water_volume_per_person = case_when(calc_total_volume_per_person < 15 ~ "less_than_15L_per_person_per_day",
                                               calc_total_volume_per_person == 50 ~ "15L_per_person_per_day",
                                               calc_total_volume_per_person > 15 ~ "more_than_15L_per_person_per_day"),
    # shelter and nfi
    int.sleeping_mat_num_average = sleeping_mat_num/hh_size,
    i.sleeping_mat_num_average = case_when(int.sleeping_mat_num_average < 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                         int.sleeping_mat_num_average == 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "1_per_person",
                                         int.sleeping_mat_num_average > 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "above_1_per_person"),

    int.blanket_num_average = blanket_num/hh_size,
    i.blanket_num_average = case_when(int.blanket_num_average < 1 & blanket_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                     int.blanket_num_average == 1 & blanket_cond %in% c("good", "moderate") ~ "1_per_person",
                                     int.blanket_num_average > 1 & blanket_cond %in% c("good", "moderate") ~ "above_1_per_person"),

    int.mosquito_net_num_average = mosquito_net_num/hh_size,
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

    i.kitchen_set_category = case_when((hh_size > 0 & hh_size <4)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_1_and_3_HH_size",
                                      (hh_size > 3 & hh_size <7)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_4_and_6_HH_size",
                                      (hh_size > 6 & hh_size <10)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_7_and_9_HH_size",
                                      (hh_size > 9)& (kitchen_set_cond %in%c("good", "moderate")) ~ "10_or_more_HH_size"),
    i.number_kitchen_set = ifelse(i.kitchen_set_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
                                                              "10_or_more_HH_size"), kitchen_set_num, NA_character_),
    
    i.number_of_minutes_to_and_from_water_source = case_when(number_of_minutes_to_and_from_water_source <= 30 ~ "30_min_or_less",
                                        (number_of_minutes_to_and_from_water_source > 30 & 
                                        number_of_minutes_to_and_from_water_source <= 60) ~ "above_30mins_below_1hr",
                                        number_of_minutes_to_and_from_water_source > 60 ~ "more_than_1hr",
                                        TRUE ~ NA_character_),
    
    
    
   
    # int.monthly_meb_2001 = 440000/5,
    # int.monthly_expenditure = calc_monthly_expenditure/hh_size,
    # 
    # i.hh_avg_exp_vs_meb = ifelse(int.monthly_expenditure >= int.monthly_meb_2001,
    #                                "monthly_expenditure_greater_than_meb", "monthly_expenditure_less_than_meb"),

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
                            (individual_age > 11 & individual_age < 18) ~ "between_12_and_17_years",
                            (individual_age > 17 & individual_age < 26) ~ "between_18_and_25_years",
                            (individual_age > 25 & individual_age < 60) ~ "between_26_and_59_years",
                            (individual_age > 59) ~ "greater_than_59_years",
                                               TRUE ~ NA_character_),

         i.hh_member_mh_by_age_group_and_gender = case_when(feel_so_afraid %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_angry %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_uninterested_in_things %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_hopeless %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_severely_upset_about_bad_things_that_happened %in%c("all_of_the_time", "most_of_the_time")|
                             often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("all_of_the_time", "most_of_the_time") ~
                            "mental_illness_yes", 
                             feel_so_afraid %in%c(",a_little_of_the_time", "some_of_the_time")|
                             feel_so_angry %in%c(",a_little_of_the_time", "some_of_the_time")|
                             feel_so_uninterested_in_things %in%c(",a_little_of_the_time", "some_of_the_time")|
                             feel_so_hopeless %in%c(",a_little_of_the_time", "some_of_the_time")|
                             feel_so_severely_upset_about_bad_things_that_happened %in%c(",a_little_of_the_time", "some_of_the_time")|
                             often_unable_to_carry_out_essential_activities_due_to_feelings %in%c(",a_little_of_the_time", "some_of_the_time") ~
                             "mental_illness_mild",  TRUE ~ "none") %>% 
     
         # i.hh_member_mh_by_age_group_and_gender = case_when(feel_so_afraid %in%c("all_of_the_time", "most_of_the_time")|
         #                      feel_so_angry %in%c("all_of_the_time", "most_of_the_time")|
         #                      feel_so_uninterested_in_things %in%c("all_of_the_time", "most_of_the_time")|
         #                      feel_so_hopeless %in%c("all_of_the_time", "most_of_the_time")|
         #                      feel_so_severely_upset_about_bad_things_that_happened %in%c("all_of_the_time", "most_of_the_time")|
         #                      often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("all_of_the_time", "most_of_the_time") ~
         #                      "mental_illness_yes", 
         #                    feel_so_afraid %in%c(",a_little_of_the_time", "some_of_the_time")|
         #                      feel_so_angry %in%c(",a_little_of_the_time", "some_of_the_time")|
         #                      feel_so_uninterested_in_things %in%c(",a_little_of_the_time", "some_of_the_time")|
         #                      feel_so_hopeless %in%c(",a_little_of_the_time", "some_of_the_time")|
         #                      feel_so_severely_upset_about_bad_things_that_happened %in%c(",a_little_of_the_time", "some_of_the_time")|
         #                      often_unable_to_carry_out_essential_activities_due_to_feelings %in%c(",a_little_of_the_time", "some_of_the_time") ~
         #                      "mental_illness_mild",  TRUE ~ "none")) %>% 
         group_by(uuid) %>% 
           summarise(
             int.hh_mh_entries = paste(i.hh_member_mh_by_age_group_and_gender, collapse = " : ")
           ) %>% 
           mutate(i.hh_mh_entries =  case_when(str_detect(string = int.hh_mh_entries, 
                                                          pattern = "mental_illness_yes") ~ "mental_illness_yes",
                                               str_detect(string = int.hh_mh_entries, 
                                                          pattern = "mental_illness_mild") ~ "mental_illness_mild",
                                               str_detect(string = int.hh_mh_entries, 
                                                          pattern = "none") ~ "none"))
}

