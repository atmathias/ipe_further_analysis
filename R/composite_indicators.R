# creating composite indicators -------------------------------------------

create_composites_verification <- function(input_df) {
  input_df %>% 
    # demographic indicators
    mutate(region = case_when(settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                              settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", 
                                                         "Rhino") ~ "West Nile"),
                  i.hoh_by_gender = ifelse(relation_to_hoh %in% c("head_of_household"), gender, NA_character_),
                 
                  i.disability_age_group = case_when((progres_age >=5 & progres_age <13) ~ "age_5_12",
                                                     (progres_age >12 & progres_age <19) ~ "age_13_18",
                                                     (progres_age >18 & progres_age <25) ~ "age_19_24",
                                                     (progres_age >=25 & progres_age <60) ~ "age_25_59",
                                                     (progres_age > 59)  ~ "age_greater_59",
                                                     TRUE ~ NA_character_),
                 i.hh_member_disability =  case_when(difficulty_seeing %in% c(1706,1707)|difficulty_hearing %in% c(1706 , 1707)|
                                                       difficulty_walking %in% c(1706 , 1707)|difficulty_remembering %in% c(1706 , 1707)|
                                                       difficulty_selfcare %in% c(1706 , 1707)|difficulty_communicating %in% c(1706 , 1707)~ "yes_disability", 
                                                       difficulty_seeing %in% c(1704 , 1705)|difficulty_hearing %in% c(1704 , 1705)|
                                                       difficulty_walking %in% c(1704 , 1705)|difficulty_remembering %in% c(11704 , 1705)|
                                                       difficulty_selfcare %in% c(1704 , 1705)|difficulty_communicating %in% c(1704 , 1705)~
                                                       "no_disability", TRUE ~ NA_character_),
           
                i.hh_member_disability_by_age_group = i.hh_member_disability,
                i.hh_member_disability_by_gender = i.hh_member_disability,
                  
                i.hoh_disability = case_when(relation_to_hoh %in% c("head_of_household") & i.hh_member_disability %in% c("yes_disability")~ "yes_disability", 
                                             relation_to_hoh %in% c("head_of_household") & i.hh_member_disability %in% c("no_disability")~ "no_disability", 
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
              i.hh_member_with_chronic_condition_access_healthcare  = hh_member_with_chronic_condition_access_healthcare,
              
                 
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
           
             i.hh_member_occupation_pastyear = main_occupation_past_year,
             i.hh_member_occupation_pastyear_by_age = i.hh_member_occupation_pastyear,
             i.hh_member_occupation_pastyear_by_gender = i.hh_member_occupation_pastyear,
           
             i.hh_member_worked_past7days_age_group = case_when((progres_age >= 0 & progres_age <= 2) ~ "age_0_2",
                                                              (progres_age >= 3 & progres_age <=5) ~ "age_3_5",
                                                              (progres_age >=6 & progres_age <12) ~ "age_6_12",
                                                              (progres_age >=13 & progres_age <18) ~ "age_13_18",
                                                              (progres_age >19 & progres_age <24) ~ "age_19_24",
                                                              (progres_age >25 & progres_age <59) ~ "age_25_59",
                                                              (progres_age > 59)  ~ "age_greater_59",
                                                              TRUE ~ NA_character_),
              i.hh_member_worked_past7days_by_age = hh_member_worked_past7days,
              i.hh_member_worked_past7days_by_gender = hh_member_worked_past7days,
               
           
                   # protection
                   
                  i.avg_time_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1759) ~ "Between 1 and 13 hours",
                                              progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1760) ~ "Between 14 and 42 hours",
                                              progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(1761) ~ "more than 42",
                                              progres_relationshiptofpname %in% c("Focal Point") & avg_time_child_working_payment %in% c(2473) ~ "No kid working",
                                              TRUE ~ NA_character_),
      i.avg_time_children_worked_HHchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1759, 2488) ~ "Between 1 and 14 hours",
                                              progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1760, 2489) ~ "Between 14 and 27 hours",
                                              progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(2490) ~ "Between 28 and 42 hours",
                                              progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(1761, 2491) ~ "More than 43",
                                              progres_relationshiptofpname %in% c("Focal Point") & avt_working_hh %in% c(2473, 2492) ~ "No kid working",
                                                      TRUE ~ NA_character_),
                
    i.age_dependant = ifelse((progres_age >= 0 & progres_age <= 14) | (progres_age >= 65 & progres_age <= 100), 1, 0),
    i.age_independent = ifelse((progres_age >= 15 & progres_age <= 64), 1, 0),
    
    
  # i.lcsi_hhs 
  # i.top_priority_needs = 
  i.most_commonly_hh_need_rank_1 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_1, NA_character_),
  i.most_commonly_hh_need_rank_2 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_2, NA_character_),
  i.most_commonly_hh_need_rank_3 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_3, NA_character_),
  
  i.children_attending_school = children_attending_school,
  
  i.school_going_age_group = case_when((progres_age >=3 & progres_age <=5) ~ "age_3_5",
                                     (progres_age >=6 & progres_age <=12) ~ "age_6_12",
                                     (progres_age >=13 & progres_age <=18) ~ "age_13_18",
                                     (progres_age >=19 & progres_age <=24) ~ "age_19_24"),
  
  i.children_attending_school_by_age = children_attending_school,
  i.children_attending_school_by_gender = children_attending_school) %>% 
  
  
 
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
                             "mental_illness_mild",  TRUE ~ "none")) %>% 
     
     select(-c(starts_with("int.")))
}

