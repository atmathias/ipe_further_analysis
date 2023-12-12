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


                i.hh_member_disability_by_age_group = ifelse(!is.na(i.hh_member_disability), i.hh_member_disability,
                                                             NA_character_),
           
               i.disability_age_group_5_12 = ifelse(i.disability_age_group %in% c("age_5_12"), 
                                                i.hh_member_disability, NA_character_), 
               i.disability_age_group_13_18 = ifelse(i.disability_age_group %in% c("age_13_18"), 
                                                    i.hh_member_disability, NA_character_), 
               i.disability_age_group_19_24 = ifelse(i.disability_age_group %in% c("age_19_24"), 
                                                    i.hh_member_disability, NA_character_), 
               i.disability_age_group_25_59 = ifelse(i.disability_age_group %in% c("age_25_59"), 
                                                    i.hh_member_disability, NA_character_), 
               i.disability_age_group_above_59 = ifelse(i.disability_age_group %in% c("age_greater_59"), 
                                                    i.hh_member_disability, NA_character_), 
                                                                                      
                # i.age_group_disability_yes = ifelse(i.hh_member_disability %in% c("yes_disability"), i.disability_age_group,
                #                                             NA_character_),
                # i.age_group_disability_no = ifelse(i.hh_member_disability %in% c("no_disability"), i.disability_age_group,
                #                                NA_character_),

                i.hoh_disability = case_when(relation_to_hoh %in% c("head_of_household") & i.hh_member_disability %in% c("yes_disability")~ "yes_disability",
                                             relation_to_hoh %in% c("head_of_household") & i.hh_member_disability %in% c("no_disability")~ "no_disability",
                                                                            TRUE ~ NA_character_),
                    # health indicators
                     
                i.hh_member_with_chronic_condition = case_when(hh_member_with_chronic_condition == 1694 ~ "yes",
                                                               hh_member_with_chronic_condition == 1695 ~ "no",
                                                               TRUE ~  NA_character_),
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
                i.hh_member_with_chronic_condition_access_healthcare  = case_when(hh_member_with_chronic_condition_access_healthcare
                                                                                  == 1708 ~ "yes",  hh_member_with_chronic_condition_access_healthcare
                                                                                  == 1709 ~ "no", TRUE ~ NA_character_),
              i.hh_member_with_chronic_condition_access_healthcare_by_age_group = i.hh_member_with_chronic_condition_access_healthcare,

                  # livelihoods indicators

            i.hh_member_occupation_age_group = case_when((progres_age >0 & progres_age <5) ~ "age_1_4",
                                                                (progres_age >4 & progres_age <12) ~ "age_5_11",
                                                                (progres_age >11 & progres_age <18) ~ "age_12_17",
                                                                (progres_age >17 & progres_age <26) ~ "age_18_25",
                                                                (progres_age >24 & progres_age <60) ~ "age_26_59",
                                                                (progres_age > 59)  ~ "age_greater_59",
                                                                TRUE ~ NA_character_),

           i.hh_member_occupation_pastyear_by_age_group = ifelse(!is.na(main_occupation_past_year), 
                                                                        main_occupation_past_year, NA_character_),
           
           i.hh_member_occupation_age_group_1_4 = ifelse(i.hh_member_occupation_age_group %in% c("age_1_4"), 
                                                          main_occupation_past_year, NA_character_),
           i.hh_member_occupation_age_group_5_11 = ifelse(i.hh_member_occupation_age_group %in% c("age_5_11"), 
                                                          main_occupation_past_year, NA_character_),
           i.hh_member_occupation_age_group_12_17 = ifelse(i.hh_member_occupation_age_group %in% c("age_12_17"),
                                                          main_occupation_past_year, NA_character_),
           i.hh_member_occupation_age_group_18_25 = ifelse(i.hh_member_occupation_age_group %in% c("age_18_25"), 
                                                          main_occupation_past_year, NA_character_),
           i.hh_member_occupation_age_group_26_59 = ifelse(i.hh_member_occupation_age_group %in% c("age_26_59"), 
                                                          main_occupation_past_year, NA_character_),
           i.hh_member_occupation_age_group_above_59 = ifelse(i.hh_member_occupation_age_group %in% c("age_greater_59"), 
                                                          main_occupation_past_year, NA_character_),
  
            i.hh_member_worked_past7days_age_category = case_when((progres_age >0 & progres_age <5) ~ "age_1_4",
                                                                (progres_age >4 & progres_age <12) ~ "age_5_11",
                                                                (progres_age >11 & progres_age <18) ~ "age_12_17",
                                                                (progres_age >17 & progres_age <26) ~ "age_18_25",
                                                                (progres_age >24 & progres_age <60) ~ "age_26_59",
                                                                (progres_age > 59)  ~ "age_greater_59",
                                                                TRUE ~ NA_character_),

           i.hh_member_worked_past7days_by_age_group = ifelse(!is.na(worked_in_past_7_days), worked_in_past_7_days,
                                                              NA_character_),
           int.worked_in_past_7_days = case_when(worked_in_past_7_days == 1725 ~ "Worked at least one hour for pay or profit",
                                                 worked_in_past_7_days == 1725 ~ "Did not work, but was actively searching for work",
                                                 worked_in_past_7_days == 1725 ~ "Did not work, and was not actively searching for work"),
           
           i.hh_member_worked_past7days_age_group_1_4 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_1_4"), 
                                                         int.worked_in_past_7_days, NA_character_),
           i.hh_member_worked_past7days_age_group_5_11 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_5_11"), 
                                                          int.worked_in_past_7_days, NA_character_),
           i.hh_member_worked_past7days_age_group_12_17 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_12_17"),
                                                           int.worked_in_past_7_days, NA_character_),
           i.hh_member_worked_past7days_age_group_18_25 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_18_25"), 
                                                           int.worked_in_past_7_days, NA_character_),
           i.hh_member_worked_past7days_age_group_26_59 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_26_59"), 
                                                           int.worked_in_past_7_days, NA_character_),
           i.hh_member_worked_past7days_age_group_above_59 = ifelse(i.hh_member_worked_past7days_age_category %in% c("age_greater_59"), 
                                                              int.worked_in_past_7_days, NA_character_),           

           # i.hh_member_worked_atleast_1hr_for_pay_or_profit = ifelse(int.worked_in_past_7_days %in% c("Worked at least one hour for pay or profit"), i.hh_member_worked_past7days_age_category,
           #                                                    NA_character_),
           # i.hh_member_didnt_work_but_actively_searched_for_work = ifelse(int.worked_in_past_7_days %in% c("Worked at least one hour for pay or profit"), i.hh_member_worked_past7days_age_category,
           #                                                    NA_character_),
           # i.hh_member_neither_worked_nor_searched_for_work = ifelse(int.worked_in_past_7_days %in% c("Did not work, and was not actively searching for work"), i.hh_member_worked_past7days_age_category,
           # NA_character_),


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
                # calculating dependency ratio
              i.age_dependant = ifelse((progres_age >= 0 & progres_age <= 14) | (progres_age >= 65 & progres_age <= 100), 1, 0),
              i.age_independent = ifelse((progres_age >= 15 & progres_age <= 64), 1, 0),


  # i.lcsi_hhs
  # i.top_priority_needs = hh
  i.most_commonly_hh_need_rank_1 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_1, NA_character_),
  i.most_commonly_hh_need_rank_2 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_2, NA_character_),
  i.most_commonly_hh_need_rank_3 = ifelse(progres_relationshiptofpname %in% c("Focal Point"), most_commonly_hh_need_rank_3, NA_character_),

  i.school_going_age_group = case_when((progres_age >=3 & progres_age <=5) ~ "age_3_5",
                                     (progres_age >=6 & progres_age <=12) ~ "age_6_12",
                                     (progres_age >=13 & progres_age <=18) ~ "age_13_18",
                                     (progres_age >=19 & progres_age <=24) ~ "age_19_24"),

  i.children_attending_school_by_age_group = ifelse(!is.na(attending_school_now), attending_school_now,
                                                    NA_character_),
  
  i.hh_children_attending_school_age_group_3_5 = ifelse(i.school_going_age_group %in% c("age_3_5"), 
                                                        attending_school_now, NA_character_),
  i.hh_children_attending_school_age_group_6_12 = ifelse(i.school_going_age_group %in% c("age_6_12"), 
                                                         attending_school_now, NA_character_),
  i.hh_children_attending_school_age_group_13_18 = ifelse(i.school_going_age_group %in% c("age_13_18"),
                                                          attending_school_now, NA_character_),
  i.hh_children_attending_school_age_group_19_24 = ifelse(i.school_going_age_group %in% c("age_19_24"), 
                                                          attending_school_now, NA_character_)) %>% 
  
  # i.children_attending_school_yes = ifelse(attending_school_now %in% c(1694), i.school_going_age_group,
  #                                                 NA_character_),
  # i.children_attending_school_no = ifelse(attending_school_now %in% c(1695), i.school_going_age_group,
  #                                                  NA_character_)) %>% 
  
      select(-c(starts_with("int.")))

}
 # creating composites sampled

create_composites_sampled <- function(input_df) {
  input_df %>%
    mutate(region = case_when(settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                              settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya",
                                                "Rhino") ~ "West Nile"),
    i.hoh_by_gender = ifelse(relation_to_hoh %in% c("head_of_household"), gender, NA_character_),

      # wash
      
    i.hh_main_water_source = case_when(main_water_source %in% c("dug_well_unprotected", "spring_unprotected", 
                                                  "dug_well_unprotected", "surface_water_river_dam_lake_pond_stream_canal") ~ "using_unimproved_waters_yes",
                                       main_water_source %in% c("bottled_water", "dug_well_protected", "spring_protected",
                                                  "tube_wellborehole_handpump", "water_piped_into_the_dwellingplot",
                                                  "water_tank_where_rainwater_is_collected_protected") ~ "using_unimproved_waters_no",
                                                                TRUE ~ NA_character_), 
    
    i.hh_latrine_type = case_when(latrine_type %in% c("covered_pit_latrine_with_a_slab", "ventilated_improved_pit_latrine", 
                                                    "ecosan_compost_toilet", "flush_toilet") ~ "using_unimproved_latrine_no",
                                  latrine_type %in% c("uncovered_pit_latrine_without_a_slab", "covered_pit_latrine_without_a_slab", 
                                                      "uncovered_pit_latrine_with_a_slab") ~ "using_unimproved_latrine_yes",
                                                                TRUE ~ NA_character_),
    
      
    i.total_water_volume_per_person = case_when(calc_total_volume_per_person < 15 ~ "less_than_20L_per_person_per_day",
                                                  calc_total_volume_per_person == 50 ~ "20L_per_person_per_day",
                                                  calc_total_volume_per_person > 15 ~ "more_than_20L_per_person_per_day"),

    i.number_of_minutes_to_and_from_water_source = case_when(number_of_minutes_to_and_from_water_source < 30 ~ "less_than_30_min",
                                                             (number_of_minutes_to_and_from_water_source >= 30) ~ "between_30_and_60_min",
                                                             number_of_minutes_to_and_from_water_source > 60 ~ "more_than_60_min",
                                                             TRUE ~ NA_character_),

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

    # i.water_container_category = case_when((hh_size > 0 & hh_size <4)& jerry_can_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
    #                                         (hh_size > 3 & hh_size <7)& jerry_can_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
    #                                         (hh_size > 6 & hh_size <10)&jerry_can_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
    #                                         (hh_size > 9)&jerry_can_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    # i.number_water_container = case_when(i.water_container_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
    #                                                                     "10_or_more_HH_size") ~ jerry_can_num),
    #                                                                     
    # i.number_water_container = as.numeric(i.number_water_container),                                                                       
    # i.median_water_container = median(i.number_water_container, na.rm = TRUE),
    # 
    # i.plastic_bucket_category = case_when((hh_size > 0 & hh_size <4)& plastic_bucket_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
    #                                        (hh_size > 3 & hh_size <7)& plastic_bucket_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
    #                                        (hh_size > 6 & hh_size <10)&plastic_bucket_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
    #                                        (hh_size > 9)&plastic_bucket_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    # i.number_plastic_bucket = ifelse(i.plastic_bucket_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
    #                                                                     "10_or_more_HH_size"), plastic_bucket_num, NA_character_),
    # i.number_plastic_bucket = as.numeric(i.number_plastic_bucket),
    # i.median_plastic_bucket = median(i.number_plastic_bucket, na.rm = TRUE),
    # 
    # 
    # i.tarpaulin_category = case_when((hh_size > 0 & hh_size <4)& tarpaulin_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
    #                                         (hh_size > 3 & hh_size <7)& tarpaulin_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
    #                                         (hh_size > 6 & hh_size <10)&tarpaulin_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
    #                                         (hh_size > 9)&tarpaulin_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    # i.number_tarpaulin = ifelse(i.tarpaulin_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
    #                                                                      "10_or_more_HH_size"), tarpaulin_num, NA_character_),
    #   
    # i.number_tarpaulin = as.numeric(i.number_tarpaulin),
    # i.median_number_tarpaulin = median(i.number_tarpaulin, na.rm = TRUE),
    # 
    # i.solar_lamp_category = case_when((hh_size > 0 & hh_size <4)& solar_lamp_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
    #                                   (hh_size > 3 & hh_size <7)& solar_lamp_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
    #                                   (hh_size > 6 & hh_size <10)&solar_lamp_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
    #                                   (hh_size > 9)& solar_lamp_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
    # i.number_solar_lamp = ifelse(i.solar_lamp_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
    #                                                          "10_or_more_HH_size"), solar_lamp_num, NA_character_),
    # i.number_solar_lamp = as.numeric(i.number_solar_lamp),
    # i.median_number_solar_lamp = median(i.number_solar_lamp, na.rm = TRUE),
    # 
    # i.kitchen_set_category = case_when((hh_size > 0 & hh_size <4)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_1_and_3_HH_size",
    #                                    (hh_size > 3 & hh_size <7)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_4_and_6_HH_size",
    #                                    (hh_size > 6 & hh_size <10)& (kitchen_set_cond %in%c("good", "moderate")) ~ "between_7_and_9_HH_size",
    #                                    (hh_size > 9)& (kitchen_set_cond %in%c("good", "moderate")) ~ "10_or_more_HH_size"),
    # i.number_kitchen_set = ifelse(i.kitchen_set_category %in% c("between_1_and_3_HH_size", "between_4_and_6_HH_size", "between_7_and_9_HH_size",
    #                                                             "10_or_more_HH_size"), kitchen_set_num, NA_character_),
    # 
    # i.median_number_kitchen_set = median(i.number_kitchen_set, na.rm = TRUE),
    # 

    int.ind_monthly_meb_2001 = 440000/5,
    int.ind_monthly_expenditure = calc_monthly_expenditure/hh_size,

    i.hh_avg_exp_vs_meb = case_when(int.ind_monthly_expenditure > int.ind_monthly_meb_2001 ~ "monthly_expenditure_greater_than_meb",
                                    int.ind_monthly_expenditure < int.ind_monthly_meb_2001 ~ "monthly_expenditure_less_than_meb",
                                    int.ind_monthly_expenditure == int.ind_monthly_meb_2001 ~ "monthly_expenditure_equals_meb"),

    i.fcs = (cereal_grain_root_fcs*2 + pulses_fcs*3 + vegetables_fcs*1 + fruits_fcs*1 + meat_fcs*4 + milk_products_fcs*4 +
                                                                                  sugar_fcs*0.5 + oil_fats_fcs*0.5),
    i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
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

         i.hh_member_mh_state = case_when(feel_so_afraid %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_angry %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_uninterested_in_things %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_hopeless %in%c("all_of_the_time", "most_of_the_time")|
                             feel_so_severely_upset_about_bad_things_that_happened %in%c("all_of_the_time", "most_of_the_time")|
                             often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("all_of_the_time", "most_of_the_time") ~
                            "mental_illness_yes",
                             feel_so_afraid %in%c("none_of_the_time")|
                             feel_so_angry %in%c("none_of_the_time")|
                             feel_so_uninterested_in_things %in%c("none_of_the_time")|
                             feel_so_hopeless %in%c("none_of_the_time")|
                             feel_so_severely_upset_about_bad_things_that_happened %in%c("none_of_the_time")|
                             often_unable_to_carry_out_essential_activities_due_to_feelings %in%c("none_of_the_time") ~
                             "mental_illness_no",  TRUE ~ NA_character_),

         i.hh_member_mh_by_age_group = ifelse(!is.na(i.hh_member_mh_state), i.hh_member_mh_state,
                                                                        NA_character_),
         
         i.hh_mental_health_age_group_12_17 = ifelse(i.mental_health_age_category %in% c("between_12_and_17_years"), 
                                                     i.hh_member_mh_state, NA_character_),                                   
         i.hh_mental_health_age_group_18_25 = ifelse(i.mental_health_age_category %in% c("between_18_and_25_years"), 
                                                     i.hh_member_mh_state, NA_character_),                                   
         i.hh_mental_health_age_group_26_59 = ifelse(i.mental_health_age_category %in% c("between_26_and_59_years"), 
                                                     i.hh_member_mh_state, NA_character_),                                   
         i.hh_mental_health_age_group_above_59 = ifelse(i.mental_health_age_category %in% c("greater_than_59_years"), 
                                                     i.hh_member_mh_state, NA_character_) 
                                              
                                              
         # i.hh_member_mental_illness_yes = ifelse(i.hh_member_mh_state %in% c("mental_illness_yes"), i.mental_health_age_category,
         #                                           NA_character_),
         # i.hh_member_mental_illness_no = ifelse(i.hh_member_mh_state %in% c("mental_illness_no"), i.mental_health_age_category,
                                                           # NA_character_)

     ) %>%

     select(-c(starts_with("int.")))
}
                                          
  
   