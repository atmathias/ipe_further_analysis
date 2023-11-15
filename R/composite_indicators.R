# creating composite indicators -------------------------------------------


create_composites_verification <- function(input_df) {
  input_df %>% 
    # demographic indicators
    mutate(location_region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                                                                   settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                                                   settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", "Rhino") ~ "West Nile"),
                                       i.hoh_by_gender = ifelse(progres_relationshiptofpname %in% c("Focal Point"), progres_sexname, NA_character_),
           i.hoh_single_female = ifelse(progres_sexname %in% c("Female") & progres_relationshiptofpname %in% c("Focal Point") & 
                                         progres_maritalstatusname %in% c("Single"), "yes", "no"),
           i.hoh_child = ifelse(progres_age <= 17 & progres_relationshiptofpname %in% c("Focal Point"), "yes", "no"),
           i.disability_age_group = case_when(progres_age %in% c(5:12) ~ "age_5_12",
                                              progres_age %in% c(13:18) ~ "age_13_18",
                                              progres_age %in% c(19:24) ~ "age_19_24",
                                              progres_age %in% c(25:59) ~ "age_25_59",
                                              progres_age %in% c(60:100) ~ "age_greater_59"),
           i.hh_member_disability_by_age_group_and_gender =  ifelse(26 %in% c(1706, 1707)|27 %in% c(1706, 1707)|14 %in% c(1706, 1707)|28 %in% c(1706, 1707)|
                                  16 %in% c(1706, 1707)|29 %in% c(1706, 1707),"yes_disability", "no_disability"),
           i.hh_with_disabled_member =  ifelse(26 %in% c(1706, 1707)|27 %in% c(1706, 1707)|14 %in% c(1706, 1707)|28 %in% c(1706, 1707)|
                                          16 %in% c(1706, 1707)|29 %in% c(1706, 1707),"yes_disability", "no_disability"),
           i.hoh_disability = ifelse(progres_relationshiptofpname & c("Focal Point") & i.disability_hhs %in% c("yes_disability"), 
                                     "yes_disability", "no_disability"),
             # health indicators
           i.hh_member_with_chronic_condition = ifelse(31 %in% c(1694), "yes", "no"),
           
           i.chronic_illness_age_group = case_when(progres_age %in% c(0:2) ~ "age_0_2",
                                              progres_age %in% c(3:5) ~ "age_3_5",
                                              progres_age %in% c(6:12) ~ "age_6_12",
                                              progres_age %in% c(13:18) ~ "age_13_18",
                                              progres_age %in% c(19:24) ~ "age_19_24",
                                              progres_age %in% c(25:59) ~ "age_25_59",
                                              progres_age %in% c(60:100) ~ "age_greater_59"),
           
           i.hh_member_with_chronic_condition_by_age_group = ifelse(31 %in% c(1694), "yes", "no"),
           
           i.hh_member_with_chronic_condition_access_healthcare = case_when(32 == 1708 ~ "yes",
                                                                          32 == 1709 ~ "no",  TRUE ~ NA_character_),
           i.hh_member_with_chronic_condition_access_healthcare_by_age_group = case_when(32 == 1708 ~ "yes",
                                                                                       32 == 1709 ~ "no",
                                                                                       TRUE ~ NA_character_),
           # livelihoods indicators
           # i.main_occupation_past_year = case_when(36 == 2438 ~ "no_occupation_student", 36 == 2441 ~ "child_care_workers",
           #                                       36 == 2442 ~ "cleaners_office_helpers", 36 == 2443 ~ "cooks",
           #                                       36 == 2444 ~ "crop_farm_labourers",     36 == 2447 ~ "electronic_mechanics",
           #                                       36 == 2446 ~ "domestic_housekeepers", 36 == 2449 ~ "field_crop_growers",
           #                                       36 == 2451 ~ "house_builders",        36 == 2453 ~ "livestock_producers",
           #                                       36 == 2455 ~ "mining", 36 == 2456 ~ "no_occupation_home_maker",
           #                                       36 == 2457 ~ "no_occupation_unemployed", 36 == 2458 ~ "retail_trade_managers",
           #                                       36 == 2459 ~ "security_guards", 36 == 2460 ~ "shop_sale_assistants",
           #                                       36 == 2461 ~ "shopkeepers", 36 == 2462 ~ "market_salespersons",
           #                                       36 == 2463 ~ "street_service_workers", 36 %in% c(2464, 2465) ~ "street_food_salespersons",
           #                                       36 == 2467 ~ "subsistance_farmers", 36 == 2470 ~ "transport_labourers",
           #                                       36 == 2471 ~ "waiters", 36 == 2474 ~ "hairdressers", 36 == 2476 ~ "vocational_teachers",
           #                                       36 == 2482 ~ "other_language_teachers", 36 == 2486 ~ "teachers_unclassified",
           #                                                                                             TRUE ~ 36),           
           
           i.hh_member_occupation_pastyear_age_group = case_when(progres_age %in% c(1:4) ~ "age_1_4",
                                                                 progres_age %in% c(5:11) ~ "age_5_11",
                                                                 progres_age %in% c(12:17)~ "age_12_17",
                                                                 progres_age %in% c(18:25) ~ "age_18_25",
                                                                 progres_age %in% c(26:59) ~ "age_26_59",
                                                                 progres_age %in% c(60:100) ~ "age_greater_59")) %>% 
         
             mutate(int.age_dependant = ifelse(progres_age %in% c(0:14) | progres_age %in% c(65:100), 1, 0),
                    int.age_independent = ifelse(progres_age %in% c(15:64), 1, 0)) %>% 
             summarise(
               i.dependency_ratio = sum(age_dependant)/sum(age_independent)*100) %>% 
             
            # protection 
    mutate(i.hh_with_child_outside_of_home = ifelse(progres_relationshiptofpname %in% c("Focal Point") & 
                                                    child_currently_not_living_with_you == 1694, "yes", "no") 
                      )
  select(-c(starts_with("int.")))

}

      
      
      
      
      

