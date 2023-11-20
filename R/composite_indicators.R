# creating composite indicators -------------------------------------------

create_composites_verification <- function(input_df) {
  input_df %>% 
    # demographic indicators
    mutate(location_region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                                       settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                       settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", 
                                                         "Rhino") ~ "West Nile"),
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
                  i.hoh_disability = ifelse(progres_relationshiptofpname %in% c("Focal Point") & i.hh_with_disabled_member %in% c("yes_disability"),
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
                                                                        progres_age %in% c(60:100) ~ "age_greater_59"),
                   # protection
                  i.hh_with_child_outside_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & 2 %in% c(1694) ~ "yes",
                                                              progres_relationshiptofpname %in% c("Focal Point") & 2 %in% c(1695) ~ "no",
                                                              TRUE ~ NA_character_),
                  # i.hh_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & 123 %in% c(1694) ~ "yes",
                  #                                             progres_relationshiptofpname %in% c("Focal Point") & 123 %in% c(1695) ~ "no",
                  #                                             TRUE ~ NA_character_),
                  i.avg_time_children_worked_forpayment = case_when(progres_relationshiptofpname %in% c("Focal Point") & 53 %in% c(1759) ~ "Between 1 and 13 hours",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 53 %in% c(1760) ~ "Between 14 and 42 hours",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 53 %in% c(1761) ~ "more than 42",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 53 %in% c(2473) ~ "No kid working",
                                                                    TRUE ~ NA_character_),
                  i.hh_children_worked_Hhchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & 124 %in% c(1694) ~ "yes",
                                                           progres_relationshiptofpname %in% c("Focal Point") & 124 %in% c(1695) ~ "no",
                                                                    TRUE ~ NA_character_),
                  i.avg_time_children_worked_HHchores = case_when(progres_relationshiptofpname %in% c("Focal Point") & 54 %in% c(1759, 2488) ~ "2488",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 54 %in% c(1760, 2489) ~ "2489",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 54 %in% c(2490) ~ "2490",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 54 %in% c(1761, 2491) ~ "2491",
                                                                    progres_relationshiptofpname %in% c("Focal Point") & 54 %in% c(2473, 2492) ~ "2492",
                                                                    TRUE ~ NA_character_),
                  i.hh_children_dangerous_work_conditions = case_when(progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1762) ~ "1762",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1763) ~ "1763",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1764) ~ "1764",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1765) ~ "1765",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1766) ~ "1766",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1767) ~ "1767",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1768) ~ "1768",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1769) ~ "1769",
                                                                      progres_relationshiptofpname %in% c("Focal Point") & 55 %in% c(1770) ~ "1770",
                                                                      TRUE ~ NA_character_)) %>% 
           
             # other analysis
             # mutate(int.age_dependant = ifelse(progres_age %in% c(0:14) | progres_age %in% c(65:100), 1, 0),
             #        int.age_independent = ifelse(progres_age %in% c(15:64), 1, 0)) %>%
             # summarise(
             #   i.dependency_ratio = sum(int.age_dependant)/sum(int.age_independent)*100) %>% 

      select(-c(starts_with("int.")))
}
      
      
      
      
      

