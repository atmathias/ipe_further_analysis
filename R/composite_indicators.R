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
                   i.hh_with_child_outside_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1694) ~ "yes",
                                                               progres_relationshiptofpname %in% c("Focal Point") & child_currently_not_living_with_you %in% c(1695) ~ "no",
                                                               TRUE ~ NA_character_),
             
                   i.hh_with_child_outside_of_home = case_when(progres_relationshiptofpname %in% c("Focal Point") & 
                                                 where_children_are_living %in% c("Under care of another family in Uganda (foster family)",
                                              "Under care of another relative (kinship care arrangement) in Uganda)",
                                              "Under care of another family in his/her country of origin",
                                              "Living alone independently in another location",
                                              "Living in a third country (not Uganda nor country of origin)"), ~ "yes",
                                               progres_relationshiptofpname %in% c("Focal Point") & where_children_are_living %in% c("Don't know") ~ "dk",
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
                i.hh_children_dangerous_work_conditions = case_when(progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1762 : 1770) ~ c(1762 : 1770),
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1763) ~ "1763",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1764) ~ "1764",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1765) ~ "1765",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1766) ~ "1766",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1767) ~ "1767",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1768) ~ "1768",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1769) ~ "1769",
                                                        # progres_relationshiptofpname %in% c("Focal Point") & child_work_involve %in% c(1770) ~ "1770",
                                                        TRUE ~ NA_character_)) %>%
  
             # other analysis
             # mutate(int.age_dependant = ifelse(progres_age %in% c(0:14) | progres_age %in% c(65:100), 1, 0),
             #        int.age_independent = ifelse(progres_age %in% c(15:64), 1, 0)) %>%
             # summarise(
             #   i.dependency_ratio = sum(int.age_dependant)/sum(int.age_independent)*100) %>% 

      select(-c(starts_with("int.")))
}
      
      
      
      
      

