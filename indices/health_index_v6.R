
health_crit_1_var_3_lvl_1_ls <- c('preventative_consultation_check_up')
health_crit_1_var_3_lvl_2_ls <- c("consultation_or_drugs_for_chronic_illness",
                                  "consultation_or_drugs_for_other_illnesses",'elective_non_life_saving_surgery',
                                  "antenatal_or_post_natal_services", 
                                  "other")

health_crit_1_var_3_lvl_2_delivery <- c("safe_delivery_services",'consultation_or_drugs_for_acute_illness')


health_crit_1_var_3_lvl_3_ls <- c("trauma_care","emergency_life_saving_surgery")

health_crit_1_var_3_undef_ls <- c("dont_know", "prefer_not_to_answer")


# crit 3

health_crit_var_3_lvl_1 <- c("D_9_medicines_type/medicines_for_allergies", "D_9_medicines_type/medicines_for_birth_control")

health_crit_var_3_lvl_2 <- c("D_9_medicines_type/medicines_to_regulate_blood_sugar", "D_9_medicines_type/medicines_for_high_blood_pressure",
                             'D_9_medicines_type/medicines_for_thyroid_problems','D_9_medicines_type/other')

health_crit_var_3_lvl_3 <- c("D_9_medicines_type/medicines_for_mental_health_conditions", "D_9_medicines_type/medicines_for_infections",
                             'D_9_medicines_type/medicines_for_pain')

health_crit_var_3_lvl_4 <- c("D_9_medicines_type/medicines_to_prevent_or_treat_cancer", "D_9_medicines_type/medicines_to_prevent_or_treat_heart_problems",
                             'D_9_medicines_type/medicines_for_longterm_lung_problems','D_9_medicines_type/medicines_for_the_neurologicnervous_system_diseases')

health_crit_var_3_undef <- c("D_9_medicines_type/dont_know", "D_9_medicines_type/prefer_not_to_answer")


# crit 5

health_crit_5_var_1_lvl_1 <- c('D_12_medicines_barriers/none','D_12_medicines_barriers/time_necessary_to_access_the_services__access_medicine')

health_crit_5_var_1_lvl_2 <- c('D_12_medicines_barriers/transport_necessary_get_to_services_access_medicine',
                               'D_12_medicines_b arriers/other',
                               'D_12_medicines_barriers/dont_know',
                               'D_12_medicines_barriers/prefer_not_to_answer')

health_crit_5_var_1_lvl_3 <- c('D_12_medicines_barriers/cost_of_medicine',
                               'D_12_medicines_barriers/cost_of_treatment',
                               'D_12_medicines_barriers/need_for_unofficial_payments',
                               'D_12_medicines_barriers/needed_services_medicine_were_not_available')

health_crit_5_var_1_lvl_4 <- c('D_12_medicines_barriers/security_concerns',
                               'D_12_medicines_barriers/not_have_needed_documents',
                               'D_12_medicines_barriers/lack_of_medical_facilities_facilities_difficult_to_access',
                               'D_12_medicines_barriers/lack_of_pharmacies_pharmacies_difficult_to_access',
                               'D_12_medicines_barriers/refusal_to_provide_service_medicine')

######

health_crit_5_var_met_lvl_1 <- c('D_12_medicines_barriers/none',
                                 'D_12_medicines_barriers/time_necessary_to_access_the_services__access_medicine',
                                 'D_12_medicines_barriers/transport_necessary_get_to_services_access_medicine')

health_crit_5_var_unmet_lvl_1 <- c('D_12_medicines_barriers/none',
                                 'D_12_medicines_barriers/time_necessary_to_access_the_services__access_medicine')

health_crit_5_var_unmet_lvl_2 <- c('D_12_medicines_barriers/other',
                                   'D_12_medicines_barriers/dont_know',
                                   'D_12_medicines_barriers/transport_necessary_get_to_services_access_medicine')


health_crit_5_var_met_lvl_2 <- c('D_12_medicines_barriers/other',
                                 'D_12_medicines_barriers/dont_know',
                                 'D_12_medicines_barriers/needed_services_medicine_were_not_available',
                                 'D_12_medicines_barriers/need_for_unofficial_payments',
                                 'D_12_medicines_barriers/cost_of_treatment',
                                 'D_12_medicines_barriers/cost_of_medicine')

health_crit_5_var_unmet_lvl_2 <- c('D_12_medicines_barriers/other',
                                   'D_12_medicines_barriers/dont_know',
                                   'D_12_medicines_barriers/transport_necessary_get_to_services_access_medicine')

health_crit_5_var_met_lvl_3 <- c('D_12_medicines_barriers/not_have_needed_documents',
                                 'D_12_medicines_barriers/lack_of_medical_facilities_facilities_difficult_to_access',
                                 'D_12_medicines_barriers/lack_of_pharmacies_pharmacies_difficult_to_access',
                                 'D_12_medicines_barriers/refusal_to_provide_service_medicine')

health_crit_5_var_unmet_lvl_3 <- c('D_12_medicines_barriers/cost_of_medicine',
                                   'D_12_medicines_barriers/cost_of_treatment',
                                   'D_12_medicines_barriers/need_for_unofficial_payments',
                                   'D_12_medicines_barriers/needed_services_medicine_were_not_available')

health_crit_5_var_met_lvl_4 <- c('D_12_medicines_barriers/security_concerns')

health_crit_5_var_unmet_lvl_4 <- c('D_12_medicines_barriers/security_concerns',
                                   'D_12_medicines_barriers/not_have_needed_documents',
                                   'D_12_medicines_barriers/lack_of_medical_facilities_facilities_difficult_to_access',
                                   'D_12_medicines_barriers/lack_of_pharmacies_pharmacies_difficult_to_access',
                                   'D_12_medicines_barriers/refusal_to_provide_service_medicine')




health_data_prel <- data.list$hh_members %>% 
  mutate(
    member = 1,
    health_crit_1_var_1_need = ifelse(D_3_health_need  %in% 'yes',1,0),
    health_crit_1_var_1_noneed = ifelse(D_3_health_need %in% 'no',1,0),
    health_crit_1_var_1_undef = ifelse(D_3_health_need %in% c('dont_know','prefer_not_to_answer')|
                                         is.na(D_3_health_need), 1,0),
    
    health_crit_1_var_2_obtained = ifelse(D_5_obtained_healthcare %in% 'yes',1,0),
    health_crit_1_var_2_notobtained = ifelse(D_5_obtained_healthcare %in% 'no',1,0),
    health_crit_1_var_2_undef = ifelse(D_5_obtained_healthcare %in% c('dont_know','prefer_not_to_answer')|
                                         is.na(D_3_health_need), 1,0),
    
    health_crit_1_var_3_lvl_1 = ifelse(
      grepl(paste0('(',paste0(health_crit_1_var_3_lvl_1_ls, collapse = ')|('),')'), D_4_health_service_type),1,0),
    health_crit_1_var_3_lvl_2 = ifelse(
      grepl(paste0('(',paste0(health_crit_1_var_3_lvl_2_ls, collapse = ')|('),')'), D_4_health_service_type),1,0),
    health_crit_1_var_3_lvl_2_delv = ifelse(
      grepl(paste0('(',paste0(health_crit_1_var_3_lvl_2_delivery, collapse = ')|('),')'), D_4_health_service_type),1,0),
    health_crit_1_var_3_lvl_3 = ifelse(
      grepl(paste0('(',paste0(health_crit_1_var_3_lvl_3_ls, collapse = ')|('),')'), D_4_health_service_type),1,0),
    health_crit_1_var_3_undef = ifelse(
      grepl(paste0('(',paste0(health_crit_1_var_3_undef_ls, collapse = ')|('),')'), D_4_health_service_type) |
        is.na(D_4_health_service_type),1,0),
    
    # we need a combo of having a level of a need and obtaining it 
    health_crit_1_var_3_lvl_1_obtained = health_crit_1_var_3_lvl_1*health_crit_1_var_2_obtained,
    health_crit_1_var_3_lvl_2_obtained = health_crit_1_var_3_lvl_2*health_crit_1_var_2_obtained,
    health_crit_1_var_3_lvl_2_delv_obtained = health_crit_1_var_3_lvl_2_delv*health_crit_1_var_2_obtained,
    health_crit_1_var_3_lvl_3_obtained = health_crit_1_var_3_lvl_3*health_crit_1_var_2_obtained,
    
    # same with non obtaining
    health_crit_1_var_3_lvl_1_nonobtained = health_crit_1_var_3_lvl_1*health_crit_1_var_2_notobtained,
    health_crit_1_var_3_lvl_2_nonobtained = health_crit_1_var_3_lvl_2*health_crit_1_var_2_notobtained,
    health_crit_1_var_3_lvl_2_delv_nonobtained = health_crit_1_var_3_lvl_2_delv*health_crit_1_var_2_notobtained,
    health_crit_1_var_3_lvl_3_nonobtained = health_crit_1_var_3_lvl_3*health_crit_1_var_2_notobtained,
    
    
  ) %>% 
  select(uuid, member,starts_with('health_crit')) %>% 
  group_by(uuid) %>% 
  summarise(
    member = sum(member, na.rm = TRUE),
    across(starts_with('health_crit_1'), sum)) %>% 
  ungroup()


names_to_drop <- setdiff(names(health_data_prel),'uuid')

data.list$main <- 
  data.list$main %>% 
  left_join(health_data_prel) %>% 
  mutate(
    health_crit_1 = case_when(
      health_crit_1_var_3_lvl_3_nonobtained>0 ~ 5,
      
      health_crit_1_var_3_lvl_2_delv_nonobtained>0 ~ 4,
      
      
      (health_crit_1_var_3_lvl_2_nonobtained >0) |
        (health_crit_1_var_3_lvl_3_obtained>0) ~ 3,
      
      (health_crit_1_var_3_lvl_1 > 0) |
        (health_crit_1_var_3_lvl_2_obtained > 0 |
           health_crit_1_var_3_lvl_2_delv_obtained > 0) ~ 2,
      
      health_crit_1_var_1_noneed == member | (
        health_crit_1_var_1_need > 0 &
          health_crit_1_var_3_lvl_1_obtained == member) ~1,
      
      health_crit_1_var_1_undef > 0 | health_crit_1_var_2_undef > 0 | health_crit_1_var_3_undef > 0 ~ NA_real_,
    ),
    
    
    health_crit_2 = case_when(
      
      B_15_wgss_seeing %in% c('a_lot_of_difficulty','cannot_do_at_all')| 
        B_16_wgss_hearing %in% c('a_lot_of_difficulty','cannot_do_at_all') |
        B_17_wgss_walking %in% c('a_lot_of_difficulty','cannot_do_at_all') |
        B_18_wgss_cognit %in% c('a_lot_of_difficulty','cannot_do_at_all') |
        B_19_wgss_selfcare %in% c('a_lot_of_difficulty','cannot_do_at_all')|
        B_20_wgss_comm %in% c('a_lot_of_difficulty','cannot_do_at_all') ~ 3,
      
      B_15_wgss_seeing %in% 'some_difficulty'| B_16_wgss_hearing %in% 'some_difficulty' |
        B_17_wgss_walking %in% 'some_difficulty' | B_18_wgss_cognit %in% 'some_difficulty' |
        B_19_wgss_selfcare %in% 'some_difficulty'| B_20_wgss_comm %in% 'some_difficulty' ~ 2,
      
      B_15_wgss_seeing %in% 'none'& B_16_wgss_hearing %in% 'none' &
        B_17_wgss_walking %in% 'none' & B_18_wgss_cognit %in% 'none' &
        B_19_wgss_selfcare %in% 'none'& B_20_wgss_comm %in% 'none' ~ 1,
      
      B_15_wgss_seeing %in% c('dont_know','prefer_not_to_answer')| 
        B_16_wgss_hearing %in% c('dont_know','prefer_not_to_answer') |
        B_17_wgss_walking %in% c('dont_know','prefer_not_to_answer') |
        B_18_wgss_cognit %in% c('dont_know','prefer_not_to_answer') |
        B_19_wgss_selfcare %in% c('dont_know','prefer_not_to_answer')|
        B_20_wgss_comm %in% c('dont_know','prefer_not_to_answer') ~ NA_real_
    ),
    
    health_crit_3 = case_when(
      rowSums(across(all_of(health_crit_var_3_lvl_4) , .fns = as.numeric), na.rm = T) > 0 ~ 4,
      
      rowSums(across(all_of(health_crit_var_3_lvl_3) , .fns = as.numeric), na.rm = T) > 0 &
        rowSums(across(all_of(health_crit_var_3_lvl_4) , .fns = as.numeric), na.rm = T) == 0 ~ 3,
      
      rowSums(across(all_of(health_crit_var_3_lvl_2) , .fns = as.numeric), na.rm = T) > 0 &
        rowSums(across(all_of(health_crit_var_3_lvl_3) , .fns = as.numeric), na.rm = T) == 0 &
        rowSums(across(all_of(health_crit_var_3_lvl_4) , .fns = as.numeric), na.rm = T) == 0 ~ 2,
      
      D_7_medicines_sought %in% 'no' |
        (D_7_medicines_sought %in% 'yes' & D_8_medicinces_obtained  %in% 'yes')|
        rowSums(across(all_of(health_crit_var_3_lvl_1) , .fns = as.numeric), na.rm = T) > 0 ~ 1, # is we need it here?
      
      is.na(D_7_medicines_sought) | is.na(D_9_medicines_type) |
        rowSums(across(all_of(health_crit_var_3_undef) , .fns = as.numeric), na.rm = T) > 0 ~ NA_real_
    ),
    D_2_health_trasport_length = as.numeric(D_2_health_trasport_length),
    
    health_crit_4 = case_when(
      D_2_health_trasport_length > 45 ~ 3,
      between(D_2_health_trasport_length, 31, 45) ~ 2,
      between(D_2_health_trasport_length, 0, 30) ~ 1,
      is.na(D_2_health_trasport_length) ~ NA_real_
    ),
    
    needs_met_unmet = case_when(
      D_8_medicinces_obtained %in% 'no' | health_crit_1_var_2_notobtained > 0 ~ 'unmet',
      
      (health_crit_1_var_2_obtained == health_crit_1_var_1_need & D_8_medicinces_obtained %in% 'yes') |
        (health_crit_1_var_1_need == 0) |
        (health_crit_1_var_2_obtained == health_crit_1_var_1_need &
           (D_8_medicinces_obtained %in% c('prefer_not_to_answer','dont_know') | is.na(D_8_medicinces_obtained))) |
        (health_crit_1_var_2_undef == health_crit_1_var_1_need &
           D_8_medicinces_obtained %in% c('yes')) ~ 'met',
      
      (D_8_medicinces_obtained %in% c('prefer_not_to_answer','dont_know') | is.na(D_8_medicinces_obtained))  & 
        health_crit_1_var_2_undef == health_crit_1_var_1_need ~ NA_character_
      # ,
      # 
      # 
      # TRUE ~ '-1'
    ),
    
    
    health_crit_5 = case_when(
      ((rowSums(across(all_of(health_crit_5_var_met_lvl_4) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("met")) |
         (rowSums(across(all_of(health_crit_5_var_unmet_lvl_4) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("unmet"))) ~ 4,
      
      ((rowSums(across(all_of(health_crit_5_var_met_lvl_3) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("met")) |
         (rowSums(across(all_of(health_crit_5_var_unmet_lvl_3) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("unmet"))) ~ 3,
      
      ((rowSums(across(all_of(health_crit_5_var_met_lvl_2) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("met")) |
      (rowSums(across(all_of(health_crit_5_var_unmet_lvl_2) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("unmet"))) ~ 2,
      
      (rowSums(across(all_of(health_crit_5_var_met_lvl_1) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("met")) |
      (rowSums(across(all_of(health_crit_5_var_unmet_lvl_1) , .fns = as.numeric), na.rm = T) > 0 & needs_met_unmet %in% c("unmet")) |
      (member - health_crit_1_var_1_noneed == 0 & `D_7_medicines_sought` == 'no') |
      (health_crit_1_var_1_need + health_crit_1_var_1_undef == 0 & (D_7_medicines_sought == 'dont_know' | D_7_medicines_sought =='prefer_not_to_answer')) |
      (health_crit_1_var_1_need + health_crit_1_var_1_noneed == 0 | D_7_medicines_sought == "no") |
      is.na(D_12_medicines_barriers) ~ 1,
      
      needs_met_unmet %in% NA | D_12_medicines_barriers == "prefer_not_to_answer" ~ NA_real_,
      
      TRUE ~ -1
        
    )
    
    # health_crit_6 = case_when(
    #   (rowSums(across(all_of(c('D_15_felt_stressed','D_16_depressed_mood','D_17_anxious')), ~ .x %in% 'yes'))== 3 &
    #      (K_2_first %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' |
    #         K_2_second %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' |
    #         K_2_third %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry')
    #   ) ~ 3,
    #   
    #   
    #   rowSums(across(all_of(c('D_15_felt_stressed','D_16_depressed_mood','D_17_anxious')), ~ .x %in% 'yes')) <=2  &
    #     rowSums(across(all_of(c('D_15_felt_stressed','D_16_depressed_mood','D_17_anxious')), ~ .x %in% 'yes')) >0  &
    #     (K_2_first %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' |
    #        K_2_second %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' |
    #        K_2_third %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry')~ 2,
    #   
    #   (D_15_felt_stressed  %in% 'no' & D_16_depressed_mood %in% 'no' &  D_17_anxious  %in% 'no') |
    #     (rowSums(across(all_of(c('D_15_felt_stressed','D_16_depressed_mood','D_17_anxious')), ~ .x %in% 'yes')) >0 &
    #        ! K_2_first %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' &
    #        ! K_2_secnond %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry' &
    #        ! K_2_third %in% 'household_members_feeling_very_distressed_upset_sad_worried_scared_or_angry'
    #     ) ~ 1,
    #   
    #   D_15_felt_stressed  %in% c('prefer_not_to_answer','dont_know') |
    #     D_16_depressed_mood %in% c('prefer_not_to_answer','dont_know') | 
    #     D_17_anxious  %in% c('prefer_not_to_answer','dont_know') |
    #     is.na(D_15_felt_stressed) | is.na(D_16_depressed_mood) | is.na(D_17_anxious) ~ NA_real_)
  ) %>% 
  select(-all_of(names_to_drop)) 




