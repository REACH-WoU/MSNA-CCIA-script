crit_1_q2_level_3 <- c('lack_quiet_space','lack_equipment','lack_school_supplies','internet_bad',
                       'cannot_afford_the_direct_costs_of_education','inadequate_infrastructure_for_learning_in_safe_environment',
                       'lack_of_or_poor_quality_of_teachers','unable_enrol_in_school_lack_of_enrolment_space',
                       'unable_enrol_in_school_recent_displacemente_vacuation_return','the_childs_disability_or_other_health_issues',
                       'remote_edu_quality_bad','lack_of_interest','language','other','dont_know','prefer_not_to_answer')

crit_1_q2_level_4 <- c('protection_risks_while_at_or_traveling_to_the_school','marriage_engagement_andor_pregnancy',
                       'unable_enrol_in_school_lack_of_documentation','discrimination_or_stigmatization_of_the_child')



educ_data_prel <- data.list$hh_members  %>% 
  mutate(
    child = ifelse(between(as.numeric(B_5_hh_mem_age), 5,18),1,0),
    
    educ_crit_1_var_1_yes = ifelse(C_2_child_enrol %in% 'yes' , 1, 0),
    educ_crit_1_var_1_no = ifelse(C_2_child_enrol %in% 'no' , 1, 0),
    educ_crit_1_var_1_undef = ifelse(C_2_child_enrol %in% c('dont_know','prefer_not_to_answer') | is.na(C_2_child_enrol), 1,0),
    
    educ_crit_1_var_2_lvl_1 = ifelse(C_5_child_not_enrol_reason  %in% 'child_has_already_completed_compulsory_school_grades',1,0),
    educ_crit_1_var_2_lvl_3 = ifelse(grepl(paste0('(', paste0(crit_1_q2_level_3, collapse = ")|("),')'), C_5_child_not_enrol_reason),1,0),
    educ_crit_1_var_2_lvl_4 = ifelse(grepl(paste0('(', paste0(crit_1_q2_level_4, collapse = ")|("),')'), C_5_child_not_enrol_reason),1,0),
    
    # Combination of not attending formal education and having a level 3 | level 4 reason for it
    educ_crit_1_var_2_lvl_3_var_1_no = educ_crit_1_var_2_lvl_3*educ_crit_1_var_1_no,
    educ_crit_1_var_2_lvl_4_var_1_no = educ_crit_1_var_2_lvl_4*educ_crit_1_var_1_no,
    
    
    educ_crit_1_var_3_lvl_1= ifelse(C_3_learn_modality %in% c('in_person'),1,0),
    educ_crit_1_var_3_lvl_2 = ifelse(C_3_learn_modality %in% c('remote','blended'),1,0),
    educ_crit_1_var_3_undef = ifelse(C_3_learn_modality %in% c('dont_know','prefer_not_to_answer') | is.na(C_3_learn_modality),1,0)
    
    
  ) %>% 
  select(child,starts_with('educ_crit_1'),uuid) %>% 
  group_by(uuid) %>% 
  summarise(
    child = sum(child, na.rm = TRUE),
    across(starts_with('educ_crit_1'), sum)) %>% 
  ungroup()

names_to_drop <- setdiff(names(educ_data_prel),'uuid')



data.list$main <- data.list$main %>%
  left_join(educ_data_prel) %>% 
  mutate(
    educ_crit_1 = case_when(
      
      child == 0 ~ 1,
      
      (educ_crit_1_var_1_no >0 & educ_crit_1_var_2_lvl_4_var_1_no>0) ~ 4,
      
      (educ_crit_1_var_1_no >0 & educ_crit_1_var_2_lvl_3_var_1_no>0 & educ_crit_1_var_2_lvl_4 ==0) ~ 3,
      
      (educ_crit_1_var_1_yes == child & educ_crit_1_var_3_lvl_2 > 0)|
        (educ_crit_1_var_1_no == educ_crit_1_var_2_lvl_1 & educ_crit_1_var_3_lvl_2> 0)|
        (educ_crit_1_var_1_no == educ_crit_1_var_2_lvl_1 & educ_crit_1_var_1_yes == educ_crit_1_var_3_lvl_2) ~ 2,
      
      child == 0 |
        (educ_crit_1_var_1_yes == child & educ_crit_1_var_1_yes == educ_crit_1_var_3_lvl_1)|
        (educ_crit_1_var_1_no == educ_crit_1_var_2_lvl_1 & educ_crit_1_var_1_yes == educ_crit_1_var_3_lvl_1) ~ 1,
      
      educ_crit_1_var_3_undef > 0  | educ_crit_1_var_1_undef > 0 ~ NA_real_
    ),
    
    educ_crit_2 = case_when(
      child == 0 ~ 1,
      
      #(educ_crit_1_var_3_lvl_2 > 0 & G_17_internet_hours == 'never_0hrs') ~ 4,
      
      (educ_crit_1_var_3_lvl_2 > 0 & G_17_internet_hours %in% c('sometimes_1_11hrs','never_0hrs')) ~ 3,
      
      (child == educ_crit_1_var_3_lvl_1 & G_17_internet_hours %in% c('sometimes_1_11hrs','never_0hrs')) |
        (educ_crit_1_var_3_lvl_2 > 0 & G_17_internet_hours == 'often_12_23hrs') ~ 2,
      
      child == 0 | 
        (child == educ_crit_1_var_3_lvl_1 & G_17_internet_hours == 'often_12_23hrs') |
        G_17_internet_hours %in% 'always_24hrs' ~ 1,
      
      educ_crit_1_var_3_undef > 0  | 
        G_17_internet_hours %in% c('dont_know','prefer_not_to_answer') ~ NA_real_
    ),
    
    educ_crit_3 = case_when(
      child == 0 ~ 1,
      C_15_home_damage %in% 'yes' ~ 4,
      C_13_school_displ %in% 'yes' | C_14_school_damage %in% 'yes' ~ 3,
      
      C_13_school_displ %in% 'no' & C_14_school_damage %in% 'no' 
      & C_15_home_damage %in% 'no' & C_12_2_school_missile %in% 'yes' ~ 2,
      
      child == 0 |
        (C_13_school_displ %in% 'no' & C_14_school_damage %in% 'no' 
         & C_15_home_damage %in% 'no' & C_12_2_school_missile %in% 'no') |
        (child > 0 & educ_crit_1_var_3_lvl_2 ==0) |
        (child > 0 & educ_crit_1_var_3_lvl_2 >= 1 & is.na(C_14_school_damage))|
        (child > 0 & educ_crit_1_var_1_no > 0) ~ 1,
      
      C_13_school_displ %in% c('dont_know','prefer_not_to_answer')|
        C_14_school_damage %in% c('dont_know','prefer_not_to_answer') |
        C_15_home_damage %in% c('dont_know','prefer_not_to_answer')|
        C_12_2_school_missile %in% c('dont_know','prefer_not_to_answer')|
        is.na(C_13_school_displ)|is.na(C_14_school_damage)|
        is.na(C_15_home_damage)|is.na(C_12_2_school_missile) ~ NA_real_
    )) %>%
  select(-all_of(names_to_drop))
