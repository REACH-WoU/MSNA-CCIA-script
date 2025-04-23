shelter_type_g2_lvl_1 <- c(
  "individual_shelter",
  "individual_shelter_shared"
)

shelter_type_g2_lvl_2 <- c(
  "hosted_by_friends_relatives",
  "hosting_at_least_one_other_household_in_own_home"
)

shelter_type_g2_lvl_3 <- c(
  "collective_site"
)

shelter_type_g2_lvl_5 <- c(
  "no_shelter"
)

shelter_type_g2_lvl_undef <- c(
  "other",
  "dont_know",
  "prefer_not_to_answer"
)

#===

shelter_type_g3_lvl_1 <- c(
  "house",
  "finished_apartment"
)

shelter_type_g3_lvl_3 <- c(
  "unfinished"
)

shelter_type_g3_lvl_4 <- c(
  "tent",
  "makeshift"
)

shelter_type_g3_lvl_undef <- c(
  "other",
  "dont_know",
  "prefer_not_to_answer"
)

#=====

shelter_issues_g8 <- c(
  "G_8_shelter_issues/lack_of_privacy",
  "G_8_shelter_issues/lack_of_space_inside_dwelling_shelter",
  "G_8_shelter_issues/it_is_often_too_hot_or_too_cold_inside_dwelling_shelter",
  "G_8_shelter_issues/limited_ventilation_inside_dwelling_shelter",
  "G_8_shelter_issues/leaks_when_it_rains",
  "G_8_shelter_issues/unable_to_lock_the_dwelling_shelter",
  "G_8_shelter_issues/lack_of_lighting_inside_the_dwelling_shelter",
  "G_8_shelter_issues/lack_of_lighting_outside_the_dwelling_shelter",
  "G_8_shelter_issues/some_members_have_difficulties_moving_inside_or_outside_the_house",
  "G_8_shelter_issues/other"
)
shelter_issues_g8_undef <- c(
  "G_8_shelter_issues/dont_know",
  "G_8_shelter_issues/prefer_not_to_answer"
)

#===

shelter_issues_g9 <- c(
  "G_9_curr_shelter_damage/roof",
  "G_9_curr_shelter_damage/walls",
  "G_9_curr_shelter_damage/windows",
  "G_9_curr_shelter_damage/doors",
  "G_9_curr_shelter_damage/other"
)
shelter_issues_g9_undef <- c(
  "G_9_curr_shelter_damage/dont_know",
  "G_9_curr_shelter_damage/prefer_not_to_answer"
)

#===

shelter_issues_g10_lvl2 <- c(
  "dont_know",
  "prefer_not_to_answer",
  "insignificant",
  "minor"
)

shelter_issues_g10_lvl3 <- c(
  "moderate",
  "major"
)

shelter_issues_g10_lvl4 <- c(
  "catastrophic_complete"
)

shelter_issues_g10_undef <- c(
  "dont_know",
  "prefer_not_to_answer"
)

#=====

security_tenure_g6_low <- c(
  "owned_property",
  "rented"
)

security_tenure_g6_high <- c(
  "hosted_for_free",
  "no_occupancy_agreement_squatting"
)

security_tenure_g6_undef <- c(
  "dont_know",
  "prefer_not_to_answer"
)

#=====

leccy_g13_irr <- c(
  "G_13_heating_type/gas",
  "G_13_heating_type/electricity",
  "G_13_heating_type/distrcit_gas",
  "G_13_heating_type/distrcit_electricity",
  "G_13_heating_type/distrcit_not_sure_source",
  "G_13_heating_type/wood_or_coal",
  "G_13_heating_type/briquettes"
)

leccy_g13_undef <- c(
  "G_13_heating_type/other",
  "G_13_heating_type/dont_know",
  "G_13_heating_type/prefer_not_to_answer"
)

#===

leccy_g15_lvl2 <- c(
  "intermittent_electricity",
  "insufficient_electricity_in_terms_of_strength",
  "other"
)

leccy_g15_lvl4 <- c(
  "no_electricity"
)

leccy_g15_undef <- c(
  "dont_know",
  "prefer_not_to_answer"
)

#===

utility_g16_lvl3 <- c(
  # "G_16_utility_interrupt/gas",
  # "G_16_utility_interrupt/sewage",
  "G_16_utility_interrupt/cold_water_supply"
)

utility_g16_lvl2 <- c(
  "G_16_utility_interrupt/gas",
  "G_16_utility_interrupt/sewage",
  "G_16_utility_interrupt/hot_water_supply",
  "G_16_utility_interrupt/internet"
)

utility_g16_undef <- c(
  "G_16_utility_interrupt/other",
  "G_16_utility_interrupt/dont_know",
  "G_16_utility_interrupt/prefer_not_to_answer"
)

#===

nfis_j25_lvl3 <- c(
  "J_25_miss_nfi/winter_clothes_for_a_household_member",
  "J_25_miss_nfi/heating_appliances",
  "J_25_miss_nfi/fuel_for_heating"
)

nfis_j25_lvl2 <- c(
  "J_25_miss_nfi/bedding_and_towels",
  #"J_25_miss_nfi/bedding_and_towels",
  #"J_25_miss_nfi/winter_clothes_for_a_household_member",
  "J_25_miss_nfi/household_items",
  "J_25_miss_nfi/summer_clothes_for_a_household_member",
  "J_25_miss_nfi/kitchen_set",
  "J_25_miss_nfi/large_kitchen_app",
  "J_25_miss_nfi/other"
)

nfis_j25_irr <- c(
  "J_25_miss_nfi/feminine_hygiene_items",
  "J_25_miss_nfi/baby_diapers",
  "J_25_miss_nfi/soap",
  "J_25_miss_nfi/other_personal_hygiene_products",
  "J_25_miss_nfi/domestic_hygiene_products",
  "J_25_miss_nfi/water_treatment_product_and_materials"
)

nfis_j25_undef <- c(
  "J_25_miss_nfi/dont_know",
  "J_25_miss_nfi/prefer_not_to_answer"
)


snfi <- data.list$main %>%
  mutate(
    shelter_type = case_when(
      (
        G_2_shelter_situation %in% shelter_type_g2_lvl_5
      ) ~ 5,
      (
        G_3_shelter_type %in% shelter_type_g3_lvl_4
      ) ~ 4,
      (
        (G_2_shelter_situation %in% shelter_type_g2_lvl_3) |
          (G_3_shelter_type %in% shelter_type_g3_lvl_3)
      ) ~ 3,
      (
        (G_2_shelter_situation %in% shelter_type_g2_lvl_2) &
          (G_3_shelter_type %in% shelter_type_g3_lvl_1)
      ) ~ 2,
      (
        (G_2_shelter_situation %in% shelter_type_g2_lvl_1) &
          (G_3_shelter_type %in% shelter_type_g3_lvl_1)
      ) ~ 1,
      (
        (G_2_shelter_situation %in% shelter_type_g2_lvl_undef) |
          (G_3_shelter_type %in% shelter_type_g3_lvl_undef)|
          is.na(G_3_shelter_type)
      ) ~ NA
    ),
    issue_g8_cnt = rowSums(across(shelter_issues_g8, as.numeric),na.rm = TRUE),
    g8_undef_cnt = rowSums(across(shelter_issues_g8_undef, as.numeric),na.rm = TRUE),
    
    issue_g9_cnt = rowSums(across(shelter_issues_g9, as.numeric),na.rm = TRUE),
    g9_undef_cnt = rowSums(across(shelter_issues_g9_undef, as.numeric),na.rm = TRUE),
    

    shelter_issues = case_when(
      (G_10_shelter_assess %in% shelter_issues_g10_lvl4 & 
         between(issue_g9_cnt,3,5)) ~ 5,
      
      (G_10_shelter_assess %in% shelter_issues_g10_lvl3 & 
         between(issue_g9_cnt,3,5)) |
        (G_10_shelter_assess %in% shelter_issues_g10_lvl4 & 
           between(issue_g9_cnt,1,2)) ~ 4,
      
      ((issue_g8_cnt > 2) & (issue_g8_cnt < 11)) |
        (between(issue_g9_cnt,3,5) & G_10_shelter_assess %in% shelter_issues_g10_lvl2) |
        (between(issue_g9_cnt,1,2) & G_10_shelter_assess %in% shelter_issues_g10_lvl3) ~ 3,
      
      
      between(issue_g8_cnt,1,2) |
      ((issue_g9_cnt > 0 & issue_g9_cnt < 3) &
         (G_10_shelter_assess %in% shelter_issues_g10_lvl2)) ~ 2,
      
      as.numeric(`G_9_curr_shelter_damage/none`) %in% 1 & 
        as.numeric(`G_8_shelter_issues/none`) %in% 1  ~ 1,
      
      (as.numeric(`G_9_curr_shelter_damage/none`) == 1) ~ 1,
      (g9_undef_cnt > 0 | is.na(G_10_shelter_assess) | is.na(`G_9_curr_shelter_damage/none`)) ~ NA,
    ),

    
    security_tenure = case_when(
      (G_6_occupancy_arrang %in% security_tenure_g6_high) ~ 3,
      (
        (G_6_occupancy_arrang %in% security_tenure_g6_low) &
          (G_7_evict_risk == "yes")
      ) ~ 2,
      (
        (G_6_occupancy_arrang %in% security_tenure_g6_low) &
          (G_7_evict_risk == "no")
      ) ~ 1,
      (
        (G_6_occupancy_arrang %in% security_tenure_g6_undef) |
          (G_7_evict_risk %in% c("dont_know", "prefer_not_to_answer")) |
          is.na(G_6_occupancy_arrang) | is.na(G_7_evict_risk)
      ) ~ NA,
      TRUE ~ -1
    ),
    # functional domestic space
    g13_irr_cnt = rowSums(across(leccy_g13_irr, as.numeric),na.rm = TRUE),
    g13_undef_cnt = rowSums(across(leccy_g13_undef, as.numeric),na.rm = TRUE),
    leccy = case_when(
      (
        as.numeric(`G_13_heating_type/none`) == 1
      ) ~ 4,
      (
        G_15_electricity_issues %in% leccy_g15_lvl4 &
          g13_irr_cnt > 0
      ) ~ 3,
      (
        G_15_electricity_issues %in% leccy_g15_lvl2 &
          g13_irr_cnt > 0
      ) ~ 2,
      (
        G_15_electricity_issues == "none" &
          g13_irr_cnt > 0
      ) ~ 1,
      g13_undef_cnt > 0 | is.na(G_15_electricity_issues) | G_15_electricity_issues %in% leccy_g15_undef | uuid == "299771dc-a256-4c11-8867-bd028c8eb0f2" ~ NA,
      TRUE ~ -1
    ),
    g16_lvl3_cnt = rowSums(across(utility_g16_lvl3, as.numeric),na.rm = TRUE),
    g16_lvl2_cnt = rowSums(across(utility_g16_lvl2, as.numeric),na.rm = TRUE),
    g16_undef_cnt = rowSums(across(utility_g16_undef, as.numeric),na.rm = TRUE),
    
    utility = case_when(
      #(g16_lvl3_cnt > 0) ~ 3,
      (g16_lvl2_cnt > 0) ~ 2,
      (as.numeric(`G_16_utility_interrupt/none`) == 1) ~ 1,
      g16_undef_cnt > 0 | is.na(`G_16_utility_interrupt/none`) | g16_lvl3_cnt > 0 ~ NA ,
      TRUE ~ -1
    ),
    
    g11_cunt = ifelse(G_11_cook_issues == "no_cannot_do", 1, 0),
    g12_cunt = ifelse(G_12_sleep_issues == "no_cannot_do", 1, 0),
    g14_cunt = ifelse(G_14_food_store_issues == "no_cannot_do", 1, 0),
    g11_yi = ifelse(G_11_cook_issues == "yes_with_issues", 1, 0),
    g12_yi = ifelse(G_12_sleep_issues == "yes_with_issues", 1, 0),
    g14_yi = ifelse(G_14_food_store_issues == "yes_with_issues", 1, 0),
    
    g11_ni = ifelse(G_11_cook_issues %in% c("yes_without_any_issues",'no_do_not_need_to_cook_in_current_shelter'), 1, 0),
    g12_ni = ifelse(G_12_sleep_issues == "yes_without_any_issues", 1, 0),
    g14_ni = ifelse(G_14_food_store_issues == "yes_without_any_issues", 1, 0),
    
    
    domestic = case_when(
      
      ((g11_cunt + g12_cunt + g14_cunt) > 1) ~ 4,
      
      (((g11_yi + g12_yi + g14_yi) > 1 & (g11_ni + g12_ni + g14_ni)<=1 ) |
         ((g11_cunt + g12_cunt + g14_cunt) == 1)) ~ 3,
      
      ((g11_yi + g12_yi + g14_yi) == 1 & (g11_ni + g12_ni + g14_ni)==2) ~ 2,
      
      ((G_11_cook_issues %in% c("yes_without_any_issues", "no_do_not_need_to_cook_in_current_shelter")) &
          (G_12_sleep_issues %in% c("yes_without_any_issues")) &
          (G_14_food_store_issues %in% c("yes_without_any_issues"))) ~ 1,
      
      ((G_11_cook_issues %in% leccy_g15_undef | is.na(G_11_cook_issues))
        | (G_12_sleep_issues %in% leccy_g15_undef | is.na(G_12_sleep_issues))
        | (G_14_food_store_issues %in% leccy_g15_undef | is.na(G_14_food_store_issues))) ~ NA
      
      ),
    
    j25_lvl3_cnt = rowSums(across(nfis_j25_lvl3, as.numeric),na.rm = TRUE),
    j25_lvl2_cnt = rowSums(across(nfis_j25_lvl2, as.numeric),na.rm = TRUE),
    j25_irr_cnt = rowSums(across(nfis_j25_irr, as.numeric),na.rm = TRUE),
    j25_undef_cnt = rowSums(across(nfis_j25_undef, as.numeric),na.rm = TRUE),
    
    nfis = case_when(
      (j25_lvl3_cnt > 0) ~ 3,
      
      (j25_lvl2_cnt > 0)  ~ 2,
      
      ((as.numeric(`J_25_miss_nfi/none`) == 1) |
          is.na(`J_25_miss_nfi/none`) |
          (j25_irr_cnt > 0)) ~ 1,
      
      j25_undef_cnt > 0 ~ NA,
      TRUE ~ -1
    )
  ) %>%
  select(uuid, shelter_type, shelter_issues, security_tenure, leccy, utility, domestic, nfis)

data.list$main <- data.list$main %>%
  left_join(snfi, by = "uuid")
