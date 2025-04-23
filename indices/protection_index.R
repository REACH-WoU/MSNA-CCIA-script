safety_f2_lvl1 <- c(
  "F_2_gen_safety_concerns/none",
  "F_2_gen_safety_concerns/stray_animals"
)

safety_f2_conflict <- c(
  "F_2_gen_safety_concerns/violence_related_to_the_conflict_impacting_public_infrastructure",
  "F_2_gen_safety_concerns/violence_related_to_the_conflict_impacting_private_infrastructure",
  "F_2_gen_safety_concerns/violence_related_to_the_conflict_impacting_civilians",
  "F_2_gen_safety_concerns/presence_of_landmines_uxos"
)

safety_f2_lvl2 <- c(
  "F_2_gen_safety_concerns/conscription",
  "F_2_gen_safety_concerns/being_robbed",
  "F_2_gen_safety_concerns/presence_of_military_actors",
  "F_2_gen_safety_concerns/other"
)

safety_f2_lvl3 <- c(
  "F_2_gen_safety_concerns/violence_and_harassment_not_related_conflict_in_public_spaces",
  "F_2_gen_safety_concerns/violence_and_harassment_not_related_conflict_in_private_spaces",
  "F_2_gen_safety_concerns/discrimination",
  "F_2_gen_safety_concerns/exploitation",
  "F_2_gen_safety_concerns/social_tension_in_the_community"
)

safety_f2_lvl4 <- c(
  "F_2_gen_safety_concerns/arbitrary_arrest_detention",
  "F_2_gen_safety_concerns/kidnapping_forced_disappearance"
)

safety_f2_undef <- c(
  "F_2_gen_safety_concerns/dont_know",
  "F_2_gen_safety_concerns/prefer_not_to_answer"
)

#=====

barriers_k202_lvl2 <- c(
  "K_20_2_govern_barriers/yes_social_workers_from_state_institutions_not_visit_location_often",
  "K_20_2_govern_barriers/yes_lack_information_about_available_services",
  "K_20_2_govern_barriers/yes_quality_of_services_not_adequate",
  "K_20_2_govern_barriers/yes_services_not_always_functional",
  "K_20_2_govern_barriers/yes_fear_of_conscription",
  "K_20_2_govern_barriers/other"
)

barriers_k202_lvl3 <- c(
  "K_20_2_govern_barriers/yes_lack_of_available_services",
  "K_20_2_govern_barriers/yes_insufficient_number_social_workers_provide_services",
  "K_20_2_govern_barriers/yes_limited_services_for_people_with_disability",
  "K_20_2_govern_barriers/yes_distance_lack_or_cost_for_transportation_relevant_facilities",
  "K_20_2_govern_barriers/yes_discrimination_against_specific_social_groups"
)

#====

needs_level_2 <- c(
  "F_8_concerns_hlp/rental_disputes",
  "F_8_concerns_hlp/no_social_affordable_housing_available_in_the_area",
  "F_8_concerns_hlp/other"
)

needs_level_3 <- c(
  "F_8_concerns_hlp/looting_of_private_property",
  "F_8_concerns_hlp/lack_of_access_to_eligibility_for_compensation_mechanisms",
  "F_8_concerns_hlp/lack_of_information_on_compensation_mechanisms",
  "F_8_concerns_hlp/lack_of_compensation_through_compensation_mechanisms",
  "F_8_concerns_hlp/lack_of_documents_access_to_documents_proving_ownership_of_housing",
  "F_8_concerns_hlp/eviction_from_rented_housing"
)

needs_level_4 <- c(
  "F_8_concerns_hlp/damaged_or_destroyed_housing_in_area_occupied_by_the_russian_federation",
  "F_8_concerns_hlp/damaged_or_destroyed_housing_in_area_not_occupied_by_russian_federation",
  "F_8_concerns_hlp/damage_or_destroyed_property_other_than_housing",
  "F_8_concerns_hlp/land_contaminated_with_eos",
  "F_8_concerns_hlp/property_is_occupied_by_others",
  "F_8_concerns_hlp/housing_and_or_land_is_used_for_military_purposes",
  "F_8_concerns_hlp/housing_and_or_land_is_not_accessible_due_to_military_restrictions_active_hostilities"
)

#====

legal_assist_level_2 <- c(
  "K_20_legal_assist/yes_to_obtain_civil_documents",
  "K_20_legal_assist/yes_to_resolve_issues_labour_law",
  "K_20_legal_assist/other"
)

legal_assist_level_3 <- c(
  "K_20_legal_assist/yes_to_obtain_property_documentation",
  "K_20_legal_assist/yes_access_pensions_idps",
  "K_20_legal_assist/yes_to_apply_for_idp_allowance",
  "K_20_legal_assist/yes_to_access_social_benefits",
  "K_20_legal_assist/yes_to_apply_for_utility_subsidies",
  "K_20_legal_assist/yes_to_obtain_identity_documents",
  "K_20_legal_assist/yes_to_apply_for_compensation_for_damaged_or_destroyed_property"
)

#===

child_sep_very_severy <- c(
  # "B_39_child_outside_reason/Engagement with armed groups/forces",
  "B_39_child_outside_reason/kidnapped_abducted",
  "B_39_child_outside_reason/stayed_behind_at_the_area_of_origin",
  "B_39_child_outside_reason/missing",
  "B_39_child_outside_reason/arbitrarily_detained"
)

child_sep_severy <- c(
  "B_39_child_outside_reason/got_separated_during_displacement",
  "B_39_child_outside_reason/left_the_house_to_seek_employment",
  "B_39_child_outside_reason/married_and_left_the_house",
  "B_39_child_outside_reason/left_the_house_to_reside_with_nonfamily_member",
  "B_39_child_outside_reason/living_in_a_state_institution",
  "B_39_child_outside_reason/other"
)

child_sep_non_severy <- c(
  "B_39_child_outside_reason/left_the_house_to_study",
  "B_39_child_outside_reason/living_with_foster_family",
  "B_39_child_outside_reason/living_abroad"
)


protection <- data.list$main %>%
  mutate(
    f2_lvl1_cnt = rowSums(across(safety_f2_lvl1, as.numeric),na.rm = TRUE),
    f2_conflict_cnt = rowSums(across(safety_f2_conflict, as.numeric),na.rm = TRUE),
    f2_lvl2_cnt = rowSums(across(safety_f2_lvl2, as.numeric),na.rm = TRUE),
    f2_lvl3_cnt = rowSums(across(safety_f2_lvl3, as.numeric),na.rm = TRUE),
    f2_lvl4_cnt = rowSums(across(safety_f2_lvl4, as.numeric),na.rm = TRUE),
    f2_undef_cnt = rowSums(across(safety_f2_undef, as.numeric),na.rm = TRUE),
    # `30km_fl_rb` = ifelse(grepl("away", `hh_30km_fl`), "0", "1"),
    safety = case_when(
      (
        as.numeric(`30km_fl_rb`) == 1 &
        f2_conflict_cnt > 0
      ) ~ 5,
      (
        (
          f2_lvl4_cnt > 0
        )
          |
        (
          as.numeric(`30km_fl_rb`) == 1
        )
      ) ~ 4,
      (
        (
          f2_lvl3_cnt > 0
        )
          |
        (
          f2_conflict_cnt > 0 &
          as.numeric(`prox_30_100_fl`) == 1
        )
      ) ~ 3,
      (
        (
          f2_lvl2_cnt > 0
        )
          | 
        (
          f2_conflict_cnt > 0 &
            as.numeric(`prox_30_100_fl`) == 0 &
            as.numeric(`30km_fl_rb`) == 0 
        ) |
        (
          as.numeric(`prox_30_100_fl`) == 1
        )
      ) ~ 2,
      
      (
        rowSums(across(safety_f2_lvl1, as.numeric),na.rm = TRUE) > 0
      ) ~ 1,
      
      f2_undef_cnt > 0 | is.na(`F_2_gen_safety_concerns/none`) | is.na(`30km_fl_rb`) ~ NA,
      
      TRUE ~ -1,
    ),
    safety_corrected = case_when(
      (
       # as.numeric(`30km_fl_rb`) == 1 &
          f2_conflict_cnt > 0
      ) ~ 5,
      (
        (
          f2_lvl4_cnt > 0
        )
       # |
       #   (
       #     as.numeric(`30km_fl_rb`) == 1
       #   )
      ) ~ 4,
      (
        (
          f2_lvl3_cnt > 0
        )
        #|
        #  (
        #    f2_conflict_cnt > 0 
        #    &  as.numeric(`prox_30_100_fl`) == 1
        #  )
      ) ~ 3,
      (
        (
          f2_lvl2_cnt > 0
        )
       # | 
       #   (
       #     f2_conflict_cnt > 0 &
       #       as.numeric(`prox_30_100_fl`) == 0 &
       #       as.numeric(`30km_fl_rb`) == 0 
       #   ) |
       #   (
       #     as.numeric(`prox_30_100_fl`) == 1
       #   )
      ) ~ 2,
      
      (
        rowSums(across(safety_f2_lvl1, as.numeric),na.rm = TRUE) > 0
      ) ~ 1,
      
      f2_undef_cnt > 0 | is.na(`F_2_gen_safety_concerns/none`)  ~ NA,
      
      TRUE ~ -1,
    ),
    k202_lvl2_cnt = rowSums(across(barriers_k202_lvl2, as.numeric),na.rm = TRUE),
    k202_lvl3_cnt = rowSums(across(barriers_k202_lvl3, as.numeric),na.rm = TRUE),
    barriers = case_when(
      (
        K_20_1_govern_services == "yes" &
          k202_lvl3_cnt > 0
      ) ~ 3,
      (
        K_20_1_govern_services == "yes" &
          k202_lvl2_cnt > 0
      ) ~ 2,
      (
        K_20_1_govern_services == "no" |
          (K_20_1_govern_services == "yes" & as.numeric(`K_20_2_govern_barriers/none`) == 1)
      ) ~ 1,
      (
        as.numeric(`K_20_2_govern_barriers/prefer_not_to_answer`) == 1 |
          K_20_1_govern_services %in% c("dont_know", "prefer_not_to_answer") |
          is.na(K_20_1_govern_services) |
          (K_20_1_govern_services == "yes" & k202_lvl2_cnt+k202_lvl3_cnt == 0)
      ) ~ NA,
      TRUE ~ -1,
    ),
    N_level_2_cnt = rowSums(across(needs_level_2, as.numeric), na.rm = TRUE),
    N_level_3_cnt = rowSums(across(needs_level_3, as.numeric), na.rm = TRUE),
    N_level_4_cnt = rowSums(across(needs_level_4, as.numeric), na.rm = TRUE),
    needs = case_when(
      (
        N_level_4_cnt > 0
      ) ~ 4,
      (
        N_level_3_cnt > 0 &
          N_level_4_cnt == 0
      ) ~ 3,
      (
        N_level_2_cnt > 0 &
          N_level_3_cnt == 0 &
          N_level_4_cnt == 0
      ) ~ 2,
      (
        as.numeric(`F_8_concerns_hlp/none`) == 1
      ) ~ 1,
      (
        as.numeric(`F_8_concerns_hlp/dont_know` == 1) |
          as.numeric(`F_8_concerns_hlp/prefer_not_to_answer` == 1) |
          is.na(`F_8_concerns_hlp`)
      ) ~ NA,
      TRUE ~ -1,
    ),
    legal_assist_level_2_cnt = rowSums(across(legal_assist_level_2, as.numeric), na.rm = TRUE),
    legal_assist_level_3_cnt = rowSums(across(legal_assist_level_3, as.numeric), na.rm = TRUE),
    # legal_assist_level_4_cnt = rowSums(across(needs_level_4, as.numeric), na.rm = TRUE),
    legal_assist = case_when(
      # () ~ 4,
      (
        legal_assist_level_3_cnt > 0
      ) ~ 3,
      (
        legal_assist_level_2_cnt > 0 &
          legal_assist_level_3_cnt == 0
      ) ~ 2,
      (
        as.numeric(`K_20_legal_assist/none`) == 1
      ) ~ 1,
      (
        as.numeric(`K_20_legal_assist/dont_know` == 1) |
          as.numeric(`K_20_legal_assist/prefer_not_to_answer` == 1) |
          is.na(`K_20_legal_assist`)
      ) ~ NA,
      TRUE ~ -1
    ),
    
    non_severe = rowSums(across(child_sep_non_severy, as.numeric), na.rm = TRUE),
    severe = rowSums(across(child_sep_severy, as.numeric), na.rm = TRUE),
    very_severe = rowSums(across(child_sep_very_severy, as.numeric), na.rm = TRUE),
    child_sep = case_when(
      (
        very_severe > 0
      ) ~ 4,
      (
        severe > 0 &
          very_severe == 0
      ) ~ 3,
      (
        non_severe > 0  &
          severe == 0 &
          very_severe == 0
      ) ~ 2,
      (
        `B_38_child_outside_hh` == "no"
      ) ~ 1,
      (
        is.na(`B_38_child_outside_hh`) |
          `B_38_child_outside_hh` == "dont_know" |
          `B_38_child_outside_hh` == "prefer_not_to_answer" |
          `B_39_child_outside_reason` == "dont_know" |
          `B_39_child_outside_reason` == "prefer_not_to_answer" |
          (`B_38_child_outside_hh` != "no" & is.na(`B_39_child_outside_reason`))
      ) ~ NA,
      TRUE ~ -1
    )
  ) %>%
  select(uuid, safety, barriers, needs, legal_assist, child_sep) #%>%
#  select(uuid,safety_corrected)

#table(protection$safety)

data.list$main <- data.list$main %>%
  left_join(protection, by = "uuid")