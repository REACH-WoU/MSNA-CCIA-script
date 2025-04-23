# crit 2

crit_2_q1_improved_dr <- c('bottled_water','water_kiosk','trucked_in_water','protected_well','protected_spring',
                           'public_tap_standpipe','public_well_or_boreholes','piped_into_compound_yard_or_plot',
                           'piped_to_neighbour','tanker_truck','cart_with_small_tank_drum','sachet_water')

crit_2_q1_improved_on_prem <- c('tap_drinking_water_piped_into_dwelling',
                                'technical_piped_water')


crit_2_q2_acceptable <- c('very_good','good','neither_good_nor_bad')

crit_2_q2_unacceptable <- c('bad','very_bad')


crit_2_q3_can_treat <- c('yes','no_no_need_to_treat_the_water')

crit_2_q3_cannot_treat <- c('no_need_to_but_cannot_treat_the_water')


# crit 3


crit_3_q1_improved_s <- c('flush_to_piped_sewer_system','flush_to_septic_tank','flush_to_pit_latrine',
                          'composting_toilet','pit_latrine_with_slab')


crit_3_q1_unimproved_s <- c('flush_to_dont_know_where','flush_to_open_drain','flush_to_elsewhere',
                            'pit_latrine_without_slab_open_pit','hanging_toilethanging_latrine')



crit_3_q2_can_manage <- c('no_my_household_does_not_handle_this_service','no_there_are_no_problems')

crit_3_q2_cannot_manage <- c('yes_it_is_expensive_high_price_of_the_service','yes_it_is_dirty',
                             'yes_special_car_service_refuses_to_come','yes_pumping_services_are_not_available_in_the_area',
                             'yes_emptying_the_sewage_is_not_possible_due_to_technical_reasons','other')

# crit 4

crit_4_q2_level_2 <- c('H_11_wash_issues_type/insufficient_essential_household_items_for_hygiene',
                       'H_11_wash_issues_type/insufficient_space',
                       'H_11_wash_issues_type/availability_of_hot_water','H_11_wash_issues_type/other',
                       'H_11_wash_issues_type/dont_know','H_11_wash_issues_type/prefer_not_to_answer')


crit_4_q2_level_3 <- c('H_11_wash_issues_type/inadequate_space','H_11_wash_issues_type/unsafe_space',
                       'H_11_wash_issues_type/availability_of_water',
                       'H_11_wash_issues_type/no_hygiene_facility_within_the_shelter')


# crit 5

crit_5_q1_level_undef <- c('dont_know','prefer_not_to_answer')

crit_5_q1_level_na <- c('none','winter_clothes_for_a_household_member','summer_clothes_for_a_household_member',
                        'bedding_and_towels','heating_appliances','fuel_for_heating','household_items',
                        'kitchen_set','large_kitchen_app','other')

crit_5_q1_level_2 <- c('other_personal_hygiene_products','domestic_hygiene_products')

crit_5_q1_level_3 <- c('feminine_hygiene_items','baby_diapers')

crit_5_q1_level_4 <- c('soap','water_treatment_product_and_materials')


# crit 6

crit_6_q1_level_1 <- c('we_use_specialized_waste_collection_service_for_regular_disposal','we_sort_waste_and_recycle')

crit_6_q1_level_2 <- c('we_take_the_garbage_to_designated_dumpsite_ourselves',
                       'we_burn_all_the_garbage','we_partially_burn_garbage_and_dispose_of_the_rest_in_a_pit_latrine_or_similar')

crit_6_q1_level_3 <- c('we_dispose_of_garbage_in_unauthorized_places')

crit_6_q1_level_undef <- c('other','dont_know','prefer_not_to_answer')


# crit 7

crit_7_q1_improved <- c('H_8_water_source_dom/tap_drinking_water_piped_into_dwelling','H_8_water_source_dom/trucked_in_water',
                        'H_8_water_source_dom/protected_well','H_8_water_source_dom/protected_spring',
                        'H_8_water_source_dom/public_tap_standpipe','H_8_water_source_dom/public_well_or_boreholes',
                        'H_8_water_source_dom/technical_piped_water','H_8_water_source_dom/piped_into_compound_yard_or_plot',
                        'H_8_water_source_dom/piped_to_neighbour','H_8_water_source_dom/tanker_truck',
                        'H_8_water_source_dom/cart_with_small_tank_drum','H_8_water_source_dom/sachet_water')

crit_7_q1_unimproved <- c('H_8_water_source_dom/unprotected_well','H_8_water_source_dom/unprotected_spring',
                          'H_8_water_source_dom/rainwater_collection','H_8_water_source_dom/surface_water')


crit_7_q1_stressed <- c('H_8_water_source_dom/bottled_water','H_8_water_source_dom/water_kiosk')

crit_7_q1_undef <- c('H_8_water_source_dom/other','H_8_water_source_dom/dont_know','H_8_water_source_dom/prefer_not_to_answer')


crit_7_q2_na <- c('G_16_utility_interrupt/none','G_16_utility_interrupt/gas','G_16_utility_interrupt/hot_water_supply',
                  'G_16_utility_interrupt/internet','G_16_utility_interrupt/other',
                  'G_16_utility_interrupt/dont_know','G_16_utility_interrupt/prefer_not_to_answer')


crit_7_q2_relevant <- c('G_16_utility_interrupt/sewage','G_16_utility_interrupt/cold_water_supply')



crit_7_q3_level_3 <- c('H_9_enough_water/cooking','H_9_enough_water/laundry_and_cleaning','H_9_enough_water/personal_hygiene')
 

crit_7_q3_level_4 <- c('H_9_enough_water/flushing_toilet','H_9_enough_water/none')

crit_7_q3_undef <- c('H_9_enough_water/dont_know','H_9_enough_water/prefer_not_to_answer')


wash <- data.list$main %>% 
  mutate(H_7_water_lack = as.numeric(H_7_water_lack)) %>% 
  mutate(
    wash_crit_1 = case_when(
      H_7_water_lack %_>_% 20 ~ 5,
      H_7_water_lack %_>_% 10 & H_7_water_lack %_<=_% 20 ~ 4,
      H_7_water_lack %_>_% 4 & H_7_water_lack %_<=_% 10  ~ 3,
      H_7_water_lack %_>_% 0 & H_7_water_lack %_<=_% 4 ~ 2,
      H_7_water_lack %==% 0 ~ 1,
      TRUE ~ NA_real_
    ),
    wash_crit_2 = case_when(
      H_2_water_source %in% c('rainwater_collection','surface_water')  ~ 5,
      
      # (H_2_water_source %in% crit_2_q1_improved_on_prem &
      #    H_5_water_assess %in% crit_2_q2_unacceptable) |
        (H_2_water_source %in% c('unprotected_well','unprotected_spring') &
           H_5_water_assess %in% crit_2_q2_unacceptable)~ 4,
      
      (H_2_water_source %in% crit_2_q1_improved_dr &
         (as.numeric(H_3_1_water_fetch_int) > 60 | (H_3_water_fetch == "dont_know" & H_4_water_range == "more_than_1_hour")) &
         H_5_water_assess %in% crit_2_q2_acceptable) |
      (H_2_water_source %in% c( c(crit_2_q1_improved_dr,crit_2_q1_improved_on_prem)) &
         H_5_water_assess %in% crit_2_q2_unacceptable)|
        # (H_2_water_source %in% c( c(crit_2_q1_improved_dr,crit_2_q1_improved_on_prem)) &
        #    H_5_water_assess %in% crit_2_q2_acceptable) |
        (H_2_water_source %in% c('unprotected_well','unprotected_spring') & H_5_water_assess %in% crit_2_q2_acceptable) ~ 3,
        # ((H_2_water_source %in% c( c(crit_2_q1_improved_dr)) &
        #     as.numeric(H_3_1_water_fetch_int) >30 &
        #     H_5_water_assess %in% crit_2_q2_acceptable &
        #     H_6_water_treat  %in% crit_2_q3_can_treat))|
        # ((H_2_water_source %in% c( c(crit_2_q1_improved_dr)) &
        #     H_4_water_range %in% c('more_than_30_minutes_but_less_than_1_hour','more_than_1_hour') &
        #     H_5_water_assess %in% crit_2_q2_acceptable &
        #     H_6_water_treat  %in% crit_2_q3_can_treat)) ~ 3,
      
      (H_2_water_source %in% crit_2_q1_improved_dr &
        (as.numeric(H_3_1_water_fetch_int) <= 60 | (H_3_water_fetch == "dont_know" & H_4_water_range != "more_than_1_hour")) &
        H_5_water_assess %in% crit_2_q2_acceptable) ~2, # |
        # (H_2_water_source %in% crit_2_q1_improved_dr &
        #    H_4_water_range %in%  '30_minutes_or_less' &
        #    H_5_water_assess %in% crit_2_q2_acceptable &
        #    H_6_water_treat  %in% crit_2_q3_can_treat) ~ 2,
      
      (H_2_water_source %in% crit_2_q1_improved_on_prem &
        H_5_water_assess %in% crit_2_q2_acceptable)|
        (H_2_water_source %in% crit_2_q1_improved_dr &
           H_3_water_fetch %in%  'do_not_cllect' &
           H_5_water_assess %in% crit_2_q2_acceptable) ~ 1,
      
      H_2_water_source %in% c('other','dont_know','prefer_not_to_answer') |
        H_5_water_assess %in% c('dont_know','prefer_not_to_answer') |
        H_6_water_treat  %in% c('dont_know','prefer_not_to_answer') | 
        H_3_water_fetch %in% c('dont_know','prefer_not_to_answer') |
        (!is.na(H_3_water_fetch) & is.na(H_3_1_water_fetch_int))|
        is.na(H_2_water_source) | is.na(H_5_water_assess) | is.na(H_6_water_treat)~ NA_real_
    ),
    
    wash_crit_3 = case_when(
      H_14_toilet %in% c('plastic_bag','bucket','none') |
        (H_14_toilet %in% crit_3_q1_unimproved_s &
           H_19_sewage %in% crit_3_q2_cannot_manage &
           H_17_safe_toilet %in% 'no') ~ 5,
      
      (H_14_toilet %in% crit_3_q1_improved_s &
         H_19_sewage %in% crit_3_q2_cannot_manage&
         H_17_safe_toilet %in% 'no')  |
        (H_14_toilet %in% crit_3_q1_unimproved_s &
           H_19_sewage %in% crit_3_q2_cannot_manage &
           H_17_safe_toilet %in% 'yes') |
        (H_14_toilet %in% crit_3_q1_unimproved_s &
           (H_19_sewage %in% crit_3_q2_can_manage | is.na(H_19_sewage))&
           H_17_safe_toilet %in% 'no') ~ 4,
      
      (H_14_toilet %in% crit_3_q1_improved_s &
         H_19_sewage %in% crit_3_q2_cannot_manage &
         H_17_safe_toilet %in% 'yes') |
        (H_14_toilet %in% crit_3_q1_unimproved_s &
           (H_19_sewage %in% crit_3_q2_can_manage | is.na(H_19_sewage))&
           H_17_safe_toilet %in% 'yes')~ 3,
      
      (H_14_toilet %in% crit_3_q1_improved_s &
        (H_19_sewage %in% crit_3_q2_can_manage | is.na(H_19_sewage))&
        H_17_safe_toilet %in% 'no') |
        
        (H_14_toilet %in% crit_3_q1_improved_s &
        (H_19_sewage %in% crit_3_q2_can_manage | is.na(H_19_sewage))&
        H_17_safe_toilet %in% 'yes' & H_15_shared_toilet  == 'yes') ~ 2,
      
      H_14_toilet %in% crit_3_q1_improved_s &
        (H_19_sewage %in% crit_3_q2_can_manage | is.na(H_19_sewage))&
        H_17_safe_toilet %in% 'yes' & H_15_shared_toilet =='no' ~ 1,
      
      H_14_toilet %in% c('other','dont_know','prefer_not_to_answer')|
        H_19_sewage %in% c('dont_know','prefer_not_to_answer')|
        H_15_shared_toilet %in% c('dont_know','prefer_not_to_answer')|
        H_17_safe_toilet %in% c('dont_know','prefer_not_to_answer')|
        is.na(H_14_toilet)  | is.na(H_17_safe_toilet)~ NA_real_
    ),
    
    wash_crit_4 = case_when(
      (H_10_wash_issues %in% 'no_cannot_do' ) |
        (H_10_wash_issues %in% 'yes_with_issues'  & rowSums(across(all_of(crit_4_q2_level_3),.fns = as.numeric), na.rm = T) >1)~ 4,
      
      (H_10_wash_issues %in% 'yes_with_issues'  & rowSums(across(all_of(crit_4_q2_level_3),.fns = as.numeric), na.rm = T) ==1) |
        (H_10_wash_issues %in% 'yes_with_issues'  & rowSums(across(all_of(crit_4_q2_level_2),.fns = as.numeric), na.rm = T) >1)~ 3,
      
      H_10_wash_issues %in% 'yes_with_issues'  & rowSums(across(all_of(crit_4_q2_level_2),.fns = as.numeric), na.rm = T) ==1 ~ 2,
      H_10_wash_issues %in% 'yes_without_any_issues' ~ 1,
      is.na(H_11_wash_issues_type) ~ NA_real_
    ),
    
    wash_crit_5 = case_when(
      
      grepl(paste0('(',paste0(crit_5_q1_level_4, collapse = ')|('),')'),J_25_miss_nfi) | 
        (H_13_soap == "no") | (H_12_hand_washing == "no_handwashing_place_in_dwellingyardplot") ~ 3,
      grepl(paste0('(',paste0(crit_5_q1_level_3, collapse = ')|('),')'),J_25_miss_nfi) |
        (H_13_soap == "no") | (H_12_hand_washing == "no_handwashing_place_in_dwellingyardplot") ~ 3,
      
      (grepl(paste0('(',paste0(crit_5_q1_level_2, collapse = ')|('),')'),J_25_miss_nfi) & H_13_soap =='yes' & 
        H_12_hand_washing %in% 'mobile_object_reported') |
        
         (H_12_hand_washing %in% 'mobile_object_reported' &   H_13_soap =='yes' & 
            grepl(paste0('(',paste0(crit_5_q1_level_na, collapse = ')|('),')'),J_25_miss_nfi) &
            !grepl(paste0('(',paste0(crit_5_q1_level_2, collapse = ')|('),')'),J_25_miss_nfi)) |
        (grepl(paste0('(',paste0(crit_5_q1_level_2, collapse = ')|('),')'),J_25_miss_nfi) &
           H_12_hand_washing %in% c('fixed_facility_reported_in_dwelling', 'fixed_facility_reported_in_yardplot') &
           H_13_soap =='yes')~ 2,
      
      grepl(paste0('(',paste0(crit_5_q1_level_na, collapse = ')|('),')'),J_25_miss_nfi) &
        H_13_soap %in%'yes' & 
        (H_12_hand_washing %in% c('fixed_facility_reported_in_dwelling','fixed_facility_reported_in_yardplot')) ~ 1,
      
      grepl(paste0('(',paste0(crit_5_q1_level_undef, collapse = ')|('),')'),J_25_miss_nfi) | 
        is.na(J_25_miss_nfi) | H_12_hand_washing %in% c('prefer_not_to_answer','other','dont_know')|
        is.na(H_12_hand_washing) | is.na(H_13_soap)|
        H_13_soap %in% c('prefer_not_to_answer','other','dont_know')~ NA_real_,
      TRUE ~ -1
      
    ),
    
    # wash_crit_6 = case_when(
    #   H_18_garbage_disposal %in% crit_6_q1_level_3 ~ 3,
    #   H_18_garbage_disposal %in% crit_6_q1_level_2 ~ 2,
    #   H_18_garbage_disposal %in% crit_6_q1_level_1 ~ 1,
    #   H_18_garbage_disposal %in% crit_6_q1_level_undef | is.na(H_18_garbage_disposal) ~ NA_real_
    # ),
    
    # wash_crit_6 = case_when(
    #   (H_6_water_treat %in% c("no_need_to_but_cannot_treat_the_water")) ~ 3,
    #   #() ~ 2,
    #   (H_6_water_treat %in% c("yes", "no_no_need_to_treat_the_water", "dont_know")) ~ 1,
    #   H_6_water_treat == "prefer_not_to_answer" ~ NA,
    #   TRUE ~ -1
    # ),

    wash_crit_7 = case_when(
      (rowSums(across(all_of(crit_7_q1_improved),.fns = as.numeric), na.rm = T) >0 &
         (rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) > 
            rowSums(cbind(as.numeric(`H_8_water_source_dom/bottled_water`),as.numeric(`H_8_water_source_dom/water_kiosk`)), na.rm=T)) &
         (as.numeric(`H_9_enough_water/flushing_toilet`) %in% 0 |
            as.numeric(`H_9_enough_water/none`) %in% 1 )) |
        (rowSums(across(all_of(crit_7_q1_improved),.fns = as.numeric), na.rm = T) ==0 &
           rowSums(across(all_of(crit_7_q1_stressed),.fns = as.numeric), na.rm = T) == 0 &
           rowSums(across(all_of(crit_7_q1_undef),.fns = as.numeric), na.rm = T) == 0 &
           rowSums(across(all_of(crit_7_q1_unimproved),.fns = as.numeric), na.rm = T) >0 &
           ((rowSums(across(all_of(c(crit_7_q3_level_3,"H_9_enough_water/flushing_toilet")),.fns = as.numeric), na.rm = T) > 0 &
               rowSums(across(all_of(c(crit_7_q3_level_3,"H_9_enough_water/flushing_toilet")),.fns = as.numeric), na.rm = T) < 4) |
              is.na(`H_9_enough_water/flushing_toilet`) | as.numeric(`H_9_enough_water/none`)==1) ) |
        (rowSums(across(all_of(crit_7_q1_improved),.fns = as.numeric), na.rm = T) ==0 &
           rowSums(across(all_of(crit_7_q1_stressed),.fns = as.numeric), na.rm = T) > 0 &
           rowSums(across(all_of(crit_7_q1_undef),.fns = as.numeric), na.rm = T) == 0 &
           rowSums(across(all_of(crit_7_q1_unimproved),.fns = as.numeric), na.rm = T) ==0) ~ 4,
      
      (rowSums(across(all_of(c(crit_7_q1_stressed,crit_7_q1_improved)),.fns = as.numeric), na.rm = T) >0 &
         (rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) > 
            rowSums(cbind(as.numeric(`H_8_water_source_dom/bottled_water`),as.numeric(`H_8_water_source_dom/water_kiosk`)), na.rm=T))&
         ((as.numeric(`H_9_enough_water/flushing_toilet`) == 1 | is.na(`H_9_enough_water/flushing_toilet`)) &
            rowSums(across(all_of(crit_7_q3_level_3),.fns = as.numeric), na.rm = T) < 3)) | 
        
        (rowSums(across(all_of(crit_7_q1_improved),.fns = as.numeric), na.rm = T) ==0 &
           rowSums(across(all_of(crit_7_q1_stressed),.fns = as.numeric), na.rm = T) == 0 &
           rowSums(across(all_of(crit_7_q1_undef),.fns = as.numeric), na.rm = T) == 0 &
           rowSums(across(all_of(crit_7_q1_unimproved),.fns = as.numeric), na.rm = T) >0 &
           rowSums(across(all_of(crit_7_q3_level_3),.fns = as.numeric), na.rm = T) ==3 &
           (as.numeric(`H_9_enough_water/flushing_toilet`) == 1 | is.na(`H_9_enough_water/flushing_toilet`)))  ~ 3,
      
      rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) >0 &
        (rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) > 
           rowSums(cbind(as.numeric(`H_8_water_source_dom/bottled_water`),as.numeric(`H_8_water_source_dom/water_kiosk`)), na.rm=T)) &
        rowSums(across(all_of(crit_7_q3_level_3),.fns = as.numeric), na.rm = T) ==3 &
        (as.numeric(`H_9_enough_water/flushing_toilet`) == 1 | is.na(`H_9_enough_water/flushing_toilet`)) &
        rowSums(across(all_of(crit_7_q2_relevant),.fns = as.numeric), na.rm = T) >0 ~ 2,
      
      rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) >0 &
        (rowSums(across(all_of(c(crit_7_q1_improved,crit_7_q1_stressed)),.fns = as.numeric), na.rm = T) > 
           rowSums(cbind(as.numeric(`H_8_water_source_dom/bottled_water`),as.numeric(`H_8_water_source_dom/water_kiosk`)), na.rm=T)) &
        rowSums(across(all_of(crit_7_q3_level_3),.fns = as.numeric), na.rm = T) ==3 &
        (as.numeric(`H_9_enough_water/flushing_toilet`) == 1 | is.na(`H_9_enough_water/flushing_toilet`)) &
        rowSums(across(all_of(crit_7_q2_na),.fns = as.numeric), na.rm = T) >0 &
        rowSums(across(all_of(crit_7_q2_relevant),.fns = as.numeric), na.rm = T) ==0 ~ 1,
      
      rowSums(across(all_of(crit_7_q1_undef),.fns = as.numeric), na.rm = T)>0 |
        rowSums(across(all_of(crit_7_q3_undef),.fns = as.numeric), na.rm = T)>0 |
        is.na(H_8_water_source_dom) | is.na(G_16_utility_interrupt)~ NA_real_
    )
  ) %>%
  select(uuid, wash_crit_1, wash_crit_2, wash_crit_3, wash_crit_4, wash_crit_5,
         # wash_crit_6,
         wash_crit_7)


data.list$main <- data.list$main %>%
  left_join(wash, by = "uuid")
