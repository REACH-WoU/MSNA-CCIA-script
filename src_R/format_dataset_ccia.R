################################################################################
### FORMAT DATASET
################################################################################

# Run the indeces -----------------------------

print('Running the LSGs')
source('src_R/max_lsg.R')

source('indices/livelihoods_index.R') # CHECK LIVELIHOOD CRIT_3
source('indices/protection_index.R')
source('indices/education_index_v4.R') # CHECK
source('indices/shelter_index.R') # CHECK SHELTER CRIT_3
source('indices/health_index_v6.R')
source('indices/wash_index_v4.R') # CHECK WASH CRIT_1
source('indices/food_security.R')

#names_added <- setdiff(names(data.list[[1]]), names_og)
  
source('indices/lsg_msni.R')

# source('daf_creator.R')

# write.xlsx(data.list, 'data/MSNA2403_2024_final_anonymized_data_19July2024_weighted_index_added.xlsx')


#-------------------------------- intercrossing --------------------------------------
if (TRUE){
  

  
lsg_columns <- data.list$main %>% select(starts_with("lsg_")) %>% colnames

lsg_no_protection <- setdiff(lsg_columns,'lsg_protection')
lsg_no_shelter <- setdiff(lsg_columns,'lsg_shelter_nfi')
lsg_no_education <- setdiff(lsg_columns,'lsg_education')
lsg_no_wash <- setdiff(lsg_columns,'lsg_wash')
lsg_no_livelihoods <- setdiff(lsg_columns,'lsg_livelihoods')
lsg_no_health <- setdiff(lsg_columns,'lsg_health')
lsg_no_food <- setdiff(lsg_columns,'lsg_food_security')


intercrossinglsgs_3_4_5 <- data.list$main%>%
  mutate_at(lsg_columns, as.numeric) %>%
  mutate_at(lsg_columns, ~ifelse(. %in% c(3,4,5),1,0)) %>%
  select(uuid,all_of(lsg_columns))

intercrossinglsgs_4_5 <- data.list$main%>%
  mutate_at(lsg_columns, as.numeric) %>%
  mutate_at(lsg_columns, ~ifelse(. %in% c(4,5),1,0)) %>%
  select(uuid,all_of(lsg_columns))


intercrossing_3_4_5 <- intercrossinglsgs_3_4_5 %>%
  mutate(lsg_education = ifelse(lsg_education == 1, "Education", NA)) %>%
  mutate(lsg_shelter_nfi = ifelse(lsg_shelter_nfi == 1, "Shelter_NFI", NA)) %>%
  mutate(lsg_wash = ifelse(lsg_wash == 1, "WASH", NA)) %>%
  mutate(lsg_protection = ifelse(lsg_protection == 1, "Protection", NA)) %>%
  mutate(lsg_health = ifelse(lsg_health == 1, "Health", NA)) %>%
  mutate(lsg_food_security = ifelse(lsg_food_security == 1, "Food Security", NA)) %>%
  mutate(lsg_livelihoods = ifelse(lsg_livelihoods == 1, "Livelihoods", NA)) %>%
  unite("Merged_3_4_5", lsg_columns, sep = "; ", na.rm = T) %>%
  mutate(Merged_3_4_5 = ifelse(Merged_3_4_5 == "","Combination (1,2)",Merged_3_4_5))


intercrossing_4_5 <- intercrossinglsgs_4_5 %>%
  mutate(lsg_education = ifelse(lsg_education == 1, "Education", NA)) %>%
  mutate(lsg_shelter_nfi = ifelse(lsg_shelter_nfi == 1, "Shelter_NFI", NA)) %>%
  mutate(lsg_wash = ifelse(lsg_wash == 1, "WASH", NA)) %>%
  mutate(lsg_protection = ifelse(lsg_protection == 1, "Protection", NA)) %>%
  mutate(lsg_health = ifelse(lsg_health == 1, "Health", NA)) %>%
  mutate(lsg_food_security = ifelse(lsg_food_security == 1, "Food Security", NA)) %>%
  mutate(lsg_livelihoods = ifelse(lsg_livelihoods == 1, "Livelihoods", NA)) %>%
  unite("Merged_4_5", lsg_columns, sep = "; ", na.rm = T) %>%
  mutate(Merged_4_5 = ifelse(Merged_4_5 == "","Combination (1,2,3)",Merged_4_5))



data.list$main <- data.list$main %>%
  left_join(intercrossing_3_4_5, by = "uuid") %>%
  left_join(intercrossing_4_5, by = 'uuid')


data.list$main <- data.list$main %>%
  mutate(
    LSG_incidence_general = case_when(
      rowSums(across(lsg_columns, ~ .x %in% c(3,4,5)), na.rm = T) > 0 ~ 'any sectoral LSG',
      rowSums(across(lsg_columns, ~ .x %in% c(1,2)), na.rm = T) > 0 ~ 'no sectoral LSG',
    ),

    LSG_incidence_general_extreme = case_when(
      rowSums(across(lsg_columns, ~ .x %in% c(4,5)), na.rm = T) > 0 ~ 'any sectoral LSG',
      rowSums(across(lsg_columns, ~ .x %in% c(1,2,3)), na.rm = T) > 0 ~ 'no sectoral LSG',
    ),
    LSG_incidence_detail = case_when(
      rowSums(across(lsg_columns, ~ .x %in% c( 3,4,5 )),na.rm = T) >= 4 ~ '4 or more sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x %in% c( 3,4,5 )),na.rm = T) == 3 ~ '3 sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x %in% c( 3,4,5 )),na.rm = T) == 2 ~ '2 sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x %in% c( 3,4,5 )),na.rm = T) == 1 ~ '1 sectoral LSG',
      rowSums(across(lsg_columns, ~ .x %in% c( 3,4,5 )),na.rm = T) == 0 ~ '0 sectoral LSGs'
    ),
    LSG_incidence_detail_extreme = case_when(
      rowSums(across(lsg_columns, ~ .x  %in% c( 4,5 )),na.rm = T) >= 4 ~ '4 or more sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x  %in% c( 4,5 )),na.rm = T) == 3 ~ '3 sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x  %in% c( 4,5 )),na.rm = T) == 2 ~ '2 sectoral LSGs',
      rowSums(across(lsg_columns, ~ .x  %in% c( 4,5 )),na.rm = T) == 1 ~ '1 sectoral LSG',
      rowSums(across(lsg_columns, ~ .x  %in% c( 4,5 )),na.rm = T) == 0 ~ '0 sectoral LSGs'
    )
  ) %>%
  mutate(
    shelter_lsg = case_when(lsg_shelter_nfi%in% c( 3,4,5 ) ~ 'Shelter vulterability',
                            lsg_shelter_nfi%in% c( 1,2 ) ~ 'No shelter LSG'),
    education_lsg = case_when(lsg_education%in% c( 3,4,5 ) ~ 'Education vulterability',
                              lsg_education%in% c( 1,2 ) ~ 'No education LSG'
    ),
    wash_lsg = case_when(lsg_wash%in% c( 3,4,5 ) ~ 'Wash vulterability',
                         lsg_wash%in% c( 1,2 ) ~ 'No wash LSG'
    ),
    livelihoods_lsg = case_when(lsg_livelihoods%in% c( 3,4,5 ) ~ 'Livelihoods vulterability',
                                lsg_livelihoods%in% c( 1,2 ) ~ 'No livelihoods LSG'
    ),
    health_lsg = case_when(lsg_health%in% c( 3,4,5 ) ~ 'Health vulterability',
                           lsg_health%in% c( 1,2 ) ~ 'No health LSG'
    ),
    protection_lsg = case_when(lsg_protection%in% c( 3,4,5 ) ~ 'Protection vulterability',
                               lsg_protection%in% c( 1,2 ) ~ 'No protection LSG'
    ),
    food_lsg = case_when(lsg_food_security%in% c( 3,4,5 ) ~ 'Food vulterability',
                         lsg_food_security%in% c( 1,2 ) ~ 'No food LSG'
    ),
    shelter_lsg_extreme = case_when(lsg_shelter_nfi%in% c( 4,5 ) ~ 'Shelter vulterability',
                                    lsg_shelter_nfi%in% c( 1,2,3 ) ~ 'No shelter LSG'
    ),
    education_lsg_extreme = case_when(lsg_education%in% c( 4,5 ) ~ 'Education vulterability',
                                      lsg_education%in% c( 1,2,3 ) ~ 'No education LSG'
    ),
    wash_lsg_extreme = case_when(lsg_wash%in% c( 4,5 ) ~ 'Wash vulterability',
                                 lsg_wash%in% c( 1,2,3 ) ~ 'No wash LSG'
    ),
    livelihoods_lsg_extreme = case_when(lsg_livelihoods%in% c( 4,5 ) ~ 'Livelihoods vulterability',
                                        lsg_livelihoods%in% c( 1,2,3 ) ~ 'No livelihoods LSG'
    ),
    health_lsg_extreme = case_when(lsg_health%in% c( 4,5 ) ~ 'Health vulterability',
                                   lsg_health%in% c( 1,2,3 ) ~ 'No health LSG'
    ),
    protection_lsg_extreme = case_when(lsg_protection%in% c( 4,5 ) ~ 'Protection vulterability',
                                       lsg_protection%in% c( 1,2,3 ) ~ 'No protection LSG'
    ),
    food_lsg_extreme = case_when(lsg_food_security%in% c( 4,5 ) ~ 'Food vulterability',
                                 lsg_food_security%in% c( 1,2,3 ) ~ 'No food LSG'
    )
  ) %>%
  # add the final overlap variables
  mutate(
    protection_overlaps = case_when(
      lsg_protection%in% c( 1,2 ) ~ 'HH with no protection LSG',
      lsg_protection%in% c( 3,4,5 ) & rowSums(across(lsg_no_protection, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in protection and other sectors',
      lsg_protection%in% c( 3,4,5 ) & rowSums(across(lsg_no_protection, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in protection',
      .default = 'NA'
    ),
    shelter_overlaps = case_when(
      lsg_shelter_nfi%in% c( 1,2 ) ~ 'HH with no shelter LSG',
      lsg_shelter_nfi%in% c( 3,4,5 ) & rowSums(across(lsg_no_shelter, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in shelter and other sectors',
      lsg_shelter_nfi%in% c( 3,4,5 ) & rowSums(across(lsg_no_shelter, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in shelter',
      .default = 'NA'

    ),
    education_overlaps = case_when(
      lsg_education%in% c( 1,2 ) ~ 'HH with no education LSG',
      lsg_education%in% c( 3,4,5 ) & rowSums(across(lsg_no_education, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in education and other sectors',
      lsg_education%in% c( 3,4,5 ) & rowSums(across(lsg_no_education, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in education',
      .default = 'NA'

    ),
    wash_overlaps = case_when(
      lsg_wash%in% c( 1,2 ) ~ 'HH with no WASH LSG',
      lsg_wash%in% c( 3,4,5 ) & rowSums(across(lsg_no_wash, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in WASH and other sectors',
      lsg_wash%in% c( 3,4,5 ) & rowSums(across(lsg_no_wash, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in WASH',
      .default = 'NA'

    ),
    livelihoods_overlaps = case_when(
      lsg_livelihoods%in% c( 1,2 ) ~ 'HH with no livelyhoods LSG',
      lsg_livelihoods%in% c( 3,4,5 ) & rowSums(across(lsg_no_livelihoods, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in livelyhoods and other sectors',
      lsg_livelihoods%in% c( 3,4,5 ) & rowSums(across(lsg_no_livelihoods, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in livelyhoods',
      .default = 'NA'

    ),
    health_overlaps = case_when(
      lsg_health%in% c( 1,2 ) ~ 'HH with no health LSG',
      lsg_health%in% c( 3,4,5 ) & rowSums(across(lsg_no_health, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in health and other sectors',
      lsg_health%in% c( 3,4,5 ) & rowSums(across(lsg_no_health, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in health',
      .default = 'NA'

    ),
    food_overlaps = case_when(
      lsg_food_security%in% c( 1,2 ) ~ 'HH with no Food Security LSG',
      lsg_food_security%in% c( 3,4,5 ) & rowSums(across(lsg_no_food, ~ .x %in% c(3,4,5)), na.rm = T)>0 ~ 'HH with LSG in Food Security and other sectors',
      lsg_food_security%in% c( 3,4,5 ) & rowSums(across(lsg_no_food, ~ .x %in% c(3,4,5)), na.rm = T)==0 ~ 'HH with LSG only in Food Security',
      .default = 'NA'

    )
  )



# co-occurence analysis

ls <- c('Shelter_NFI', 'Livelihoods', 'Health', 'Protection', 'Food Security','WASH', 'Education' )

list_unique <- combn(ls, 2, simplify = FALSE)

for (i in 1:length(list_unique)){
  string1 <- list_unique[[i]][1]
  string2 <- list_unique[[i]][2]

  data.list$main[,paste0('cooccurence_',string1,'_',string2)] <- ifelse(grepl(string1, data.list$main$Merged_3_4_5) & grepl(string2, data.list$main$Merged_3_4_5),
                                                                        'Yes','No')

  data.list$main[,paste0('extreme_cooccurence_',string1,'_',string2)] <- ifelse(grepl(string1, data.list$main$Merged_4_5) & grepl(string2, data.list$main$Merged_4_5),
                                                                                'Yes','No')
}



list_unique <- combn(ls, 3, simplify = FALSE)

for (i in 1:length(list_unique)){
  string1 <- list_unique[[i]][1]
  string2 <- list_unique[[i]][2]
  string3 <- list_unique[[i]][3]

  data.list$main[,paste0('cooccurence_',string1,'_',string2,'_',string3)] <- ifelse(grepl(string1, data.list$main$Merged_3_4_5) &
    grepl(string2, data.list$main$Merged_3_4_5)& grepl(string3, data.list$main$Merged_3_4_5), 'Yes','No')

  data.list$main[,paste0('extreme_cooccurence_',string1,'_',string2,'_',string3)] <- ifelse(grepl(string1, data.list$main$Merged_4_5) &
    grepl(string2, data.list$main$Merged_4_5)& grepl(string3, data.list$main$Merged_4_5), 'Yes', 'No')

}


# co-occurence analysis V2



ls <- c('Shelter_NFI', 'Livelihoods', 'Health', 'Protection', 'Food Security','WASH', 'Education' )

data.list$main$Merged_3_4_5 <- gsub('\\/','_',data.list$main$Merged_3_4_5)
data.list$main$Merged_4_5 <- gsub('\\/','_',data.list$main$Merged_4_5)

for (item in ls){
  
  nm_ls <- gsub(' ','_',item)
  
  data.list$main[,paste0('cooccurence_v2_',nm_ls,'_only')] <- case_when(
    !grepl(item, data.list$main$Merged_3_4_5) ~ NA, 
    data.list$main$Merged_3_4_5 == item ~ 'Only this sector is Severe+',
    TRUE ~ paste('Cooccurrence with some other sector(s)',item)
  )
  
  data.list$main[,paste0('extreme_cooccurence_v2_',nm_ls,'_only')] <- case_when(
    !grepl(item, data.list$main$Merged_4_5) ~ NA, 
    data.list$main$Merged_4_5 == item ~ 'Only this sector is Extereme+',
    TRUE ~ paste('Cooccurrence with some other sector',item)
  )
  
  
}



list_unique <- expand.grid(ls,ls) %>% 
  filter(Var1 != Var2) %>% 
  rename(var1 = Var2,
         var2 = Var1) %>% 
  mutate(var1 = as.character(var1),
         var2 = as.character(var2))


for (i in 1:nrow(list_unique)){
  string1 <- list_unique$var1[[i]]
  string2 <- list_unique$var2[[i]]
  
  data.list$main[,paste0('cooccurence_v2_',string1,'_',string2)] <- case_when(
    !grepl(string1, data.list$main$Merged_3_4_5) ~ NA, 
    grepl(string1, data.list$main$Merged_3_4_5) & grepl(string2, data.list$main$Merged_3_4_5) ~ 'Relevant cooccurence',
    TRUE ~ paste('Other cooccurence of',string1)
    )
  
  data.list$main[,paste0('extreme_cooccurence_v2_',string1,'_',string2)] <- case_when(
    !grepl(string1, data.list$main$Merged_4_5) ~ NA, 
    grepl(string1, data.list$main$Merged_4_5) & grepl(string2, data.list$main$Merged_4_5) ~ 'Relevant cooccurence',
    TRUE ~ paste('Other extreme cooccurence of',string1)
  )
}


write.xlsx(data.list, paste0('data/',strings['dataset.name'],' dataset ',strings['out_date'],'.xlsx'))


}
