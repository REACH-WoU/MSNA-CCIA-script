
# rCSI is already there: columns rcsi (numeric) and rcsi_score (categorical)
# but here we will make an adjusted rcsi with imputed values

short_exp_ecmen <- c("J_5_exp_water",
                     "J_6_exp_fem",
                     "J_7_exp_hygiene",
                     "J_8_exp_nfi",
                     "J_9_exp_utilities",
                     "J_10_exp_fuel",
                     "J_11_exp_transport",
                     "J_12_exp_comm",
                     "J_13_exp_childcare",
                     "J_15_exp_other",
                     "J_16_exp_heating"
)
long_exp_ecmen <- c("J_19_long_exp_shelter",
                    "J_20_long_exp_nfi",
                    "J_21_long_exp_health",
                    "J_22_long_exp_nfi",
                    "J_24_long_exp_other"
)
exp_food <- c("I_27_cereals",
              "I_27_1_cereals_own_prod_val",
              "I_28_fruit",
              "I_28_1_fruit_own_prod_val",
              "I_29_animal",
              "I_29_1_animal_own_prod_val",
              "I_30_sugar",
              "I_30_1_sugar_own_prod_val",
              "I_31_bread",
              "I_31_1_bread_own_prod_val",
              "I_32_water",
              "I_33_outside",
              "I_34_all",
              "I_34_1_other_own_prod_val"
)
data.list$main <- data.list$main %>%
  mutate(food_exp_0 = rowSums(across(exp_food, as.numeric),na.rm = TRUE),
         total_exp_ecmen_0 = rowSums(across(short_exp_ecmen, as.numeric),na.rm = TRUE) + rowSums(across(long_exp_ecmen, as.numeric),na.rm = TRUE)/6) %>%
  mutate(food_exp_0 = ifelse(food_exp_0==0,as.numeric(J_3_exp_food),food_exp_0)) %>%
  mutate(ecmen_0 = ifelse(is.na(J_38_inc_aid),total_exp_ecmen_0 + food_exp_0,total_exp_ecmen_0 + food_exp_0 - as.numeric(J_38_inc_aid)),
         ecmen_per_cap_0 = ifelse(ecmen_0<=0,0,ecmen_0 / as.numeric(B_2_hh_size))) 
# Variables to impute

variables <- read_xlsx("indices/variables_to_impute.xlsx")

# First imputing zeros where it is necessary
for (i in 1:nrow(variables)) {
  # Get the column names from the variables dataframe
  num_var <- variables$num_variable[i]
  can_var <- variables$can_variable[i]
  
  # Check for NA in both num_variable and can_variable in the main dataframe
  data.list$main[[num_var]] <- ifelse(
    is.na(data.list$main[[num_var]]) & (is.na(data.list$main[[can_var]]) | data.list$main[[can_var]] == "can"), 
    0, 
    data.list$main[[num_var]]
  )
  data.list$main[[num_var]] <- as.numeric(data.list$main[[num_var]])
}

# Creating parameters

data.list$main$hh_size_par <- as.numeric(data.list$main$B_2_hh_size)
data.list$main$total_inc_per_capita <- as.numeric(data.list$main$total_inc_per_capita)
hhAllChildren <- data.list$hh_members %>%
  select(uuid, B_5_hh_mem_age) %>%
  filter(B_5_hh_mem_age %_<_% 18) %>%
  group_by(uuid) %>%
  summarise(
    children_par = n()
  ) %>%
  select(uuid, children_par)
data.list$main <- data.list$main %>%
  left_join(hhAllChildren, by = "uuid") %>%
  mutate(children_par = ifelse(is.na(children_par),0,children_par))
data.list$main$gender_par <- ifelse(data.list$main$resp_gender == "Men-respondents", 1,0)
data.list$main$age_par <- as.numeric(data.list$main$A_5_resp_age)
data.list$main$rural_par <- ifelse(is.na(data.list$main$rural_urban),0,ifelse(data.list$main$rural_urban == "urban", 1, 0))
data.list$main$income_par <- ifelse(is.na(data.list$main$total_inc_per_capita) | data.list$main$total_inc_per_capita==0,1,data.list$main$total_inc_per_capita/1000)
data.list$main$disability_par <- ifelse(data.list$main$disability_cumulative == "HH with a member with disability",1,0)
data.list$main$prod_var <- ifelse(is.na(data.list$main$I_25_2_own_prod_food),0,ifelse(data.list$main$I_25_2_own_prod_food == "none",0,1))
data.list$main <- data.list$main %>% 
  mutate(n = 1) %>%
  pivot_wider(names_from = oblast, values_from = n, values_fill = 0, names_prefix = "obl_")

# Create an empty dataframe to store model parameters
model_results <- data.frame(
  variable = character(),
  predictor = character(),  # Column for the predictor name
  coefficient = numeric(),
  std_error = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

quartile_comparison <- data.frame(
  variable = character(),
  number_imputed = numeric(),
  quartile = character(),
  original_q1 = numeric(),
  imputed_q1 = numeric(),
  difference_percent_q1 = numeric(),
  original_q2 = numeric(),
  imputed_q2 = numeric(),
  difference_percent_q2 = numeric(),
  original_q3 = numeric(),
  imputed_q3 = numeric(),
  difference_percent_q3 = numeric(),
  stringsAsFactors = FALSE
)

#Also remove zeros from the utilities and heating expenditures
data.list$main$J_16_exp_heating[data.list$main$J_16_exp_heating == 0] <- NA
data.list$main$J_9_exp_utilities[data.list$main$J_9_exp_utilities == 0] <- NA

# Loop through each column in variables$num_variable
for (var in variables$num_variable) {
  original_data <- data.list$main[[var]]
  
  # Calculate the quartiles of the original data (before imputation)
  original_quartiles <- quantile(original_data, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

  # Subset data to include only rows where the variable has NA values
  missing_data <- data.list$main %>%
    filter(is.na(get(var)))
    
  # Ensure all predictor variables are numeric or factors, and handle missing predictors
  data_clean <- data.list$main %>%
    filter(complete.cases(hh_size_par, children_par, gender_par, age_par, rural_par, income_par, 
                          disability_par, prod_var, obl_UA32, obl_UA26, obl_UA48, obl_UA46, obl_UA59,
                          obl_UA63, obl_UA56, obl_UA68, obl_UA61, obl_UA05, obl_UA73, obl_UA21, 
                          obl_UA12, obl_UA18, obl_UA51, obl_UA35, obl_UA07, obl_UA80, obl_UA74, 
                          obl_UA71, obl_UA23, obl_UA53, obl_UA65, obl_UA14))# Ensure predictors are clean
  
  # Build formula for the GLM model
  glm_formula <- as.formula(paste(var, "~ hh_size_par + children_par + gender_par + age_par + rural_par + 
                                  income_par + disability_par + prod_var + obl_UA32 + obl_UA26 + obl_UA48 + 
                                  obl_UA46 + obl_UA59 + obl_UA63 + obl_UA56 + obl_UA68 + obl_UA61 + obl_UA05 + 
                                  obl_UA73 + obl_UA21 + obl_UA12 + obl_UA18 + obl_UA51 + obl_UA35 + obl_UA07 + 
                                  obl_UA80 + obl_UA74 + obl_UA71 + obl_UA23 + obl_UA53 + obl_UA65 + obl_UA14"))
  
  # Fit the GLM model using the rows with complete cases for the predictors
  glm_model <- glm(glm_formula, data = data_clean, family = gaussian())
  
  # Predict the missing values
  predictions <- predict(glm_model, newdata = missing_data, type = "response")
  
  # Polish predictions and replace NAs in the original data with the predicted values
  predictions_rounded <- round(predictions)
  predictions_clipped <- pmin(pmax(predictions_rounded, min(original_data, na.rm = TRUE)), 
                              max(original_data, na.rm = TRUE))
  
  data.list$main[is.na(data.list$main[[var]]), var] <- predictions_clipped
  
  imputed_data <- data.list$main[[var]]
  
  # Replace values in the 99.5th and 0.5th percentiles with the median
  p_995 <- quantile(imputed_data, 0.995, na.rm = TRUE)
  p_05 <- quantile(imputed_data, 0.005, na.rm = TRUE)
  median_value <- median(imputed_data, na.rm = TRUE)
  
  # Replace values greater than the 99.5th percentile with the median
  imputed_data[imputed_data > p_995] <- median_value
  
  # Replace values less than the 0.5th percentile with the median
  imputed_data[imputed_data < p_05] <- median_value
  
  # Update the dataframe with the modified imputed data
  data.list$main[[var]] <- imputed_data
  
  # Extract coefficients, standard errors, and p-values from the GLM model
  summary_glm <- summary(glm_model)
  
  # Loop through the coefficients and store them in the dataframe
  for (coef_name in rownames(summary_glm$coefficients)) {
    model_results <- rbind(model_results, data.frame(
      variable = var,
      predictor = coef_name,  # Add the predictor name
      coefficient = summary_glm$coefficients[coef_name, 1],
      std_error = summary_glm$coefficients[coef_name, 2],
      p_value = summary_glm$coefficients[coef_name, 4]
    ))
  }
  # Get the imputed data (after imputation)
  imputed_data <- predictions_clipped
  
  # Calculate the quartiles of the imputed data
  imputed_quartiles <- quantile(imputed_data, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  
  # Calculate the percent differences for each quartile
  percent_diff_q1 <- ((imputed_quartiles[1] - original_quartiles[1]) / original_quartiles[1]) * 100
  percent_diff_q2 <- ((imputed_quartiles[2] - original_quartiles[2]) / original_quartiles[2]) * 100
  percent_diff_q3 <- ((imputed_quartiles[3] - original_quartiles[3]) / original_quartiles[3]) * 100
  
  # Add the results to the comparison dataframe
  quartile_comparison <- rbind(quartile_comparison, data.frame(
    variable = var,
    number_imputed = length(imputed_data),
    quartile = "Q1",
    original_q1 = original_quartiles[1],
    imputed_q1 = imputed_quartiles[1],
    difference_percent_q1 = percent_diff_q1,
    original_q2 = original_quartiles[2],
    imputed_q2 = imputed_quartiles[2],
    difference_percent_q2 = percent_diff_q2,
    original_q3 = original_quartiles[3],
    imputed_q3 = imputed_quartiles[3],
    difference_percent_q3 = percent_diff_q3
  ))
}

# Calculating adjusted rCSI
data.list$main <- data.list$main %>%
  mutate(rcsi_adj = mapply(sum, (as.numeric(fsl_rcsi_lessquality)*3), (as.numeric(fsl_rcsi_borrow)*1), 
                       (as.numeric(fsl_rcsi_mealsize)*2), (as.numeric(fsl_rcsi_mealnb)*1), (as.numeric(fsl_rcsi_mealadult)*3)),
         rcsi_score_adj = case_when(rcsi_adj < 4 ~ "Low",
                                rcsi_adj <= 18 ~ "Medium",
                                rcsi_adj > 18 ~ "High",
                                TRUE ~ NA))

# Calculating ECMEN

data.list$main <- data.list$main %>%
  mutate(food_exp = rowSums(across(exp_food, as.numeric),na.rm = TRUE),
         total_exp_ecmen = rowSums(across(short_exp_ecmen, as.numeric),na.rm = TRUE) + rowSums(across(long_exp_ecmen, as.numeric),na.rm = TRUE)/6) %>%
  mutate(food_exp = ifelse(food_exp==0,as.numeric(J_3_exp_food),food_exp)) %>%
  mutate(ecmen = total_exp_ecmen + food_exp - J_38_inc_aid,
         ecmen_per_cap = ifelse(ecmen<=0,0,ecmen / as.numeric(B_2_hh_size))) 
# Applying deflators to ECMEN
deflators <- read_xlsx("indices/deflators.xlsx")
data.list$main <- data.list$main %>%
  left_join(deflators,by = "oblast_name") %>%
  mutate(ecmen_final = ecmen_per_cap * as.numeric(deflator))

# Columns FCS and LCSI must be already there is already there: columns fcs (numeric), fcs_score (categorical), lcsi_score (categorical)


#Converting all the measures into 1-4 scores
data.list$main <- data.list$main %>%
  mutate(ecmen_score = case_when(ecmen_final <= 3250 ~ 4,
                                 ecmen_final <= 6621 ~ 3,
                                 ecmen_final > 6621 ~ 1,
                                 TRUE ~ NA),
         CARI_current_status = case_when(
           fcs_score == "Acceptable" & rcsi_adj < 4 ~ 1,
           fcs_score == "Acceptable" & rcsi_adj >= 4 ~ 2,
           fcs_score == "Borderline" ~ 3,
           fcs_score == "Poor" ~ 4
         ))

data.list$main <- data.list$main %>%
  mutate(lcsi_score_num = case_when(is.na(lcsi_score) ~ NA,
                                    lcsi_score == "No coping" ~ 1,
                                    lcsi_score == "Stress" ~ 2,
                                    lcsi_score == "Crisis" ~ 3,
                                    lcsi_score == "Emergency" ~ 4)) %>%
  mutate(lcsi_score_fs = ifelse(is.na(`E_17_lcs_reason/to_access_or_pay_for_food`) | `E_17_lcs_reason/to_access_or_pay_for_food` == "0",1,lcsi_score_num))
data.list$main <- data.list$main %>%
  mutate(cari_score = ((lcsi_score_fs + ecmen_score)/2 + CARI_current_status)/2,
         lsg_food_security = case_when(
           cari_score <= 1.25 ~ 1,
           cari_score >= 1.5 & cari_score <= 2.25 ~ 2,
           cari_score >= 2.5 & cari_score <= 3.25 ~ 3,
           cari_score >= 3.5 ~ 4))
# Remove parameter columns
data.list$main <- data.list$main %>%
  subset(select = -c(food_exp_0,	total_exp_ecmen_0,	ecmen_0,	ecmen_per_cap_0,
                     rcsi_adj,	rcsi_score_adj,	food_exp,	total_exp_ecmen,	ecmen,	ecmen_per_cap,	deflator,
                     lcsi_score_num,hh_size_par, children_par, gender_par, age_par, rural_par, income_par, 
                     disability_par, prod_var, obl_UA32, obl_UA26, obl_UA48, obl_UA46, obl_UA59,
                     obl_UA63, obl_UA56, obl_UA68, obl_UA61, obl_UA05, obl_UA73, obl_UA21, 
                     obl_UA12, obl_UA18, obl_UA51, obl_UA35, obl_UA07, obl_UA80, obl_UA74, 
                     obl_UA71, obl_UA23, obl_UA53, obl_UA65, obl_UA14))

#write.xlsx(data.list$main, "main_impute.xlsx")
