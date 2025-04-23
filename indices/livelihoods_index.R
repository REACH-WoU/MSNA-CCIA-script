j27_social <- c(
  "J_27_income_sources/pension",
  "J_27_income_sources/other_government_social_benefits_or_assistance"
)
j27_assistance <- c(
  "J_27_income_sources/idp_benefits",
  "J_27_income_sources/humanitarian_aid",
  "J_27_income_sources/loans_or_support_from_family_and_friends_within_ukraine",
  "J_27_income_sources/loans_support_or_charitable_donations_from_community_members"
)

j27_irregular <- c(
  "J_27_income_sources/casual_or_daily_labour",
  "J_27_income_sources/other"
)

j27_regular <- c(
  "J_27_income_sources/salaried_work",
  "J_27_income_sources/income_from_own_business_or_regular_trade",
  "J_27_income_sources/income_from_rent",
  "J_27_income_sources/income_from_own_production",
  "J_27_income_sources/money_transfers_from_abroad_from_family_and_friends"
)

j27_undefined <- c(
  "J_27_income_sources/dont_know",
  "J_27_income_sources/prefer_not_to_answer"
)

median_inc <- 5865
new_magic_number = 6471.4
new_magic_number_3 = 9707.1
small_magic_number = 2324

livelihoods <- data.list$main %>%
  mutate(
    soc = rowSums(across(j27_social, as.numeric),na.rm = TRUE),
    ass = rowSums(across(j27_assistance, as.numeric),na.rm = TRUE),
    irr = rowSums(across(j27_irregular, as.numeric),na.rm = TRUE),
    reg = rowSums(across(j27_regular, as.numeric),na.rm = TRUE),
    non = as.numeric(data.list$main$`J_27_income_sources/none`),
    undef = rowSums(across(j27_undefined, as.numeric),na.rm = TRUE),
    income_source = case_when(
      (
       (ass > 0 | non > 0) & (soc + reg + irr + undef == 0)
      ) ~ 4,
      (
        (irr > 0 & (soc + reg + ass + undef + non == 0)) |
        ((soc + ass) >= 2 & (reg + irr + undef + non == 0)) |
        ((ass + irr) >= 2 & (reg + soc + undef + non == 0))
      ) ~ 3,
      (
        (soc > 0 & (reg + ass + irr + undef + non == 0)) |
        (soc + reg + ass + irr >= 4 & (undef + non == 0)) |
        (soc + ass + irr >= 3 & (undef + non + reg == 0)) |
        (reg + ass + irr >= 3 & (undef + non + soc == 0)) |
        (soc + reg + irr >= 3 & (undef + non + ass == 0)) |
        (soc + reg + ass >= 3 & (undef + non + irr == 0)) |
        (irr + reg >= 2 & (ass + soc + undef + non == 0)) |
        (soc + irr >= 2 & (ass + reg + undef + non == 0)) |
        (ass + reg >= 2 & (irr + undef + soc + non == 0)) |
        (soc + reg >= 2 & (irr + undef + ass + non == 0))
      ) ~ 2,
      (
        reg > 0 & (ass + soc + undef + non + irr == 0)
      ) ~ 1,
      undef > 0 | is.na(non) ~ NA,
      TRUE ~ -1
    ),
    income_quantity = case_when(
      as.numeric(total_inc_per_capita) <= 2324 ~ 4,
      as.numeric(total_inc_per_capita) > 2324 & as.numeric(total_inc_per_capita) <= 6471.4  ~ 3,
      as.numeric(total_inc_per_capita) > 6471.4 & as.numeric(total_inc_per_capita) <= 9707.1  ~ 2,
      as.numeric(total_inc_per_capita) > 9707.1 ~ 1,
      TRUE ~ NA_real_
    ),
    coping = case_when(
      lcsi_score == "Emergency" ~ 4,
      lcsi_score == "Crisis" ~ 3,
      lcsi_score == "Stress" ~ 2,
      lcsi_score == "No coping" ~ 1,
      is.na(lcsi_score) ~ NA,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(uuid, income_source, income_quantity, coping)


data.list$main <- data.list$main %>%
  left_join(livelihoods, by = "uuid")
