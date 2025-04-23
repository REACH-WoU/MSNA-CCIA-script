library(expss)
cat("\nCalculating LSGs for each Sector using the make_lsg function ...\n")

main <- data.list$main




# Livelihoods (all) +


# main$lsg_livelihoods <- make_lsg(main,
#                                     crit_to_4 = c("income_source",
#                                                   "income_quantity",
#                                                   "coping"
#                                     )
# )
# 
# 
# main$lsg_livelihoods_v2 <- make_lsg(main,
#                                  crit_to_4 = c("income_source",
#                                                "coping"
#                                                ),
#                                  crit_to_3 = c(
#                                    "income_quantity_v2"
#                                  )
#                                  )
# 
# main$lsg_livelihoods_v3 <- make_lsg(main,
#                                     crit_to_4 = c("income_source",
#                                                   "income_quantity_v3",
#                                                   "coping"
#                                     )
# )



main$lsg_livelihoods <- make_lsg(main,
                                    crit_to_4 = c("income_source",
                                                  "income_quantity",
                                                  "coping"
                                    )
)



# WASH +

# 
# main$lsg_wash <- make_lsg(main,
#                           crit_to_4plus = c(
#                             "wash_crit_1",
#                             "wash_crit_2",
#                             "wash_crit_3"
#                           ),
#                           crit_to_4 = c(
#                             "wash_crit_4",
#                             "wash_crit_5",
#                             "wash_crit_6",
#                             "wash_crit_7"
#                           )
# )
# 




# WASH 2 +


main$lsg_wash <- make_lsg(main,
                             crit_to_4plus = c(
                               "wash_crit_1",
                               "wash_crit_2",
                               "wash_crit_3"
                             ),
                             crit_to_4 = c(
                               "wash_crit_4",
                               "wash_crit_7"
                             ),
                             crit_to_3 = c('wash_crit_5')
)


# Education


# main$lsg_education <- make_lsg(main,
#                                crit_to_4 = c(
#                                  "educ_crit_1",
#                                  "educ_crit_2"
#                                  ),
#                                crit_to_3 = c(
#                                  "educ_crit_3"
#                                )
# )
# 

# Education 2


# main$lsg_education_v2 <- make_lsg(main,
#                                crit_to_4 = c(
#                                  "educ_crit_1"),
#                                crit_to_3 = c(
#                                  "educ_crit_3"
#                                )
# )
# 
# Education 3


# main$lsg_education_v3 <- make_lsg(main,
#                                   crit_to_4 = c(
#                                     "educ_crit_1",
#                                     "educ_crit_2"),
#                                   crit_to_3 = c(
#                                     "educ_crit_3"
#                                   )
# )


# Education 4


main$lsg_education <- make_lsg(main,
                                  crit_to_4 = c(
                                    "educ_crit_1",
                                    "educ_crit_3"),
                                  crit_to_3 = c(
                                    "educ_crit_2"
                                  )
)



# Shelter/NFI +

# main$shelter_issues <- make_lsg(main,
#                                 crit_to_4plus = c("shelter_issues_2"),
#                                 crit_to_3 = c("shelter_issues_1")
#                                 )


main$lsg_shelter_nfi <- make_lsg(main,
                                 crit_to_4plus = c(
                                   "shelter_type",
                                   "shelter_issues"
                                 ),
                                 crit_to_4 = c(
                                   "leccy",
                                   "domestic"
                                 ),
                                 crit_to_3 = c(
                                   "security_tenure",
                                   "nfis"
                                 ),
                                 crit_to_2 = 'utility'
                                 )




# Protection


main$lsg_protection <- make_lsg(main,
                                crit_to_4plus = c(
                                  "safety"
                                  ),
                                crit_to_4 = c(
                                  "needs",
                                  "child_sep"
                                  ),
                                crit_to_3 = c(
                                  "barriers",
                                  "legal_assist"
                                  )
                        )





# Health

# main$lsg_health <- make_lsg(main,
#                             crit_to_4 = c(
#                               "health_crit_1",
#                               "health_crit_3",
#                               "health_crit_5"
#                               ),
#                             crit_to_3 = c(
#                               "health_crit_2",
#                               "health_crit_4",
#                               "health_crit_6"
#                             )
#                             )


# Health 2

# main$lsg_health_v2 <- make_lsg(main,
#                             crit_to_4 = c(
#                               "health_crit_1",
#                               "health_crit_3",
#                               "health_crit_5"
#                             ),
#                             crit_to_3 = c(
#                               "health_crit_2",
#                               "health_crit_4"
#                             )
# )


# Health 4

# main$lsg_health_v4 <- make_lsg(main,
#                                crit_to_4plus = c(
#                                  "health_crit_1"
#                                ),
#                                crit_to_4 = c(
#                                  "health_crit_3",
#                                  "health_crit_5"
#                                ),
#                                crit_to_3 = c(
#                                  "health_crit_2",
#                                  #"health_crit_4",
#                                  "health_crit_6"
#                                )
#                       )

# Health 6

main$lsg_health <- make_lsg(main,
                               crit_to_4plus = c(
                                 "health_crit_1"
                               ),
                               crit_to_4 = c(
                                 "health_crit_3",
                                 "health_crit_5"
                               ),
                               crit_to_3 = c(
                                 "health_crit_2",
                                 "health_crit_4")
)








### Food Security
#food_security <- readxl::read_excel("resources/MSNA_data_2024.xlsx", col_types = "text") %>%
#  select(uuid, `CARI classification (using adj rCSI ECMEN FS)`) %>%
#  mutate(
#    x = `CARI classification (using adj rCSI ECMEN FS)`,
#    lsg_food_security = case_when(
#      x == "Food secure" ~ 1,
#      x == "Marginally food secure" ~ 2,
#      x == "Moderately food insecure" ~ 3,
#      x == "Severely food insecure" ~ 4,
#      TRUE ~ NA
#    )
#  ) %>%
#  select(uuid, lsg_food_security)
#
#
#
#main <- main %>%
#  left_join(food_security, by = "uuid")
#
#
#


col_ccia_v4 <- c(
  "lsg_livelihoods",
  "lsg_wash",
  "lsg_education",
  "lsg_shelter_nfi",
  "lsg_protection",
  "lsg_health",
  "lsg_food_security"
)

# col_ccia_base <- c(
#   "lsg_livelihoods",
#   "lsg_wash",
#   "lsg_education",
#   "lsg_shelter_nfi",
#   "lsg_protection",
#   "lsg_health",
#   "lsg_food_security"
# )




main$ccia <- do.call(pmax, c(main[ , col_ccia_v4], na.rm = TRUE))


data.list$main <- main
cat("Finish Calculating LSGs and CINM\n")
