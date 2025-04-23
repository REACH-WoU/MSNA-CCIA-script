library(writexl)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, randomcoloR, sf, anytime, DT,
               cluster, survey, srvyr, knitr, webshot, docstring, tcltk, scales, utilityR)

#if(!require('utilityR')){
#  if(!require('devtools')){
#    install.packages('devtools')
#  }
#  devtools::install_github('https://github.com/REACH-WoU/utilityR', build_vignettes = T)
#}
#
#library(utilityR)

# source("src/utils/utils_analysis.R")
# source("src/utils/misc_utils.R")
# source("src/utils/utils_descriptive_analysis.R")
# source("src/utils/kobo_utils.R")
# source("src/utils/check_kobo.R")
# source("src/utils/tabular_analysis_utils.R")

na_max <- function(x){ifelse( !all(is.na(x)), max(x, na.rm=T), NA)}
na_min <- function(x){ifelse( !all(is.na(x)), min(x, na.rm=T), NA)}


options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

##  LOAD DATA -------------------------------------------------------------------

cat("\n> Loading data for analysis from", strings['filename.data'], "...\n")
sheet_names <- excel_sheets(strings['filename.data'])
sheet_names[1] <- paste(sheet_names[1], "(main)")
cat("> Found the following datasheets:", paste(sheet_names, collapse = ", "), "\n")

# the first sheet is always named "main"
sheet_names[1] <- "main"
data.list <- list("main" = read_excel(strings['filename.data'], sheet=1, col_types = "text"))

for(sheet in sheet_names[-1])
  data.list[[sheet]] <- read_excel(strings['filename.data'], sheet=sheet, col_types = "text")









