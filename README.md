# MSNA CCIA (contextualized complex indicator analysis) script

This script calculates LSGs (living standards gaps) in seven sectors: Livelihoods, Protection, Shelter/NFI, WASH, Health, Food Security and Education. Then the CCIA score is calculated as the maximum across all the sectors. The scale of the score is 1, 2, 3, 4 (and 4+ for some sectors and the CCIA itself) corresponding to the None/Minimal, Stress, Severe, Extreme (and Extreme+) level of needs. All the calculations are done on the households level.

## Calculation of LSGs and their dimensions

Every sectoral LSG is calculated from several dimensions. The framework for the MSNA 2024 can be found in the resources/ folder. Every every dimesion is based on certain indicators of the MSNA dataset with certain response options leading to certain levels on need.
For example "Income quantity" dimension of the Livelihoods LSG gives level 1 (None/Minimal) if income per capita is > 9707 UAH, level 2 (Stress) if income per capita is <= 9707 UAH and > 6471 UAH, level 3 (Severe) if income per capita is <= 6471 UAH and >2324 UAH, and level 4 (Extreme) if income per capita is <= 2324 UAH.
This calculations are done in the corresponding R scripts in the indices/ folder.
After all the dimensions are calculated, LSGs are calculated using ```make_lsg``` function from src_R/max_lsg.R. If any dimension is NA (because of a deleted entry for certain indicator), then the LSG is evaluated based on other dimensions. If maximum among not NA dimensions is the highest possible LSG score (4 or 4+), the LSG receives this score, otherwise LSG is also NA.

### Food security LSG
