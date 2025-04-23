# MSNA CCIA (contextualized complex indicator analysis) script

This script calculates LSGs (living standards gaps) in seven sectors: Livelihoods, Protection, Shelter/NFI, WASH, Health, Food Security and Education. Then the CCIA score is calculated as the maximum across all the sectors. The scale of the score is 1, 2, 3, 4 (and 4+ for some sectors and the CCIA itself) corresponding to the None/Minimal, Stress, Severe, Extreme (and Extreme+) level of needs. All the calculations are done on the households level.

**NOTE:** dimensions of LSGs are changing every year, this is the version for the MSNA 2024. For your round you should edit all the files in the **indices/** folder.

## Input dataset

The dataset must include all the columns specified in custom dimensions calculations such as _proximity to the frontline_, _income per capita_, _FCS_, _LCS_, etc. It is placed in the **data/** folder.

## Calculation of LSGs and their dimensions

Every sectoral LSG is calculated from several dimensions. The framework for the MSNA 2024 can be found in the resources/ folder. Every every dimesion is based on certain indicators of the MSNA dataset with certain response options leading to certain levels on need.

For example "Income quantity" dimension of the Livelihoods LSG gives level 1 (None/Minimal) if income per capita is > 9707 UAH, level 2 (Stress) if income per capita is <= 9707 UAH and > 6471 UAH, level 3 (Severe) if income per capita is <= 6471 UAH and >2324 UAH, and level 4 (Extreme) if income per capita is <= 2324 UAH.
This calculations are done in the corresponding R scripts in the **indices/** folder.

After all the dimensions are calculated, LSGs are calculated using ```make_lsg``` function from **src_R/max_lsg.R**. If any dimension is NA (because of a deleted entry for certain indicator), then the LSG is evaluated based on other dimensions. If maximum among not NA dimensions is the highest possible LSG score (4 or 4+), the LSG receives this score, otherwise LSG is also NA.

### Food security LSG

Food security LSG is calculated differently. First, NAs in numerical variables (expenditures and reduced coping strategies) are imputed using a generalized linear model. (This part is a subject for improvement such as model cross-validation, reduction of numbers of parameters, etc.) The ECMEN (economic capacity to meet essential needs) is calculated from the expenditures module per capita and assigned to levels 1, 3, 4 based on the thresholds in UAH. FCS (food consumption score) and LCSi (livelihoods coping strategy index) must be already calculated in the dataset and converted to levels 1-4. rCSI (reduced coping strategies index) is calculated with adjusted methodology on scale 0-77.

From FCS and rCSI it calculates ```CARI_current_status``` and then ```cari_score = ((lcsi_score_fs + ecmen_score)/2 + CARI_current_status)/2```

Food security LSG is rounded ```cari_score```

The detailed methodology can be found in **resources/Methodological note Food security.docx**

## CCIA calculation and cooccurrence

**src_R/format_dataset_ccia.R** calls all the LSG calculating scripts and then calculates the CCIA calling **indices/lsg_msni.R** script.

Then it calculates all possible cooccurrences of sectoral needs as following columns:
- ```Merged_3_4_5``` and ```Merged_4_5``` - list of all sectors where the household has Severe+ or Extreme+ needs respectively
- ```LSG_incidence_general``` and ```LSG_incidence_general_extreme``` - states whether the households has Severe+ or Extreme+ need in any of the sectors
- ```LSG_incidence_detail``` and ```LSG_incidence_detail_extreme``` - the number of sectors the household is in Severe+ or Extreme+ need
- ```<sector>_overlaps``` - states whether there is no Severe+ need in this sector, Severe+ need only in this sector, or Severe+ need in this and other sectors
- ```cooccurence_<sector1>_<sector2>``` and ```extreme_cooccurence_<sector1>_<sector2>``` - states if the household is in Severe+ or Extreme+ need in both of the two sectors
- ```cooccurence_v2_<sector1>_<sector2>``` and ```extreme_cooccurence_v2_<sector1>_<sector2>``` - among the households in Severe+ or Extreme+ need in sector1 states whether the sector1 is cooccurring with the sector2 or only other sectors

## Output

After running main.R the dataset with added dimensions, LSG, CCIA and cooccurrence columns will appear in the **data/** folder. This dataset then can be processed with the analysis_boilerplate_v4 script to produce frequency tables. The basic DAFs to build CCIA and cooccurence tables can be found in **resources/** folder.
