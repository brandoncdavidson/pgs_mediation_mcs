#############################
#creating MPlus data subsets#
#############################

#Due to the nature of MPlus, it is the most efficient to subset the data prior to analysis to ONLY include
#the variables used in the model script itself. Thus, this script creates 4 data subsets that correspond
#to the MPlus .inp files that code the relevant models.

rm(list=ls())
library(dplyr)

data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Full dataset/"
data_name <- "merge_with_genetic_data_only_genetic"
all_mcs_genetic <- read.csv((paste(data_path, data_name, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

#Subset 1: Lang_5_years_model
lang_5_years_data <- all_mcs_genetic %>%
  select(CCNVTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed)

#Subset 2: Nonverb_5_years_model
nonverb_5_years_data <- all_mcs_genetic %>%
  select(CCPSTSCORE, CCPCTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed)

#Subset 3: Lang_and_Nonverb_5_years_model
lang_and_nonverb_5_years_data <- all_mcs_genetic %>%
  select(CCNVTSCORE, CCPSTSCORE, CCPCTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed)

#Subset 4: Full_lang_model
full_lang_data <- all_mcs_genetic %>%
  select(BDBAST00, CCNVTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed)

#Export these new dataframes

write_csv(lang_5_years_data, paste(data_path, "lang_5_years_data", ".csv", sep = ""))
write_csv(nonverb_5_years_data, paste(data_path, "nonverb_5_years_data", ".csv", sep = ""))
write_csv(lang_and_nonverb_5_years_data, paste(data_path, "lang_and_nonverb_5_years_data", ".csv", sep = ""))
write_csv(full_lang_data, paste(data_path, "full_lang_data", ".csv", sep = ""))
