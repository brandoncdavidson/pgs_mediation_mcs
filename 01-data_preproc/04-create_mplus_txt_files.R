#############################
#creating MPlus data subsets#
#############################

#Due to the nature of MPlus, it is the most efficient to subset the data prior to analysis to ONLY include
#the variables used in the model script itself. Thus, this script creates 4 data subsets that correspond
#to the MPlus .inp files that code the relevant models.

rm(list=ls())
library(dplyr)

data_path <- "C:/Users/bd03/OneDrive - University of Cambridge/Genetic Data/Full dataset/"
data_name <- "merge_with_genetic_data_only_genetic"
all_mcs_genetic <- read.csv((paste(data_path, data_name, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

#Model 1: Language Model
model1 <- all_mcs_genetic %>%
  select(BDBAST00, CCNVTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
  replace(is.na(.), -9999)

#Model 2: Non-verbal Model
model2 <- all_mcs_genetic %>%
  select(CCPSTSCORE, CCPCTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
  replace(is.na(.), -9999)

#Supplementary Model 1: Language at 3 years, but not 5 years
suppmodel1 <- all_mcs_genetic %>%
  select(BDBAST00, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
  replace(is.na(.), -9999)

#Supplementary Model 2: Language at 5 years, but not 3 years
suppmodel2 <- all_mcs_genetic %>%
  select(CCNVTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
  replace(is.na(.), -9999)

#Supplementary Model 3: All Significant Pathways
suppmodel3 <- all_mcs_genetic %>%
  select(BDBAST00, CCPSTSCORE, CCPCTSCORE, DCMATHS7SA, DCWRSD00, total_score, weight, PTTYPE2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
  replace(is.na(.), -9999)

#Export these new dataframes into .dat files to be read in MPlus
write.table(model1, paste(data_path, "full_lang_data", ".txt", sep = ""), quote=FALSE, row.names = FALSE, col.names = FALSE)
write.table(model2, paste(data_path, "nonverb_5_years_data", ".txt", sep = ""), quote=FALSE, row.names = FALSE, col.names = FALSE)
write.table(suppmodel1, paste(data_path, "lang_3_years_data", ".txt", sep = ""), quote=FALSE, row.names = FALSE, col.names = FALSE)
write.table(suppmodel2, paste(data_path, "lang_5_years_data", ".txt", sep = ""), quote=FALSE, row.names = FALSE, col.names = FALSE)
write.table(suppmodel3, paste(data_path, "lang3_nonverb5_data", ".txt", sep = ""), quote=FALSE, row.names = FALSE, col.names = FALSE)