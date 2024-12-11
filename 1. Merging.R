rm(list=ls())

##################
#loading packages#
##################

library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(psych)

data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Full dataset/"

#######################
#LOAD IN THE VARIABLES#
#######################

#Linked genetic and phenotypic data from the Millenium Cohort Study requires approval from the Centre for Longitudinal
#Studies. As such, the datafiles you recieve will not be identical to the ones used in this analysis. Our data is 
#broken up both categorically (as it will be provided to you) and from different request instances (following ammendments
#to our application). In order to merge correctly, you need to identify which files are referring to 'cohort-members',
#and which files refer to entire households. The merging process for both is similar, but 'cohort-member' level files
#require the addition of a 'child_id'. Further, our family identifier is 'FEARON_FID', as FEARON was the primary
#applicant for our access. As such, this will change according to your application.

all_weights_name <- "GDAC_2022_17_FEARON_5_mcs_data_struct_fam_2024-08-28_15-36-42"
oecd_scores_name <- "GDAC_2022_17_FEARON_4_mcs_data_struct_fam_2024-08-05_16-49-32"
child_sex_sweep_1_name <- "GDAC_2022_17_FEARON_mcs_data_struct_HHgrid_personlist_2024-03-14"
child_sex_sweep_2_name <- "GDAC_2022_17_FEARON_5_mcs_hhgrid_structure_pheno_data_2024-08-30_12-22-56"
child_test_scores_name <- "GDAC_2022_17_FEARON_mcs_data_struct_cm_long_2024-03-14"
gcse_scores_name <- "GDAC_2022_17_FEARON_mcs_data_struct_cm_extra_long_mcs7_cm_qualifications_2024-03-14"
stratification_name <- "GDAC_2022_17_FEARON_6_mcs_data_struct_fam_2024-11-01_09-46-08"

all_weights <- read.csv((paste(data_path, all_weights_name, ".csv", sep = ""))) #family-based measure
oecd_scores <- read.csv((paste(data_path, oecd_scores_name, ".csv", sep = ""))) #family-based measure
stratification <- read_spss((paste(data_path, stratification_name, ".sav", sep = ""))) #family-based measure
child_sex_sweep_1 <- read.csv((paste(data_path, child_sex_sweep_1_name, ".csv", sep = ""))) #child-based measure
child_sex_sweep_2 <- read.csv((paste(data_path, child_sex_sweep_2_name, ".csv", sep = ""))) #child-based measure
child_test_scores <- read.csv((paste(data_path, child_test_scores_name, ".csv", sep = ""))) #child-based measure
gcse_scores <- read.csv((paste(data_path, gcse_scores_name, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A")) #child-based measure

################################################
#ADD CHILD_ID AND RESTRICT TO PRIMARY CAREGIVER#
################################################

#This section adds 'child_id' to all cohort-member level measures.

child_test_scores$child_ID <- paste(child_test_scores$FEARON_FID, substr(child_test_scores$PNUM, 1, 1), sep = "_")

gcse_scores$child_ID <- paste(gcse_scores$FEARON_FID, substr(gcse_scores$PNUM, 1, 1), sep = "_")

child_sex_sweep_1 <- subset(child_sex_sweep_1, PNUM %in% c(100, 200)) #restricts to just cohort member
child_sex_sweep_1$child_ID <- paste(child_sex_sweep_1$FEARON_FID, substr(child_sex_sweep_1$PNUM, 1, 1), sep = "_")

child_sex_sweep_2 <- subset(child_sex_sweep_2, PNUM %in% c(100, 200)) #restricts to just cohort member
child_sex_sweep_2$child_ID <- paste(child_sex_sweep_2$FEARON_FID, substr(child_sex_sweep_2$PNUM, 1, 1), sep = "_")

############################################
#CHECK FOR DUPLICATES IN THE RELEVANT FILES#
############################################

#This section ensures there are no duplicate rows in any of the files. This process is relatively complex for GCSE
#scores, given how the data is coded.

if (anyDuplicated(child_test_scores$child_ID) > 0) {
  child_test_scores <- distinct(child_test_scores, child_ID, .keep_all = TRUE)
}

if (anyDuplicated(child_sex_sweep_1$child_ID) > 0) {
  child_sex_sweep_1 <- distinct(child_sex_sweep_1, child_ID, .keep_all = TRUE)
}

if (anyDuplicated(child_sex_sweep_2$child_ID) > 0) {
  child_sex_sweep_2 <- distinct(child_sex_sweep_2, child_ID, .keep_all = TRUE)
}

#This process here is checking if any 'child_ID' appears more than 20 times (as this should be the maximum number of 
#instances of any cohort member)
gcse_scores$child_ID
childid_frequency <- table(gcse_scores$child_ID)
child_over_20 <- names(childid_frequency[childid_frequency > 20])
print(childid_over_20)

gcse_scores <- gcse_scores %>%
  group_by(child_ID) %>%
  filter(row_number() <= 20) %>%
  ungroup()

##############################
#MCS7 GCSE DATA RESTRUCTURING#
##############################

#Remove all rows that do not have data in the relevant columns.

mcs7qualifications_filtered <- gcse_scores %>%
  filter(!is.na(GC_L_GCSB_NAME_R40) | !is.na(GC_L_IGSB_NAME_R30) | !is.na(GC_L_BTEC_NAME_R30))

#Remove all columns that do not include useful data. This includes non-England based age-16 qualifications, as these are
#challenging to interpret.

mcs7qualifications_gradeonly <- mcs7qualifications_filtered %>%
  dplyr::select(child_ID, GC_L_GCSB_NAME_R40, GC_L_GCGD, GC_L_IGSB_NAME_R30, GC_L_IGGD, GC_L_BTEC_NAME_R30, GC_L_BTLV, GC_L_BTGD)

#Grade conversion functions:

numeric_to_letter_grade_gcse <- function(score) {
  grade_map <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "U", "A*", "A", "B", "C", "D", "E", "F", "G", "N/A")
  return(grade_map[score])
}

numeric_to_letter_grade_igcse <- function(score) {
  grade_map <- c("9", "8", "7", "6", "5", "4", "3", "2", "1", "A*", "A", "B", "C*", "C", "D", "E", "F", "N/A")
  return(grade_map[score])
}

numeric_to_letter_level_btec <- function(score) {
  grade_map <- c("Entry", "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8")
  return(grade_map[score])
}

numeric_to_letter_grade_btec <- function(score) {
  grade_map <- c("Pass", "Merit", "Distinction")
  return(grade_map[score])
}

# Define a new function for subject code conversion
global_counter <- 1
last_child_ID <- NULL

reset_global_counter <- function(child_ID) {
  if (!is.null(last_child_ID) && last_child_ID != child_ID) {
    global_counter <<- 1
  }
  last_child_ID <<- child_ID
}

code_to_subject <- function(Grade, child_ID) {
  reset_global_counter(child_ID)
  
  # Create a unique name for each subject with a global counter
  new_name <- paste("Subject", global_counter)
  
  # Increment the global counter
  global_counter <<- global_counter + 1
  
  return(new_name)
}

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(GC_L_GCSB_NAME_R40 = case_when(
    TRUE ~ NA_character_
  ))

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(GC_L_IGSB_NAME_R30 = case_when(
    TRUE ~ NA_character_
  ))


# Apply the functions to add Grade

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(GCSEGrade = sapply(GC_L_GCGD, numeric_to_letter_grade_gcse))

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(IGCSEGrade = sapply(GC_L_IGGD, numeric_to_letter_grade_igcse))

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(BTECLevel = sapply(GC_L_BTLV, numeric_to_letter_level_btec))

mcs7qualifications_gradeonly <- mcs7qualifications_gradeonly %>%
  mutate(BTECGrade = sapply(GC_L_BTGD, numeric_to_letter_grade_btec))

# Separate GCSE and IGCSE data

gcse_data <- mcs7qualifications_gradeonly %>%
  dplyr::select(child_ID, GC_L_GCSB_NAME_R40, GCSEGrade)

igcse_data <- mcs7qualifications_gradeonly %>%
  dplyr::select(child_ID, GC_L_IGSB_NAME_R30, IGCSEGrade)

btec_data <- mcs7qualifications_gradeonly %>%
  dplyr::select(child_ID, BTECLevel, BTECGrade)

#Rename columns for consistency

gcse_data <- gcse_data %>%
  rename(Subject = GC_L_GCSB_NAME_R40, Grade = GCSEGrade)

igcse_data <- igcse_data %>%
  rename(Subject = GC_L_IGSB_NAME_R30, Grade = IGCSEGrade)

btec_data <- btec_data %>%
  filter(BTECLevel %in% c("L1", "L2")) %>%
  mutate(CombinedBTEC = paste(BTECLevel, ifelse(!is.na(BTECGrade), BTECGrade, "")))

btec_data <- btec_data %>%
  filter(!is.na(BTECGrade))

btec_data <- btec_data %>%
  rename(Grade = CombinedBTEC)

btec_data <- btec_data %>%
  dplyr::select(child_ID, Grade)

# Filter out NAs

gcse_data <- gcse_data %>%
  filter(!is.na(Grade))

igcse_data <- igcse_data %>%
  filter(!is.na(Grade))

# Combine GCSE, IGCSE, and BTEC data

combined_data <- bind_rows(gcse_data, igcse_data, btec_data)

# Only showing edited rows

combined_data <- combined_data %>%
  dplyr::select(child_ID, Subject, Grade)

# Apply the 'code_to_subject' function

combined_data <- combined_data %>%
  group_by(child_ID, Subject) %>%
  mutate(Subject = mapply(code_to_subject, Subject, child_ID))#

table(combined_data$Subject)

#pivot the data

mcs7_qualifications_cm_grades <- combined_data %>%
  pivot_wider(
    names_from = Subject,
    values_from = Grade,
    values_fn = list(Grade = function(x) paste(na.omit(x), collapse = ",")),
  )

# Select relevant columns

mcs7_qualifications_cm_grades <- mcs7_qualifications_cm_grades %>%
  dplyr::select(child_ID, 'Subject 1', 'Subject 2', 'Subject 3', 'Subject 4', 'Subject 5', 'Subject 6', 'Subject 7', 'Subject 8', 'Subject 9', 'Subject 10', 'Subject 11', 'Subject 12', 'Subject 13', 'Subject 14', 'Subject 15', 'Subject 16')

mcs7_qualifications_cm_grades <- mcs7_qualifications_cm_grades %>%
  ungroup() %>%
  mutate(Total_Grades = rowSums(!is.na(dplyr::select(., -child_ID))))

rm(gcse_data, igcse_data, btec_data, mcs7qualifications_filtered, mcs7qualifications_gradeonly, combined_data, global_counter, last_child_ID, child_over_20)

################
#MERGE THE DATA#
################

#merge the two family-based variables first. then merge the 4 child-based variables. then perform a left-join for the
#family variables onto the child (ensuring that, for twins, both twins will have the relevant data for their household).
#note - any renaming that occurs here is simply due to columns like 'FEARON_FID' appearing in every dataframe, and this
#ensures we are saving the one with all the relevant information.

#we encountered an issue that some ppts had withdrew in-between different data requests we had made. if you are
#identifying some participants lacking data in recent iterations but have data earlier on - this may be the cause. if so,
#check with the CLS and act accordingly. in our instance, we removed those participants when identified - as they no
#longer wanted their data to be used.

merge_family_1 <- merge(oecd_scores, all_weights, by = "FEARON_FID", all.x = TRUE, all.y = TRUE)
merge_family <- merge(merge_family_1, stratification, by = "FEARON_FID", all.x = TRUE, all.y = TRUE)

child_merge_part1 <- merge(child_sex_sweep_1, child_sex_sweep_2, by = "child_ID", all.x = TRUE, all.y = TRUE)
child_merge_part2 <- merge(child_merge_part1, child_test_scores, by = "child_ID", all.x = TRUE, all.y = TRUE)
child_merge_final <- merge(child_merge_part2, mcs7_qualifications_cm_grades, by = "child_ID", all.x = TRUE, all.y = TRUE)

child_merge_final <- child_merge_final %>%
  select(-FEARON_FID, -GENDAC_QUERY_SAMPLE)

child_merge_final <- child_merge_final %>%
  rename(FEARON_FID = FEARON_FID.y)

final_merge <- left_join(child_merge_final, merge_family, by = "FEARON_FID")

rm(all_weights, child_merge_final, child_sex_sweep_1, child_sex_sweep_2, child_test_scores, gcse_scores, mcs7_qualifications_cm_grades, merge_family, merge_family_1, child_merge_part1, child_merge_part2, oecd_scores, stratification)

###########################
#DROPPING UNNEEDED COLUMNS#
###########################

#this section here selects only the relevant columns for your analysis and drops the remaining. this
#may not be necessary if you only requested the essential variables for one project.

cols_to_keep <- c("FEARON_FID",
                  "child_ID",
                  "ADOEDE00",
                  "AHCSEX00",
                  "BHCSEX00",
                  "BDBASA00",
                  "BDBASR00",
                  "BDBAST00",
                  "BDOEDE00",
                  "CCCSCO00",
                  "CCNSCO00",
                  "CCNVABIL",
                  "CCNVTSCORE",
                  "CCPCABIL",
                  "CCPCTSCORE",
                  "CCPSABIL",
                  "CCPSCO00",
                  "CCPSTSCORE",
                  "CDOEDE00",
                  "DCWRSD00",
                  "DCPCTS00",
                  "DCMATHS7SA",
                  "DDOEDE00",
                  "EOEDE000",
                  "FOEDE000",
                  "Subject 1", 
                  "Subject 2",
                  "Subject 3",
                  "Subject 4",
                  "Subject 5",
                  "Subject 6", 
                  "Subject 7", 
                  "Subject 8",
                  "Subject 9",
                  "Subject 10", 
                  "Subject 11", 
                  "Subject 12",
                  "Subject 13", 
                  "Subject 14",
                  "Subject 15",
                  "Subject 16",
                  "Total_Grades",
                  "WEIGHTGB",
                  "AOVWT1",
                  "AOVWT2",
                  "BOVWT1",
                  "BOVWT2",
                  "BOVWTGB",
                  "COVWT1",
                  "COVWT2",
                  "COVWTGB",
                  "DOVWT1",
                  "DOVWT2",
                  "DOVWTGB",
                  "EOVWT1",
                  "EOVWT2",
                  "FOVWT1",
                  "FOVWT2",
                  "GOVWT1",
                  "GOVWT2",
                  "PTTYPE2"
)

#identify any errors that may have occurred that have caused missing columns
missing_columns <- setdiff(cols_to_keep, colnames(final_merge))

# Print the missing columns
if (length(missing_columns) > 0) {
  print(paste("The following columns are missing from the data frame:", 
              paste(missing_columns, collapse = ", ")))
} else {
  print("All columns are present in the data frame.")
}

# Remove all other columns
all_data <- final_merge[, cols_to_keep, drop = FALSE]

##################
#save_merged_data#
##################

write_csv(all_data, paste(data_path, "initial_merge", ".csv", sep = ""))
