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

data_path <- "C:.../OneDrive - University of Cambridge/Genetic Data/Full Dataset"

#######################
#LOAD IN THE VARIABLES#
#######################

#As the linked genetic and phenotypic data first requires approval from the Centre for Longitudinal studies, the
#structure of this data and the files they arrive in will be determined by the variables you request. The purpose
#of this script is to select all the relevant variables from the data we were given, begin cleaning some of the
#more complex data (such as the gcse scores) and ensure that all children within a household are provided the relevant
#family-level information. as such, when merging your own files, check whether or not the script is referring to
#family-id or 'pnum', and adapt accordingly. also note that any reference to 'FEARON_FID' will be different for your
#phenotyped-linked data. our files refer to FEARON, as he was the primary applicant in our data requests.

all_weights_name <- "GDAC_2022_17_FEARON_5_mcs_data_struct_fam_2024-08-28_15-36-42.csv"
oecd_scores_name <- "GDAC_2022_17_FEARON_4_mcs_data_struct_fam_2024-08-05_16-49-32.csv"
child_sex_sweep_1_name <- "GDAC_2022_17_FEARON_mcs_data_struct_HHgrid_personlist_2024-03-14.csv"
child_sex_sweep_2_name <- "GDAC_2022_17_FEARON_5_mcs_hhgrid_structure_pheno_data_2024-08-30_12-22-56.csv"
child_test_scores_name <- "GDAC_2022_17_FEARON_mcs_data_struct_cm_long_2024-03-14.csv"
gcse_scores_name <- "GDAC_2022_17_FEARON_mcs_data_struct_cm_extra_long_mcs7_cm_qualifications_2024-03-14.csv"
stratification_name <- "GDAC_2022_17_FEARON_6_mcs_data_struct_fam_2024-11-01_09-46-08.sav"

all_weights <- read.csv((paste(data_path, all_weights_name, ".csv", sep = ""))) #family-based measure
oecd_scores <- read.csv((paste(data_path, oecd_scores_name, ".csv", sep = ""))) #family-based measure
stratification <- read_spss((paste(data_path, stratification_name, ".csv", sep = ""))) #family-based measure
child_sex_sweep_1 <- read.csv((paste(data_path, child_sex_sweep_1_name, ".csv", sep = ""))) #child-based measure
child_sex_sweep_2 <- read.csv((paste(data_path, child_sex_sweep_2_name, ".csv", sep = ""))) #child-based measure
child_test_scores <- read.csv((paste(data_path, child_test_scores_name, ".csv", sep = ""))) #child-based measure
gcse_scores <- read.csv((paste(data_path, gcse_scores_name, ".csv", sep = "", na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A")))) #child-based measure

################################################
#ADD CHILD_ID AND RESTRICT TO PRIMARY CAREGIVER#
################################################

#this section of the script is written in a confusing way - but it is acting as a fail-safe for some issues we identified
#later in the merge process. here, we are adding a 'child_id' to all files that refer to cohort members rather than family
#wide information. this will assist in the merging process later. we also add a 'completed_data_count' column, which is
#also important in the merging process.

#this section handles all child-based measures, with the exception of gcse scores - which are very complicated in how
#they are coded and, as such, have the section beneath this dedicated to their analysis.

child_test_scores$child_ID <- paste(child_test_scores$FEARON_FID, substr(child_test_scores$PNUM, 1, 1), sep = "_")

#child sex is listed under PNUM=100/200, meaning it does not rely on definition from primary caregiver, and means we can
#just subset to PNUM=100/200 to extract the child sex value at sweep 1 and 2.

child_sex_sweep_1 <- subset(child_sex_sweep_1, PNUM %in% c(100, 200))
child_sex_sweep_1$child_ID <- paste(child_sex_sweep_1$FEARON_FID, substr(child_sex_sweep_1$PNUM, 1, 1), sep = "_")
#again there are duplicates present. remove these for successful merging
child_sex_sweep_1 <- distinct(child_sex_sweep_1, child_ID, .keep_all = TRUE)

child_sex_sweep_2 <- subset(child_sex_sweep_2, PNUM %in% c(100, 200))
child_sex_sweep_2$child_ID <- paste(child_sex_sweep_2$FEARON_FID, substr(child_sex_sweep_2$PNUM, 1, 1), sep = "_")
#also duplicates here. the data is identical in all rows, so it is okay to remove the excess.
child_sex_sweep_2 <- distinct(child_sex_sweep_2, child_ID, .keep_all = TRUE)


#note - there appear to be duplicate rows in this dataframe, which need removing for a successful left-join later on.
child_test_scores <- distinct(child_test_scores, child_ID, .keep_all = TRUE)

#########################
#MCS7 GCSE DATA CLEANING#
#########################

gcse_scores$child_ID <- paste(gcse_scores$FEARON_FID, substr(gcse_scores$PNUM, 1, 1), sep = "_")

#the findings for 72 ppts have been duplicated (depicting 40 rather than 20 rows, and thus duplicating their gcse results)
#the code below identifies these children and removes their duplicated set of results.

gcse_scores$child_ID
fid_frequency <- table(gcse_scores$child_ID)
print(fid_frequency)
fid_over_20 <- names(fid_frequency[fid_frequency > 20])
print(fid_over_20)

gcse_scores <- gcse_scores %>%
  group_by(child_ID) %>%
  filter(row_number() <= 20) %>%
  ungroup()

mcs7qualifications_filtered <- gcse_scores %>%
  filter(!is.na(GC_L_GCSB_NAME_R40) | !is.na(GC_L_IGSB_NAME_R30))

##############################
#MCS7 GCSE DATA RESTRUCTURING#
##############################

#removing all columns except for GCSEs for ease, includes total number of qualifications -gcse, btec, igcse- completed

mcs7qualifications_gradeonly <- mcs7qualifications_filtered %>%
  dplyr::select(child_ID, GC_L_GCSB_NAME_R40, GC_L_GCGD, GC_L_IGSB_NAME_R30, GC_L_IGGD, GC_L_BTEC_NAME_R30, GC_L_BTLV, GC_L_BTGD)

#grade conversion function

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

# Define a new function for subject code conversion
reset_global_counter <- function(child_ID) {
  # Reset the global counter if child_ID has changed
  if (!is.null(last_child_ID) && last_child_ID != child_ID) {
    global_counter <<- 1
  }
  last_child_ID <<- child_ID
}

code_to_subject <- function(Grade, child_ID) {
  reset_global_counter(child_ID)  # Reset the counter
  
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


# this applies the two functions - so that the grades and the subject names are now displayed
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

# Rename columns for consistency

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

rm(gcse_data, igcse_data, btec_data, mcs7qualifications_filtered, mcs7qualifications_gradeonly, mcs7_cm_qualifications_structure_pheno_data, combined_data, fid_frequency, fid_over_20, global_counter, last_child_ID)

################
#MERGE THE DATA#
################

#merge the two family-based variables first. then merge the 4 child-based variables. then perform a left-join for the
#family variables onto the child (ensuring that, for twins, both twins will have the relevant data for their household)

merge_family_1 <- merge(oecd_scores, all_weights, by = "FEARON_FID", all.x = TRUE, all.y = TRUE)
merge_family <- merge(merge_family_1, stratification, by = "FEARON_FID", all.x = TRUE, all.y = TRUE)

#lets work backwards, prior attempts revealed weird ppt counts when merging. this was an issue with duplicated rows. the 
#below method is unconventional but helped me identify the problem. essentially, we take the family-based
#variables merged above, and create every potential child_ID that could be generated (i.e., accounting
#for the potential of twins). then we perform left-joins onto this dataframe and check how much data was retained (with the sanity
#checks created earlier).

#we also encountered an issue that some ppts had withdrew in-between differeent data requests we had made. if you are
#identifying some participants lacking data in recent iterations but have data earlier on - this may be the cause. if so,
#check with the CLS and act accordingly. in our instance, we removed those participants when identified - as they no
#longer wanted their data to be used.

merge_family_duplicated <- merge_family[rep(1:nrow(merge_family), each = 2), ]
merge_family_duplicated$child_ID <- paste0(merge_family_duplicated$FEARON_FID, "_", rep(1:2, times = nrow(merge_family)))

merge_full_a <- left_join(merge_family_duplicated, mcs7_qualifications_cm_grades, by = "child_ID")
num_with_total_grades <- nrow(merge_full_a[!is.na(merge_full_a$Total_Grades), ])

merge_full_b <- left_join(merge_full_a, child_test_scores, by = "child_ID")
num_with_row_data_1 <- nrow(merge_full_b[!is.na(merge_full_b$completed_data_count_1), ])

merge_full_c <- left_join(merge_full_b, child_sex_sweep_1, by = "child_ID")
num_with_row_data_2 <- nrow(merge_full_c[!is.na(merge_full_c$completed_data_count_2), ])
#ok this is perfect, there are no missing participants here. everyone with data has been added. YAY.

merge_full_d <- left_join(merge_full_c, child_sex_sweep_2, by = "child_ID")
num_with_row_data_3 <- nrow(merge_full_d[!is.na(merge_full_d$completed_data_count_3), ])

#for the most part, this was a total success. now we just need to drop the rows whereby there is no additional data (meaning
#they were the rows where unused child_IDs are present). this will be done with another row counting procedure. the logic here
#is that the child_IDs with no additional data will not have any data in the 4 failsafes we added (i.e., completed_columns_1)

merge_full_d$completed_data_count_finalcheck <- rowSums(!is.na(merge_full_d[, c("completed_data_count_1", "completed_data_count_2", "completed_data_count_3", "Total_Grades")]))

final_merge <- merge_full_d[merge_full_d$completed_data_count_finalcheck > 0, ]

rm(all_weights, child_sex_sweep_1, child_sex_sweep_2, child_test_scores, gcse_scores, mcs7_qualifications_cm_grades, merge_family, merge_family_duplicated, merge_full_a, merge_full_b, merge_full_c, merge_full_d, oecd_scores, num_with_row_data_2, num_with_row_data_3, num_with_total_grades, num_with_total_scores, non_overlapping_ppts, merge_family_1, stratification)

###########################
#DROPPING UNNEEDED COLUMNS#
###########################

cols_to_keep <- c("FEARON_FID.x",
                  "GENDAC_QUERY_SAMPLE.x",
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

###########
#write_csv#
###########

write.csv(all_data, file = "C:.../OneDrive - University of Cambridge/Genetic Data/Merged Scripts/new_data_1.csv")

