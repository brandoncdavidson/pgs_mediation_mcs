rm(list=ls())
setwd("C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Merged Scripts")
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(rempsyc)
library(ggplot2)
library(gtsummary)

#reading it in like this means that any refusal, non-completion or null values are being read as NA

all_mcs <- read.csv("new_data_1.csv", na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A")) %>%
  select(-X) %>%
  rename(FEARON_FID = FEARON_FID.x) %>%
  rename(GENDAC_QUERY_SAMPLE = GENDAC_QUERY_SAMPLE.x)

###################################################################
#CALCULATING A COMPOSITE GCSE SCORE - SEVERAL CANDIDATE MECHANISMS#
###################################################################

# Function to convert letter grade to numeric value
letter_grade_to_numeric <- function(grade) {
  grade <- toupper(grade)  # Convert input to uppercase for case insensitivity
  
  # Define mapping of letter grades to numeric values
  grade_mapping <- c("A*" = 8.5, "A" = 7, "B" = 5.5, "C*" = 5, "C" = 4.5, "D" = 3, "E"= 2.25, "F" = 1.5, "G" = 0.75, "U" = 0, "L1 Pass" = 1, "L1 Merit" = 2, "L1 Distinction" = 3, "L2 Pass" = 4, "L2 Merit" = 5.5, "L2 Distinction" = 7, "9" = 9, "8" = 8, "7" = 7, "6" = 6, "5" = 5, "4" = 4, "3" = 3, "2" = 2, "1" = 1)
  
  # Return the numeric value for the given letter grade
  return(ifelse(grade %in% names(grade_mapping), grade_mapping[grade], NA))
}

all_mcs <- all_mcs %>%
  mutate(across(c("Subject.1", 
                  "Subject.2",
                  "Subject.3",
                  "Subject.4",
                  "Subject.5",
                  "Subject.6", 
                  "Subject.7", 
                  "Subject.8",
                  "Subject.9",
                  "Subject.10", 
                  "Subject.11", 
                  "Subject.12",
                  "Subject.13", 
                  "Subject.14",
                  "Subject.15",
                  "Subject.16"), letter_grade_to_numeric))

#### TOTAL SCORE ####

calculate_total_score <- function(...) {
  # Check if all subject scores are NA
  if(all(is.na(c(...)))) {
    return(NA)
  } else {
    # Sum up all the scores (excluding 'child_ID')
    total_score <- sum(c(...), na.rm = TRUE)
    return(total_score)
  }
}

all_mcs <- all_mcs %>%
  rowwise() %>%
  mutate(total_score = calculate_total_score(c_across(starts_with("Subject")))) %>%
  ungroup()

###########
#CHILD SEX#
###########

all_mcs$AHCSEX00 <- ifelse(
  is.na(all_mcs$AHCSEX00), 
  all_mcs$BHCSEX00, 
  all_mcs$AHCSEX00
)

distribution <- table(all_mcs$AHCSEX00)
print(distribution)

missing_participant <- all_mcs[is.na(all_mcs$AHCSEX00), ]

#there is a single participant who has NA in the AHCSEX00 variable. infuriating.i checked back to the original data pre-merge
#and it seems that they are a participant who joined at sweep 2, and did not seem to fill out any of the demographic-type
#information in that sweep for the child (or at least did not fill out child sex). we could in theory request CHCSEX00 and
#hope the information is there - but it seems relatively minor and unimportant.

##################
#SES - OECD SCORE#
##################

all_mcs$average_oecd_score <- rowMeans(all_mcs[, c("ADOEDE00", "BDOEDE00", "CDOEDE00", "DDOEDE00", "EOEDE000", "FOEDE000")], na.rm = TRUE)

#########
#cleanup#
#########

all_mcs <- all_mcs %>%
  select(-BHCSEX00, -ADOEDE00, -BDOEDE00, -CDOEDE00, -DDOEDE00, -EOEDE000, -FOEDE000, -starts_with("Subject"))

###############################
#write.csv with the covariates#
###############################

write.csv(all_mcs, file = "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Merged Scripts/new_data_2.csv")
