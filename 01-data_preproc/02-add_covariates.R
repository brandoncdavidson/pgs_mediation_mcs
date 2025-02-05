rm(list=ls())
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

#reading it in like this means that any refusal, non-completion or null values are being read as NA

data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Full dataset/"
data_name <- "initial_merge"
all_mcs <- read.csv((paste(data_path, data_name, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

####################################
#CALCULATING A COMPOSITE GCSE SCORE#
####################################

#Function to convert letter grade to numeric value
letter_grade_to_numeric <- function(grade) {
  grade <- toupper(grade)  # Convert input to uppercase for case insensitivity
  grade_mapping <- c("A*" = 8.5, "A" = 7, "B" = 5.5, "C*" = 5, "C" = 4.5, "D" = 3, "E"= 2.25, "F" = 1.5, "G" = 0.75, "U" = 0, "L1 Pass" = 1, "L1 Merit" = 2, "L1 Distinction" = 3, "L2 Pass" = 4, "L2 Merit" = 5.5, "L2 Distinction" = 7, "9" = 9, "8" = 8, "7" = 7, "6" = 6, "5" = 5, "4" = 4, "3" = 3, "2" = 2, "1" = 1)
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
  if(all(is.na(c(...)))) {
    return(NA)
  } else {
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

#There is a single participant with missing data for child sex at both sweeps. Child sex will be regressed
#out of the PGS - and the participant without their child sex recorded does not provide genetic data.
#Thus, this is not an issue here.

##################
#SES - OECD SCORE#
##################

all_mcs$average_oecd_score <- rowMeans(all_mcs[, c("ADOEDE00", "BDOEDE00", "CDOEDE00", "DDOEDE00", "EOEDE000", "FOEDE000")], na.rm = TRUE)

#################
#SAMPLING WEIGHT#
#################

#the nature of MCS sampling weights is that they are developed each sweep - using the weight from the
#sweep before alongside new information. As such, the most recent weight for any participant is the
#most informative. Given that weights can only be applied once in MPlus, we will apply the most recent
#weight from any participant.

all_mcs$weight <- coalesce(
  all_mcs$GOVWT2,
  all_mcs$FOVWT2,
  all_mcs$EOVWT2,
  all_mcs$DOVWT2,
  all_mcs$COVWT2,
  all_mcs$BOVWT2,
  all_mcs$AOVWT2
)

#########
#cleanup#
#########

all_mcs <- all_mcs %>%
  select(-BHCSEX00, -ADOEDE00, -BDOEDE00, -CDOEDE00, -DDOEDE00, -EOEDE000, -FOEDE000, -starts_with("Subject"), -ends_with("OVWT1"), -ends_with("OVWT2"))

###############################
#write.csv with the covariates#
###############################

write_csv(all_mcs, paste(data_path, "merge_with_covariates", ".csv", sep = ""))
