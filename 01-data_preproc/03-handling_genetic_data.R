##################
#loading packages#
##################

rm(list=ls())
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(psych)

mcs_data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Full dataset/"
mcs_data_name <- "merge_with_covariates"
all_mcs <- read.csv((paste(mcs_data_path, mcs_data_name, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

#########################
#LOADING IN GENETIC DATA#
#########################

genetic_data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/"
genetic_data_name <- "prsice_pgi_MCS_EA4_p1e5_fearonid_20231206"
genetic_data_demographic_name <- "GDAC_2022_17_FEARON_mcs_basic_demographics_v0003_shareable_20220215"
genetic_data_principle_components_name <- "MCS_topmed_EUR_KING_QCd_PCA_bigsnpr_fearonid_20231206"

genetics1 <- read.csv((paste(genetic_data_path, genetic_data_name, ".csv", sep = "")))
mcs_demographic <- read_spss((paste(genetic_data_path, genetic_data_demographic_name, ".sav", sep = "")))
first10principlecomponents <- read.csv((paste(genetic_data_path, genetic_data_principle_components_name, ".csv", sep = "")))

#########################################
#SPLITTING INTO MOTHER, FATHER AND CHILD#
#########################################

mcs_demographic$member_ID <- paste(mcs_demographic$fearon_fid, mcs_demographic$pnum, sep = "_")
mcs_demographic <- mcs_demographic[!duplicated(mcs_demographic$member_ID), ]
genetics1$member_ID <- paste(genetics1$FEARON_FID, genetics1$PNUM, sep = "_")
first10principlecomponents$member_ID <- paste(first10principlecomponents$FEARON_FID, first10principlecomponents$PNUM, sep = "_")

pgs <- merge(genetics1, first10principlecomponents, by = 'member_ID', all.x = TRUE, all.y = TRUE)
pgs_pcs <- merge(pgs, mcs_demographic, by = "member_ID", all.x = TRUE, all.y = FALSE) %>%
  select(member_ID, fearon_fid, sex, mfc, pnum, PRS, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15, PC16, PC17, PC18, PC19, PC20) %>%
  rename(FEARON_FID = fearon_fid)

###############################
#EXTRACT AVERAGE_OECD_VARIABLE#
###############################

average_oecd <- subset(all_mcs, select = c(average_oecd_score, FEARON_FID))
average_oecd_fearonfid <- average_oecd %>% 
  distinct(FEARON_FID, .keep_all = TRUE)

#leftjoin, whereby any row with a given FEARON_FID is given the average_oecd for that household

pgs_pcs <- pgs_pcs %>%
  left_join(average_oecd_fearonfid, by = "FEARON_FID")

######################################################
#STANDARDISE PGS AND REGRESS OUT PRINCIPLE COMPONENTS#
######################################################

pgs_pcs$PRS_standardised <- as.vector(scale(pgs_pcs$PRS))
pgs_pcs_complete <- pgs_pcs %>% filter(!is.na(average_oecd_score))
pgs_pcs_regressed <- lm(PRS_standardised ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + average_oecd_score, data = pgs_pcs_complete)
pgs_pcs_complete$PRS_regressed <- residuals(pgs_pcs_regressed)
pgs_pcs <- pgs_pcs %>%
  left_join(pgs_pcs_complete %>% select(member_ID, PRS_regressed), by = "member_ID")

columns_to_remove <- paste0("PC", 1:20)
pgs_pcs <- pgs_pcs %>% 
  select(-one_of(columns_to_remove)) %>%
  select(-average_oecd_score)

#####################################
#SPLIT INTO MOTHER, CHILD AND FATHER#
#####################################

pgsmother <- subset(pgs_pcs, mfc == "M") %>%
  subset(!is.na(PRS)) %>%
  rename(Mother_PRS = PRS) %>%
  rename(Mother_PRS_standardised = PRS_standardised) %>%
  rename(Mother_PRS_regressed = PRS_regressed)
pgsfather <- subset(pgs_pcs, mfc == "F") %>%
  subset(!is.na(PRS)) %>%
  rename(Father_PRS = PRS) %>%
  rename(Father_PRS_standardised = PRS_standardised) %>%
  rename(Father_PRS_regressed = PRS_regressed)
pgschild <- subset(pgs_pcs, mfc == "C") %>%
  subset(!is.na(PRS)) %>%
  rename(Child_PRS = PRS) %>%
  rename(Child_PRS_standardised = PRS_standardised) %>%
  rename(Child_PRS_regressed = PRS_regressed) %>%
  mutate(child_ID = paste(FEARON_FID, substr(pnum, 1, 1), sep = "_"))

################
#MERGING PART 1#
################

merge1 <- merge(all_mcs, pgschild, by = "child_ID", all.x = TRUE, all.y = FALSE) %>%
  rename(FEARON_FID = FEARON_FID.x)
#In our case, there are a few participants who have genetic data due to the early request - but not
#phenotypic data - as this was requested months later (following a wave of ppt consent redaction).
#As such, we have used all.y = FALSE.

merge1 <- merge1 %>%
  mutate(FEARON_FID = if_else(is.na(FEARON_FID), FEARON_FID.y, FEARON_FID))

##############
#REMOVE TWINS#
##############

mcsids_with_two_children <- merge1 %>%
  group_by(FEARON_FID) %>%
  filter(n() > 1) %>%
  dplyr::select(FEARON_FID) %>%
  distinct()
print(mcsids_with_two_children)

merge1$twinhousehold <- FALSE
households_with_twins <- mcsids_with_two_children$FEARON_FID

merge1$twinhousehold[merge1$MCSID %in% households_with_twins] <- TRUE

#This script is contigent on a random set seed to remove different twins each time. The number I used
#is 4563.

set.seed(4563)
merge1 <- merge1 %>%
  group_by(FEARON_FID) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Remove the 'twinhousehold' column as it's not needed anymore
merge1 <- merge1 %>%
  dplyr::select(-twinhousehold)

#####################################################################################
#MERGE PART 2 - BY WAITING UNTIL AFTER TWIN DELETION, CAN MERGE BY FEARON_FID FREELY#
#####################################################################################

merge2 <- merge(merge1, pgsmother, by = "FEARON_FID", all.x = TRUE, all.y = FALSE)
all_mcs_genetic <- merge(merge2, pgsfather, by = "FEARON_FID", all.x = TRUE, all.y = FALSE)

all_mcs_genetic <- all_mcs_genetic %>%
  select(-member_ID.x, -member_ID.y, -member_ID, -FEARON_FID.y, -mfc, -mfc.x, -mfc.y, -sex, -sex.y, -sex.x, -pnum, -pnum.x, -pnum.y)

###################################################
#CHECK NUMBER OF PRS SCORES IN DATASET FOR CLARITY#
###################################################

# Keep rows where at least one of the specified columns is not NA and not empty

all_mcs_genetic_2 <- all_mcs_genetic[!(is.na(all_mcs_genetic$Child_PRS_regressed) & 
                                         is.na(all_mcs_genetic$Father_PRS_regressed) & 
                                         is.na(all_mcs_genetic$Mother_PRS_regressed)), ]

#########################
#counts of each instance#
#########################

count <- select(all_mcs_genetic_2, Child_PRS_regressed, Mother_PRS_regressed, Father_PRS_regressed) %>%
    rename(Child = Child_PRS_regressed) %>%
    rename(Mother = Mother_PRS_regressed) %>%
    rename(Father = Father_PRS_regressed) %>%
  mutate(
    Child = ifelse(!is.na(Child), "child", NA),
    Mother = ifelse(!is.na(Mother), "mother", NA),
    Father = ifelse(!is.na(Father), "father", NA),
    instance = paste(Child, Mother, Father, sep = " ")
  )

instance_freq <- table(count$instance)
print(instance_freq)
instance_prop <- prop.table(instance_freq)
print(instance_prop)

####################################
#MERGE GENETIC DATA WITH PHENOTYPIC#
####################################

write_csv(all_mcs_genetic, paste(mcs_data_path, "merge_with_genetic_data", ".csv", sep = ""))
write_csv(all_mcs_genetic_2, paste(mcs_data_path, "merge_with_genetic_data_only_genetic", ".csv", sep = ""))
