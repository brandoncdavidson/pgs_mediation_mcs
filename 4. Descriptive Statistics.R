###################################################
#descriptive statistics and sample characteristics#
###################################################

#########################################################
#load in relevant packages and create correct dataframes#
#########################################################

rm(list=ls())
library(dplyr)
library(tidyverse)
library(gtsummary)
library(Hmisc)
library(e1071)

data_path <- "C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Full dataset/"
data_name_full_sample <- "merge_with_genetic_data"
data_name_genetic_subset <- "merge_with_genetic_data_only_genetic"

all_mcs <- read.csv((paste(data_path, data_name_full_sample, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))
all_mcs_genetic <- read.csv((paste(data_path, data_name_genetic_subset, ".csv", sep = "")), na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

child_id_with_genetic <- as.list(all_mcs_genetic$child_ID)

all_mcs_nongenetic <- all_mcs %>%
  subset(!child_ID %in% child_id_with_genetic)

##############################################################################################
#demographic characteristics of relevant variable - comparison between genetic and nongenetic#
##############################################################################################

#CHILD SEX:

childsex_genetic_table <- table(all_mcs_genetic$AHCSEX00)
childsex_non_genetic_table <- table(all_mcs_nongenetic$AHCSEX00)

#calculate chi-squared
contingency_table <- rbind(childsex_genetic_table, childsex_non_genetic_table)
chi_sq_result <- chisq.test(contingency_table)
print(chi_sq_result)

#OECD SCORE:

mean(all_mcs_genetic$average_oecd_score)
sd(all_mcs_genetic$average_oecd_score)

mean(all_mcs_nongenetic$average_oecd_score, na.rm = TRUE)
sd(all_mcs_nongenetic$average_oecd_score, na.rm = TRUE)

#calculate t-test
# Perform Welch's t-test
t_test_result <- t.test(all_mcs_genetic$average_oecd_score, 
                        all_mcs_nongenetic$average_oecd_score, 
                        alternative = "two.sided",
                        var.equal = FALSE,
                        na.rm = TRUE)

print(t_test_result)

###################################################
#genetic sample: n, mean, sd, skewness coefficient#
###################################################

variables_of_interest <- c("BDBAST00", "CCNVTSCORE", "CCPSTSCORE", "CCPCTSCORE", "DCMATHS7SA", "DCWRSD00", "total_score")
data_filtered <- all_mcs_genetic[, variables_of_interest]
colnames(data_filtered) <- c("Age 3 BAS-II: Naming Vocabulary",
                             "Age 5 BAS-II: Naming Vocabulary",                             
                             "Age 5 BAS-II: Picture Similarities", 
                             "Age 5 BAS-II: Pattern Construction",
                             "Age 7 NFER",
                             "Age 7 Word Reading",
                             "GCSE Total")

summary_statistics <- data_filtered %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})", digits = list(all_continuous() ~ c(2, 2)), include = 1:7)

summary_statistics <- summary_statistics %>%
  add_n()

summary_statistics <- summary_statistics %>%
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**Mean (SD)**"
  )

summary_statistics

####################
#CORRELATION MATRIX#
####################

res2 <- rcorr(as.matrix(data_filtered))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(data_filtered[1:7]))
flattenCorrMatrix(res2$r, res2$P)

######################
#skewness coefficient#
######################

skewness(all_mcs_genetic$BDBAST00, na.rm = TRUE)
skewness(all_mcs_genetic$CCNVTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$CCPSTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$CCPCTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$DCMATHS7SA, na.rm = TRUE)
skewness(all_mcs_genetic$DCWRSD00, na.rm = TRUE)
skewness(all_mcs_genetic$total_score, na.rm = TRUE)
