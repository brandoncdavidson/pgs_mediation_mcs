###################################################
#descriptive statistics and sample characteristics#
###################################################

#########################################################
#load in relevant packages and create correct dataframes#
#########################################################

rm(list=ls())
setwd("C:/Users/brand/OneDrive - University of Cambridge/Genetic Data/Merged Scripts")
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(rempsyc)
library(gtsummary)
library(Hmisc)
library(quanteda)

all_mcs <- read.csv("new_data_3.csv", na.strings = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "NA", "N/A"))

all_mcs_genetic <- all_mcs [!(is.na(all_mcs$Child_PRS_regressed) & 
                                is.na(all_mcs$Father_PRS_regressed) & 
                                is.na(all_mcs$Mother_PRS_regressed)), ]

child_id_with_genetic <- as.list(all_mcs_genetic$child_ID)

all_mcs_nongenetic <- all_mcs %>%
  subset(!child_ID %in% child_id_with_genetic)

##############################################################################################
#demographic characteristics of relevant variable - comparison between genetic and nongenetic#
##############################################################################################

table(all_mcs_genetic$AHCSEX00)
table(all_mcs_nongenetic$AHCSEX00)

#calculate chi-squared

mean(all_mcs_genetic$average_oecd_score)
sd(all_mcs_genetic$average_oecd_score)

mean(all_mcs_nongenetic$average_oecd_score, na.rm = TRUE)
sd(all_mcs_nongenetic$average_oecd_score, na.rm = TRUE)

#t-test

###################################################
#genetic sample: n, mean, sd, skewness coefficient#
###################################################

variables_of_interest <- c("BDBAST00", "CCNVTSCORE", "CCPSTSCORE", "CCPCTSCORE", "DCMATHS7SA", "DCWRSD00", "total_score")

# Filter data for selected variables
data_filtered <- all_mcs_genetic[, variables_of_interest]

# Rename columns
colnames(data_filtered) <- c("Age 3 BAS-II: Naming Vocabulary",
                             "Age 5 BAS-II: Naming Vocabulary",                             
                             "Age 5 BAS-II: Picture Similarities", 
                             "Age 5 BAS-II: Pattern Construction",
                             "Age 7 NFER",
                             "Age 7 Word Reading",
                             "GCSE Total")

# Calculate mean and standard deviation for each variable
summary_statistics <- data_filtered %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})", digits = list(all_continuous() ~ c(2, 2)), include = 1:7)

# Add frequency (n) column
summary_statistics <- summary_statistics %>%
  add_n()

# Modify header labels
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

if(!require(e1071)) install.packages("e1071")
library(e1071)

skewness(all_mcs_genetic$BDBAST00, na.rm = TRUE)
skewness(all_mcs_genetic$CCNVTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$CCPSTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$CCPCTSCORE, na.rm = TRUE)
skewness(all_mcs_genetic$DCMATHS7SA, na.rm = TRUE)
skewness(all_mcs_genetic$DCWRSD00, na.rm = TRUE)
skewness(all_mcs_genetic$total_score, na.rm = TRUE)
