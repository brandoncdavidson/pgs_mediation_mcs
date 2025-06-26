# "Verbal and Non-verbal Cognition are Early Developmental Markers of Genetic Influences on Academic Achievement"

A repository for the research study *"Verbal and Non-verbal Cognition are Early Developmental Markers of Genetic Influences on Academic Achievement"*.

This repository is broken down into three sub-sections:

*1. Data Pre-Processing*

This contains the 4 scripts to clean MCS files, merge data and standardise/regress polygenic scores such that they can be taken forward for analysis. In the start of script 1, input your relevant raw files (which will be named individually for your research team when supplied by the Centre for Longitudinal Studies). Following this, the code will produce specifically named scripts that are then read throughout the remainder of the pre-processing.

*2. Descriptive Statistics*

This contains the 1 script to calculate preliminary N values, means, standard deviations and allows for comparison of genetic and non-genetic samples. This reproduces Table 1, Table 2 and the values for Variance Inflation Factors of the child and parent EA-PGS.

*3. MPlus Model Scripts*

This contains the .inp files that can be read into MPlus to reproduce the structural models. For each analysis, there is 3 scripts: (1) measurement models (used to identify model fit statistics and standardised path values), mediation models for academic achievement at age 7 (used to calculate confidence intervals and indirect effects) and mediation models for academic achievement at age 17 (used to calculate confidence intervals and indirect effects). We have included the .out files from the MPlus analysis, which provides the input script used to produce the findings, and all of the raw output information used to produce the Figures and Tables in the main text. 
