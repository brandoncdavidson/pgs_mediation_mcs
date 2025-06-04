# Davidson et al., 2025: "Early Developmental Markers of Genetic Influences on Academic Attainment"

A repository for the research study *"Early Developmental Markers of Genetic Influences on Academic Attainment"* by Davidson, Austerberry & Fearon (2025).

This repository is broken down into three sub-sections:

*1. Data Pre-Processing*

This contains the 4 scripts to clean MCS files, merge data and standardise/regress polygenic scores such that they can be taken forward for analysis. In the start of script 1, input your relevant raw files (which will be named individually for your research team when supplied by the Centre for Longitudinal Studies). Following this, the code will produce specifically named scripts that are then read throughout the remainder of the pre-processing.

*2. Descriptive Statistics*

This contains the 1 script to calculate preliminary N values, means, standard deviations and allows for comparison of genetic and non-genetic samples. This reproduces Table 1, Table 2 and the values for Variance Inflation Factors of the child and parent EA-PGS.

*3. MPlus Model Scripts*

This contains the .inp files that can be read into MPlus to reproduce the structural models. This includes non-mediation models (used to identify model fit statistics and standardised path values) and mediation models (used to calculate confidence intervals and indirect effects). These .inp files are named corresponding to the relevant Figure/Table of which they produce findings for.
