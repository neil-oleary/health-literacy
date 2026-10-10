# health-literacy

Data analysis and summary of results of health literacy survey in Ireland from iHealth Facts

## Files

**data_process.R:** to read and process raw exported data into analysis ready format, including derivation of instrument and domain scores from questionnaires

**survey_weights.R:** to derive survey weights for sample, calibrated to Irish Census 2022 data on age, sex and educational attainment joint distributions, as per study SAP

**md_summary.R:** to examine missing data frequency and patterns in survey dataset, as per study SAP

**summary_demo_health.R** to produce descriptive statistical summary tables and figures of demographic and selected health variables considered prognostic for study outcomes, weighted and unweighted, as per study SAP

**summary_outcomes.R** to produce descriptive statistical summary tables and figures of study outcomes, weighted and unweighted, as per study SAP

**domain_modelling.R** to fit and summarise statistical models of study outcomes, with survey weighting, as per study SAP

**pca_analysis.R** to perform exploratory PCA and SEM on all items contributing to study outcomes