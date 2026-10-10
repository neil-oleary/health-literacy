# Derive survey weights

# Based on the following target age, sex and education joint distribution
# https://data.cso.ie/table/FY076

library(csodata)
library(survey)
# Load CSO data from given table
data_cso_raw <- cso_get_data(table_code = "FY076", 
                         pivot_format = "tidy") 

# Remove marginal totals, and levels not covered by survey and format variable names
data_cso <- data_cso_raw %>%
  # Use only 2022 data
  filter(CensusYear %in% "2022") %>%
  # Remove marginal totals
  filter(Highest.Level.of.Education.Completed != "Total education ceased and not ceased") %>%
  filter(Single.Year.of.Age != "All ages") %>%
  filter(Sex != "Both sexes") %>%
  # Format age 
  mutate(age = gsub(" years", "", Single.Year.of.Age)) %>%
  mutate(age = gsub(" and over", "", age)) %>%
 # Format education 
  mutate(education = case_when(
    Highest.Level.of.Education.Completed %in% c("Economic status - other", "Not stated") ~ "Unknown",
    age %in% as.character(18:25) ~ "Under 25",
    Highest.Level.of.Education.Completed %in% c("No formal education", "Primary") ~ "Primary",
    Highest.Level.of.Education.Completed %in% c("Lower secondary", "Upper secondary") ~ "Secondary",
    Highest.Level.of.Education.Completed %in% c("Technical/vocational", "Advanced certificate/completed apprenticeship",
                                                "Higher certificate") ~ "Vocational/Techincal",
    Highest.Level.of.Education.Completed %in% c("Ordinary bachelor degree/professional qualification or both", 
                                                "Honours bachelor degree/professional qualification or both",
                                                "Postgraduate diploma or degree", 
                                                "Doctorate (Ph.D.)") ~ "Tertiary",
    Highest.Level.of.Education.Completed %in% c("Economic status - total at school, university, etc.") ~ "Unknown"
  )) %>%
  # Create age categories
  mutate(age_cat = cut(as.numeric(age), breaks = c(18, 25, 40, 60, 80), include.lowest = TRUE)) %>%
  mutate(sex = droplevels(Sex)) %>%
  # Output selected variables
  select(age, age_cat, sex, education,
         count = `Population Aged 15 Years and Over`) 

# Calculate expected population totals and proportions within each stratum (accounting for unknown values for education)
data_cso_totals <- data_cso %>%
  group_by(age_cat, sex) %>%
  mutate(age_sex_count0 = sum(count)) %>%
  ungroup() %>%
  filter(education != "Unknown") %>%
  group_by(age_cat, sex) %>%
  mutate(age_sex_count1 = sum(count)) %>%
  ungroup() %>%
  group_by(age_cat, sex, education) %>%
  summarise(exp_count_stratum = sum(count) / age_sex_count1[1] * age_sex_count0[1]) %>%
  ungroup() %>%
  mutate(total_count = sum(exp_count_stratum)) %>%
  mutate(prop_pop_stratum = exp_count_stratum / total_count) %>%
  # Remove strata not covered in survey
  # Remove <18 and <80 year old counts (not in survey)
  filter(!is.na(age_cat)) %>%
  # Remove primary education
  filter(!(education %in% "Primary")) %>%
  # Amend conditional proportions
  mutate(prop_pop_stratum = prop_pop_stratum / sum(prop_pop_stratum))
  

# Create necessary variables/strata in sample for calibrating
data_ihf_strata <- data_ihf %>%
  # Format sex
  mutate(sex = demo_sex) %>%
  # Format education 
  mutate(education = case_when(
    demo_age %in% 18:25 ~ "Under 25",
    grepl("Secondary", demo_education) ~ "Secondary",
    grepl("Vocational", demo_education) ~ "Vocational/Techincal",
    grepl("(Undergraduate)|(Postgraduate)", demo_education) ~ "Tertiary")) %>%
  # Create age categories
  mutate(age_cat = cut(as.numeric(demo_age), breaks = c(18, 25, 40, 60, 80), include.lowest = TRUE))
  
# Calculate sample totals and proportions within each stratum
data_ihf_totals <- data_ihf_strata %>%
  group_by(age_cat, sex, education) %>%
  summarise(count_stratum = n()) %>%
  ungroup() %>%
  mutate(total_count = sum(count_stratum)) %>%
  mutate(prop_stratum = count_stratum / total_count)

# Generate sample weights for each stratum
data_weights <- data_ihf_totals %>%
  full_join(data_cso_totals, by = c("age_cat", "sex", "education")) %>%
  mutate(weight = prop_pop_stratum / prop_stratum) #%>%
  #select(age_cat, sex, education, weight)

# Merge weights and create trimmed weights
data_ihf_weights <- data_ihf_strata %>%
  left_join(data_weights) %>%
  mutate(weight_threshold = median(weight) + 10 * as.numeric(diff(quantile(weight,c(0.25,0.75)))),
         weight_trim = if_else(weight > weight_threshold, weight_threshold, weight),
         weight_trim = weight_trim / sum(weight_trim) * n()) %>%
  select(demo_age, demo_sex, demo_education, 
         wt_age_cat = age_cat, wt_sex = sex, 
         wt_education = education,
         weight, weight_trim)

# Labels for data
var_label(data_ihf_weights) <- list(
  wt_age_cat = "Age category stratum for survey weight",
  wt_sex = "Sex stratum for survey weight",
  wt_education = "Education stratum for survey weight",
  weight = "Survey weight",
  weight_trim = "Survey weight - trimmed")


# # Some summaries of survey weights in sample
# # By age
# ggplot(data = data_ihf_weights, aes(y = weight)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1, colour = "red") +
#   facet_grid(~wt_age_cat)
# # By educational attainment
# ggplot(data = data_ihf_weights, aes(y = weight)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1, colour = "red") +
#   facet_grid(~wt_education)
# # By sex
# ggplot(data = data_ihf_weights, aes(y = weight)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1, colour = "red") +
#   facet_grid(~wt_sex)
# 
# 
# 
# # Key points to note:
# # - No respondents with primary education or no formal education in survey
# # - No respondents older than 80 years
# # - As a result, survey results do not cover these aspects of the target population
# # - Weights are then derived to generate more representative results for Irish adults
# #   between 18 and 80 years-old, who have at least some secondary or vocational/technical education
# # - Also, due to inconsistency of the education questions between the census and the survey, 
# #   for individuals under 25 years-old education is not used to stratify individuals
# 
# 
# # Summary of Census 2022 age-sex profiles
# census_age_sex_total <- data_cso %>%
#   group_by(age, sex) %>%
#   summarise(age_count = sum(count))
# ggplot() +
#   geom_col(data = census_age_sex_total, aes(x = age, y = age_count/8000)) +
#   geom_histogram(data = data_ihf_weights, aes(x = demo_age), 
#                  stat="count", fill = "red", alpha = 0.5) +
#   facet_grid(~sex) +
#   facet_grid(~demo_sex)
# 
# # Important plot to show under- and over-sampling in various categories
# ggplot(data_weights) +
#   geom_bar(aes(x = age_cat, y = prop_pop_stratum), stat = 'identity', fill = "red", alpha = 0.3) +
#   geom_bar(aes(x = age_cat, y = prop_stratum), stat = 'identity', fill = "green", alpha = 0.3) +
#   geom_segment(aes(x = age_cat, y = prop_pop_stratum, yend = prop_stratum, colour = (prop_pop_stratum - prop_stratum) < 0),
#                linewidth = 1.2, arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
#   facet_grid(education ~ sex)
# # Needs work to colour code arrow according to colour scheme
# 
