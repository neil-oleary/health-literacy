# Demographic and health status summaries of participants
# As per 5.1 of study SAP

# Summary statistics will be generated for demographic and participant information variables in the following tables along with graphical summaries for the following variable types:
#   •	Descriptive Table 1:
#   o	Categorical variables (e.g., sex, education) → Frequencies & percentages
#   o	Continuous variables (e.g., age) → Mean, SD, median, IQR 
# •	Data visualisation:
#   o	Bar charts for categorical data
# o	Histograms for continuous data
# All descriptive analysis will be presented as unweighted and weighted and include the count and proportions of missing data.


# The following tables will be prepared: 
#   Table 1: Unweighted and weighted* demographic summaries

library(gtsummary)

# Combine weights with demographic data
data_demo <- data_ihf %>%
  select(starts_with("demo"), weight)

# Combine weights with health data
data_health <- data_ihf %>%
  select(starts_with("health"), weight)

# Combine weights with health information data
data_info <- data_ihf %>%
  select(starts_with("info"), weight)

# Creat survey objects - for weighted tabulation
svy_demo_ihf <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_demo
  )
svy_health_ihf <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_health
  )
svy_info_ihf <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_info
  )
# Generate table with demographics
tbl_demo_unweighted <- tbl_summary(data_demo,
                              type = all_continuous() ~ "continuous2",
                              statistic = all_continuous() ~ c(
                                "{mean} ({sd})",
                                "{median} [{p25}, {p75}]"))
tbl_demo_weighted <- tbl_svysummary(svy_demo_ihf,
                               type = all_continuous() ~ "continuous2",
                               statistic = list(all_continuous() ~ c(
                                 "{mean} ({sd})",
                                 "{median} [{p25}, {p75}]"),
                                 all_categorical() ~ "{p}%"),
                               include = -weight)

tbl1_demo <- tbl_merge(
  tbls = list(tbl_demo_unweighted, tbl_demo_weighted),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>%
  modify_header(label = "**Variables**")


# Plot of distribution of demographic variables - age by sex
age_dist_unweighted <- ggplot(data_demo, aes(group = demo_sex, fill = demo_sex, x = demo_age)) +
  geom_histogram(bins = 20) +
  scale_y_continuous(limits = c(0, 40)) +
  facet_wrap(~demo_sex) +
  ggtitle("Age distribution, by sex (unweighted)") +
  theme_minimal()
age_dist_weighted <- ggplot(data_demo, aes(group = demo_sex, fill = demo_sex, x = demo_age, weight = weight)) +
  geom_histogram(bins = 20) +
  scale_y_continuous(limits = c(0, 40)) +
  facet_wrap(~demo_sex) +
  ggtitle("Age distribution, by sex (weighted)") +
  theme_minimal()
fig1_age_dist <- (age_dist_unweighted / age_dist_weighted )

# Generate table with health status
tbl_health_unweighted <- tbl_summary(data_health,
                                   type = all_continuous() ~ "continuous2",
                                   statistic = all_continuous() ~ c(
                                     "{mean} ({sd})",
                                     "{median} [{p25}, {p75}]"),
                                   include = -weight)
tbl_health_weighted <- tbl_svysummary(svy_health_ihf,
                                    type = all_continuous() ~ "continuous2",
                                    statistic = list(all_continuous() ~ c(
                                      "{mean} ({sd})",
                                      "{median} [{p25}, {p75}]"),
                                      all_categorical() ~ "{p}%"),
                                    include = -weight)

tbl2_health <- tbl_merge(
  tbls = list(tbl_health_unweighted, tbl_health_weighted),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>%
  modify_header(label = "**Variables**")

# Generate table with health status
tbl_info_unweighted <- tbl_summary(data_info,
                                     type = all_continuous() ~ "continuous2",
                                     statistic = all_continuous() ~ c(
                                       "{mean} ({sd})",
                                       "{median} [{p25}, {p75}]"),
                                   include = -weight)
tbl_info_weighted <- tbl_svysummary(svy_info_ihf,
                                      type = all_continuous() ~ "continuous2",
                                      statistic = list(all_continuous() ~ c(
                                        "{mean} ({sd})",
                                        "{median} [{p25}, {p75}]"),
                                        all_categorical() ~ "{p}%"),
                                      include = -weight)

tbl3_info <- tbl_merge(
  tbls = list(tbl_info_unweighted, tbl_info_weighted),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>%
  modify_header(label = "**Variables**")

save(tbl1_demo, tbl2_health, tbl3_info, fig1_age_dist, file = "demo_health_summaries.RData")

