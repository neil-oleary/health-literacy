library(tidyverse)
library(mvtnorm)
library(polycor)
library(psych)
library(stringr)
library(lavaan)
library(labelled)

load("analysis_dataset.RData")
# OPTIONS:
# 1. PCA with polychoric correlation processing of all item responses in advance
# 2. PCA directly on HLQ domain scores and with polychoric correlation processing of all other items in advance
# 3. PCA directly on HLQ domain scores only

# Convert items to numeric data, and format other variables
data_ihf_scores <- data_ihf %>%
  mutate(across(starts_with("hlq_p1_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("hlq_p2_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("eheals_q"), ~as.numeric(.x))) %>%
  mutate(across(matches("^ihfinfo_q[1-7]"), ~as.numeric(.x))) %>%
  mutate(demo_age_10 = demo_age/10,
         demo_employment_4cat = fct_lump_n(demo_employment, n = 3),
         health_illdisab_any = if_else(health_illdisab_none == "No", "Yes", "No"),
         info_searchfreq_4cat = fct_lump_n(info_searchfreq, n = 3, other_level = "Rarely/Never")) %>%
  # Create dummy variables for categorical levels
  mutate(demo_sex_male = as.integer(demo_sex == "Male"),
         demo_education_voctech = as.integer(demo_education == "Vocational/Technical"),
         demo_education_undergrad  = as.integer(demo_education == "Undergraduate"),
         demo_education_postgrad  = as.integer(demo_education == "Postgraduate"),
         demo_employment_4cat_pt = as.integer(demo_employment_4cat == "Employed part-time"),
         demo_employment_4cat_student = as.integer(demo_employment_4cat == "Full-time student"),
         demo_employment_4cat_other = as.integer(demo_employment_4cat == "Other"),
         health_illdisab_count_1 = as.integer(health_illdisab_count == "1"),
         health_illdisab_count_2plus = as.integer(health_illdisab_count == "2 or more"),
         info_searchfreq_4cat_week = as.integer(info_searchfreq_4cat == "Weekly"),
         info_searchfreq_4cat_mon = as.integer(info_searchfreq_4cat == "Monthly"),
         info_searchfreq_4cat_rn = as.integer(info_searchfreq_4cat == "Rarely/Never")) %>%
  set_variable_labels(demo_age_10 = "Age \n (per 10 years)",
                      demo_sex = "Sex \n (reference: Female)",
                      demo_education = "Highest level of educational attainment \n (reference: Secondary)",
                      demo_employment_4cat = "Employment status \n (reference: Employed full-time)",
                      health_illdisab_any = "Disability or long-standing illness \n (reference: None)",
                      health_illdisab_count = "Number of long-standing health conditions \n (reference: None)",
                      info_searchfreq_4cat = "Internet search frequency \n (reference: Daily)") 
# Select only items for PCA
data_ihf_scores_only <- data_ihf_scores %>%
  select(starts_with("hlq_p1_q"),
         starts_with("hlq_p2_q"),
         starts_with("eheals_q"),
         matches("^ihfinfo_q[1-7]"))

# Polychoric correlation matrix
poly <- polychoric(data_ihf_scores_only, global = FALSE)

############################################################
# SIMPLE POLYCHORIC CORRELATION PLOTTING
############################################################

# Polychoric correlation plots
df_plot_corr <- as.data.frame(poly$rho) %>%
  rownames_to_column("var1") %>%
  pivot_longer(cols = -var1,
               names_to = "var2",
               values_to = "correlation") %>%
  mutate(var1_label = toupper(gsub("_", " ", var1)),
         var1_label = gsub("EHEALS", "eHEALS", var1_label),
         var1_label = gsub("IHFINFO", "iHF Info", var1_label)) %>%
  mutate(var2_label = toupper(gsub("_", " ", var2)),
         var2_label = gsub("EHEALS", "eHEALS", var2_label),
         var2_label = gsub("IHFINFO", "iHF Info", var2_label))
figA1_corr  <- ggplot(data = df_plot_corr, aes(x = var2_label, y = var1_label, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Polychoric Correlation Matrix",
       x = NULL, y = NULL, fill = "Correlation")


############################################################
# PERFORM PCA BASED ON POLYCHORIC CORRELATION
############################################################

# Select number of components based on eigenvalues (7 in this case)
eigen(poly$rho)$values
scree(poly$rho, pc = TRUE)
fa.parallel(poly$rho, n.obs = 448, fa = "pc")

# Run PCA analysis
pca_result <- principal(poly$rho, nfactors = 7, rotate = "varimax", scores = TRUE)
print(pca_result)

# Calculate scores for each observation
scores_df <- as.data.frame(scale(data_ihf_scores_only) %*% pca_result$weights)

# Plot heatmap of component loadings for each variable
df_loadings <- as.data.frame(unclass(pca_result$loadings)) %>%
  rownames_to_column("variable") %>%
  pivot_longer(cols = -variable,
               names_to = "component",
               values_to = "loading") %>%
  mutate(variable_label = toupper(gsub("_", " ", variable)),
         variable_label = gsub("EHEALS", "eHEALS", variable_label),
         variable_label = gsub("IHFINFO", "iHF Info", variable_label))

figA2_pca_loadings <- ggplot(df_loadings, aes(x = component, y = variable_label, fill = loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "PCA Loadings Heatmap", x = "Component", y = "Variable", fill = "Loading")




############################################################
# PERFORM EXPLORATORY STRUCTURAL EQUATION MODELLING (ESEM)
############################################################

# Build item name list for ESEM model specification         
item_names <- names(data_ihf_scores_only)
items_string <- paste(item_names, collapse = " +\n                   ")

# Assemble the factors expression
n_factors = 7
factors_string <- paste(
  paste0('efa("efa")*f', 1:n_factors, collapse = " +\n  ")
)

# Assemble items expression (need to use dummy factor coding)
model <- paste0(
  factors_string,
  " =~ ",
  items_string,
  "\n\n",
  # Demographic paths
  paste0("f", 1:n_factors, " ~ demo_age_10 + demo_sex_male + ", 
         "demo_education_voctech + demo_education_undergrad + demo_education_postgrad + ",
         "demo_employment_4cat_pt + demo_employment_4cat_student + demo_employment_4cat_other + ",
         "health_illdisab_count_1 + health_illdisab_count_2plus + ",
         "info_searchfreq_4cat_week + info_searchfreq_4cat_mon + info_searchfreq_4cat_rn", collapse = "\n")
)

# Check model expression
cat(model)

# Fit model
fit <- sem(model, data = data_ihf_scores, ordered = TRUE)


# Summarise model fit
fitMeasures(fit, c("cfi", "tli", "rmsea", "rmsea.ci.lower",
                   "rmsea.ci.upper", "srmr"))

# "The 7-factor ESEM solution showed acceptable fit to the data 
# (CFI = 0.998, TLI = 0.998, RMSEA = 0.019 [95% CI: 0.015, 0.022], SRMR = 0.043)."


# Extract ESEM parameters
all_params <- parameterEstimates(fit, standardized = TRUE)

# EFA measurement part — loadings
loadings <- all_params %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, loading = std.all, pvalue) %>%
  arrange(factor, desc(abs(loading))) %>%
  mutate(item = toupper(gsub("_", " ", item)),
         item = gsub("EHEALS", "eHEALS", item),
         item = gsub("IHFINFO", "iHF Info", item))

# Structural part — demographic paths
structural <- all_params %>%
  filter(op == "~") %>%
  select(factor = lhs, predictor = rhs, beta = std.all, se, pvalue) %>%
  mutate(parent_var = str_extract(predictor, 
                           paste(c("demo_age_10", "demo_sex", "demo_education", 
                                   "demo_employment_4cat", "health_illdisab_count", 
                                   "info_searchfreq_4cat"), 
                                 collapse = "|")),
         level_label = str_remove(predictor, paste0(parent_var, "_?")),
         level_label = ifelse(level_label == "", parent_var, level_label))

# Factor correlations
factor_cors <- all_params %>%
  filter(op == "~~", lhs != rhs,
         lhs %in% paste0("f", 1:5),
         rhs %in% paste0("f", 1:5)) %>%
  select(factor1 = lhs, factor2 = rhs, correlation = std.all)


# Plot ESEM factor loadings
figA3_esem_loadings <- loadings %>%
  select(-pvalue) %>%
  pivot_wider(names_from = factor, values_from = loading) %>%
  column_to_rownames("item") %>%
  rownames_to_column("item") %>%
  pivot_longer(cols = -item,
               names_to = "factor",
               values_to = "loading") %>%
  ggplot(aes(x = factor, y = item, fill = loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "ESEM Factor Loadings",
       x = "Factor", y = "Item", fill = "Loading")


# Plot ESEM structural parameter estimates
figA4_esem_structural <- structural %>%
  ggplot(aes(x = factor, y = predictor, fill = beta)) +
  geom_tile() +
  geom_text(aes(label = round(beta, 2), colour = if_else(abs(beta) > 0.2, "white", "black")), 
            size = 3, hjust = 1.1, vjust = -0.1) +
  geom_text(aes(label = round(se, 2), colour = if_else(abs(beta) > 0.2, "white", "black")), 
            size = 3, hjust = -0.10, vjust = 1.1) +
  geom_text(aes(label = "/", colour = if_else(abs(beta) > 0.2, "white", "black")), 
            size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-0.3, 0.3),
                       na.value = "grey90") +
  scale_colour_manual(values = c("white" = "white", "black" = "black"),
                      guide = "none") +
  theme_minimal() +
  labs(title = "Structural Paths: Demographics Predicting Factors",
       x = "Factor", y = "Predictor", fill = "Beta")




# Save outputs for inclusion in report
save(figA1_corr, 
     figA2_pca_loadings, 
     figA3_esem_loadings,
     figA4_esem_structural,
     file = "outcome_pca_esem.RData")


