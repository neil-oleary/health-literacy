library(tidyverse)
library(haven)
library(labelled)
library(gtsummary)
library(gt)
library(survey)
library(VGAM)
library(gtsummary)
library(scales)
library(splines)
library(stringr)
library(ggthemes)
library(flextable)
library(RColorBrewer)
load("analysis_dataset.RData")
# Outcome variable descriptive summaries
# As per 5.2.3 and 5.2.5 of study SAP

# Domain scales or instrument scores will act as the continuous response for the specific domain or questionnaire instrument. 
# Multivariable linear regression models will be fitted to each score, including demographic variables as independent variables.
# The uncertainty of the estimates will be quantified from 95% confidence intervals. 
# All statistical models for inference will include survey weights on participant data.


# The following tables will be reported: 
#   Table 5: Statistical modelling results


# Add additional variables for regression models (and label)
data_ihf_reg <- data_ihf %>%
  mutate(demo_age_10 = demo_age/10,
         demo_employment_4cat = fct_lump_n(demo_employment, n = 3),
         health_illdisab_any = if_else(health_illdisab_none == "No", "Yes", "No"),
         info_searchfreq_4cat = fct_lump_n(info_searchfreq, n = 3, other_level = "Rarely/Never")) %>%
  set_variable_labels(demo_age_10 = "Age \n (per 10 years)",
                      demo_sex = "Sex \n (reference: Female)",
                      demo_education = "Highest level of educational attainment \n (reference: Secondary)",
                      demo_employment_4cat = "Employment status \n (reference: Employed full-time)",
                      health_illdisab_any = "Disability or long-standing illness \n (reference: None)",
                      health_illdisab_count = "Number of long-standing health conditions \n (reference: None)",
                      info_searchfreq_4cat = "Internet search frequency \n (reference: Daily)") 

# Setup survey design for modelling with survey weights (using trimmed version)
svy_ihf_reg <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight_trim,
    data = data_ihf_reg
  )

## PART 1: AGE-SEX-EDUCATION MODELLING OF HLQ ##

# Modelling results just for age, sex, education adjustment
dep_vars <- grep("^hlq[[:digit:]]_dom", names(data_ihf), value = TRUE)
dep_labels <- unlist(var_label(data_ihf)[match(dep_vars, names(data_ihf))])
dep_labels_break <- gsub(": ", "\\\\n", dep_labels)
dep_labels_compact <- gsub(":.*", "", dep_labels)
model_res <- dep_vars %>% 
  ## set names so .id = argument works downstream
  setNames(dep_vars) %>% 
  map(\(dv) {
    f <- reformulate(termlabels = c("demo_age_10", "demo_sex", "demo_education"), 
                     response = dv)
    svyglm(formula = f, design = svy_ihf_reg,
           family = gaussian(link = "identity"),
           na.action = na.exclude)})
# Generate summary tables for each model
table_res <- model_res %>% map(tbl_regression) %>% 
  map(modify_column_hide, columns = p.value)

# Merge tables vertically
tbl5_hlq_agesexedu_model <- tbl_stack(tbls = table_res, group_header = dep_labels)


# Create data frame for parameter estimates in model
coef_df <- imap(model_res, 
                ~{term_labels <- attr(terms(.x), "term.labels")
                  pattern <- stringr::str_c(term_labels, collapse = "|")
                  broom::tidy(.x, conf.int = TRUE) %>%
                    mutate(outcome   = .y, 
                           pred  = stringr::str_extract(term, pattern),
                           level  = stringr::str_remove(term, pattern))}) %>%
  list_rbind() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       demo_age_10 = "Age (per 10 years)",
                       demo_sexMale = "Sex - male \n (vs female)",
                       `demo_educationVocational/Technical` = "Education \n Vocational/Techincal \n vs Secondary",
                       demo_educationUndergraduate = "Education \n Undergraduate \n vs Secondary",
                       demo_educationPostgraduate = "Education \n Postgraduate \n vs Secondary"),
         term = fct_rev(term)) %>%
  rowwise() %>%
  mutate(outcome_label = unlist(var_label(data_ihf)[match(outcome, names(data_ihf))]), 
         pred_label = unlist(var_label(data_ihf_reg)[match(pred, names(data_ihf_reg))]),
         level = if_else(pred == "demo_age_10", "Per 10 years", level)) 

# Forest plot of parameter estimates
fig6_hlq_agesex_model_forest <- ggplot(coef_df %>% filter(grepl("(Age)|(Sex)", term)), aes(x = estimate, y = outcome_label)) +
  # Zero vertical line
  geom_vline(xintercept = 0, linetype = 2) +
  # Point estimates and confidence intervals
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2) +
  facet_wrap(~ term, scales = "free_x") +
  # Make free x-axes symmetric about 0
  geom_blank(aes(x = -conf.low)) +
  geom_blank(aes(x = -conf.high)) +
  # Wrap text to be constrained width on y-axis
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 30)) +
  labs(x = "Estimated conditional mean difference (95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))

fig6_hlq_edu_model_forest <- ggplot(coef_df %>% filter(grepl("Education", term)), aes(x = estimate, y = outcome_label)) +
  # Zero vertical line
  geom_vline(xintercept = 0, linetype = 2) +
  # Point estimates and confidence intervals
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2) +
  facet_wrap(~ term, scales = "free_x") +
  # Make free x-axes symmetric about 0
  geom_blank(aes(x = -conf.low)) +
  geom_blank(aes(x = -conf.high)) +
  # Wrap text to be constrained width on y-axis
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 30)) +
  labs(x = "Estimated conditional mean difference (95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))

## PART 2a: PLOTTING HLQ OUTCOMES AS FUNCTION OF AGE AND SEX WITH SPLINES ##

# Modelling results just for age and sex as a natural cubic spline
dep_vars <- grep("^hlq[[:digit:]]_dom", names(data_ihf), value = TRUE)
dep_labels <- unlist(var_label(data_ihf)[match(dep_vars, names(data_ihf))])
dep_labels_break <- stringr::str_wrap(dep_labels, width = 32)
dep_labels_compact <- gsub(":.*", "", dep_labels)

# Pivot Data to Longer for Plotting with ggplot facets
data_ihf_plot <- data_ihf_reg %>%
  select(any_of(dep_vars), demo_age, demo_sex, weight_trim) %>%
  pivot_longer(cols = starts_with("hlq"), values_to = "Score") 
lab_map <- setNames(str_wrap(dep_labels, width = 30), dep_vars)

fig7_hlq_agesex_spline <- ggplot(data = data_ihf_plot, aes(x = demo_age, y = Score,
                                 group = demo_sex, colour = demo_sex, 
                                 fill = demo_sex)) +
  geom_point(aes(size = weight_trim), alpha = 0.1) + 
  geom_smooth(alpha = 0.3, method = "lm", formula = y ~ splines::ns(x, df = 3), 
              mapping = aes(weight = weight_trim)) +
  facet_wrap(~ name, labeller = as_labeller(lab_map)) +
  scale_y_continuous(name = "HLQ Score", breaks = 1:5, minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(20, 80, by = 10), minor_breaks = NULL) +
  scale_color_discrete(name = "Sex") +
  scale_fill_discrete(name = "Sex") +
  scale_size_continuous(guide = "none") + 
  guides(colour = guide_legend(position = "inside"),
         fill = guide_legend(position = "inside"),
         size = NULL) +
  theme_minimal() +
  theme(legend.position.inside = c(0.95, 0.975),
        strip.text.y = element_text(size = rel(0.8)))

## PART 2b: PLOTTING IHF FREQUENCY OF HEALTH SOURCES AS FUNCTION OF AGE AND SEX WITH SPLINES ##

# Modelling results just for age and sex
dep_vars <- grep("^ihfinfo_q[1-7]", names(data_ihf), value = TRUE)
dep_labels <- unlist(var_label(data_ihf)[match(dep_vars, names(data_ihf))])

model_res <- dep_vars %>% 
  ## set names so .id = argument works downstream
  setNames(dep_vars) %>% 
  map(\(dv) {
    f <- reformulate(termlabels = c("bs(demo_age_10, degree = 4) * demo_sex"), 
                     response = dv)
    vglm(formula = f,
         family = cumulative(parallel = TRUE, reverse = TRUE),
         data = data_ihf_reg,
         weights = weight_trim,   
         na.action = na.exclude)})

age_grid <- seq(20 / 10, 70 / 10, 
                length.out = 200)

# Build prediction grid and predict for one model
predict_ord_prob <- function(model, outcome_name){
  new_df <- expand.grid(
    demo_age_10 = age_grid,
    demo_sex = c("Male", "Female")
  )
  
  pred_matrix <- predictvglm(model, newdata = new_df, type = "response")
  
  # Get category names from the model
  n_cats <- ncol(pred_matrix)
  cat_names <- paste0("cat_", seq_len(n_cats))  # or supply real labels
  colnames(pred_matrix) <- cat_names
  
  bind_cols(new_df, as.data.frame(pred_matrix)) |>
    pivot_longer(cols = all_of(cat_names),
                 names_to  = "category",
                 values_to = "probability") |>
    mutate(outcome = outcome_name)
}

# Iterate over all models
pred_df <- imap_dfr(model_res, predict_ord_prob) %>%
  mutate(category = factor(category, 
                          levels = c("cat_1", "cat_2", "cat_3", "cat_4", "cat_5"),
                          labels = c("Never", "Rarely", "Sometimes", "Often", "Very often")))

# Formatted labels here for outcome
lab_map <- setNames(c("Male", "Female", str_wrap(dep_labels, width = 30)), c("Male", "Female", dep_vars))
# Plot stacked areas of probabilities for ordinal categories
fig7a_ihffreqprob_agesex_spline <- ggplot(pred_df, aes(x = demo_age_10, y = probability, fill = category)) +
  geom_area(alpha = 0.5) +
  facet_grid(outcome ~ demo_sex, labeller = as_labeller(lab_map)) +
  scale_fill_manual(name = "Frequency", values = (brewer.pal(5, "RdYlBu"))) +
  scale_x_continuous(labels = function(x) x * 10, name   = "Age (years)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), minor_breaks = NULL) +
  labs(y = "Probability of frequency", fill = "Category") +
  theme_minimal() +
  theme(strip.text.y = element_text(size = rel(0.8), angle = 0, hjust = 0))
  
  
## PART 3: MODELLING OF HLQ WITH OTHER PREDICTOR VARIABLES 
##  ADJUSTING FOR AGE-SEX-EDUCATION

# Outcome variables and predictor variables for inclusion in models
outcomes <- grep("^hlq[[:digit:]]_dom", names(data_ihf_reg), value = TRUE)
vary_preds <- c("demo_employment_4cat", "health_illdisab_count", "info_searchfreq_4cat")  
fixed_preds <- c("demo_age_10", "demo_sex", "demo_education")

# Fit models in data frame for different combinations of outcome and predictor variables
model_grid_results <- expand_grid(outcome = outcomes, extra_pred = vary_preds) %>%
  mutate(formula = map2(outcome, extra_pred, ~ reformulate(c(fixed_preds, .y), response = .x)),
         model = map(formula, ~ svyglm(.x, design = svy_ihf_reg,
                                       family = gaussian(link = "identity"),
                                       na.action = na.exclude)))
# Tidy summary of models in the same data frame format
# Select parameter estimates only relating to additional predictor variables for each model
extra_term_summaries <- model_grid_results %>%
  mutate(term_summary = map2(model, extra_pred, ~ broom::tidy(.x, conf.int = TRUE) %>% 
                               filter(grepl(.y, term)) %>%
                               select(term, estimate, conf.low, conf.high, p.value))) %>%
  unnest(term_summary) %>%
  group_by(outcome, extra_pred) %>%
  rowwise() %>%
  # Derive level variable for each contrast level of each categorical predictor variable
  mutate(level = if_else(term==extra_pred, extra_pred, gsub(extra_pred, "", term)),
         # Apply variable labels to outcome and predictor variables
         pred_label = unlist(var_label(data_ihf_reg)[match(extra_pred, names(data_ihf_reg))]),
         outcome_label = unlist(var_label(data_ihf_reg)[match(outcome, names(data_ihf_reg))])#,
         )#outcome_label = gsub(":.*", "", outcome_label))

# Format for joining
coef_extra_df <- extra_term_summaries %>%
  select(-formula, -model) %>%
  rename(pred = extra_pred)

# Combine
coef_all_df <- bind_rows(coef_df, 
                         coef_extra_df)

# Setup data frame for plotting
plot_df <- coef_all_df %>%
  # Set variables to factor (no need to specify order)
  ungroup() %>%
  mutate(outcome = factor(outcome),
         outcome_num = as.numeric(outcome),
         outcome_num = max(outcome_num) - outcome_num + 1,
         outcome_label = factor(outcome_label),
         outcome_label = fct_rev(outcome_label),
         level = factor(level),
         pred_label = fct_relevel(pred_label,
                                  "Age \n (per 10 years)",
                                  "Sex \n (reference: Female)",
                                  "Highest level of educational attainment \n (reference: Secondary)",
                                  "Employment status \n (reference: Employed full-time)", 
                                  "Number of long-standing health conditions \n (reference: None)",
                                  "Internet search frequency \n (reference: Daily)")) %>%
  # Set vertical position of each level/term within each predictor variable
  group_by(pred, outcome) %>%
  mutate(term_id = dense_rank(as.numeric(level)),
         k = n(),
         # Offsets centred at 0; spread controls degree of separation
         spread = 0.2,
         offset = if_else(k == 1, 0, (term_id - (k + 1) / 2) * (2 * spread / (k - 1))),
         y = outcome_num + offset) %>%
  ungroup() %>%
  # Put label just beyond the CI on the side of the estimate and push text outward
  mutate(x_label = if_else(estimate >= 0, conf.high, conf.low),
         hjust   = if_else(estimate >= 0, -0.1,  1.1))

# Choose where the mini-legend sits inside each facet
x_rng <- range(plot_df$conf.low, plot_df$conf.high, na.rm = TRUE)
dx    <- 0.33 * diff(x_rng)
legend_x <- x_rng[2] - dx                   # near right edge inside facet
y_top    <- max(plot_df$y, na.rm = TRUE)    # numeric y; legend goes above outcomes
dy       <- 0.32                            # vertical spacing between legend rows
# gap between text and point (in x units)
gap <- 0.04 * diff(x_rng)

# Mini/pseudo-legend data: only levels present in each facet
legend_df <- plot_df %>%
  distinct(pred_label, level, .keep_all = TRUE) %>%
  group_by(pred_label) %>%
  arrange(level, .by_group = TRUE) %>%
  mutate(term_id = dense_rank(100-as.numeric(level)),
         x_pt   = legend_x, # point position (right side)
         x_text = legend_x + gap, # text just right of point
         y = y_top + 0.9 - (term_id - 1) * dy) %>%
  ungroup()

# Generate plot
fig8_hlq_all_model_forest <- ggplot(plot_df, aes(x = estimate, y = y, colour = level)) +
  annotate("rect", 
           xmin = x_rng[1], xmax = x_rng[2], ymin = 0.5, ymax = 4.5, 
           fill = "lightgrey", alpha = 0.3) +#, colour = NULL) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2, show.legend = FALSE) +
  # points
  geom_point(data = legend_df,
             aes(x = x_pt, y = y, colour = level),
             inherit.aes = FALSE,
             size = 2) +
  # Right-aligned text (flush to the points)
  geom_label(data = legend_df,
            aes(x = x_text, y = y, label = level, colour = level),
            inherit.aes = FALSE,
            hjust = 0, size = 2.2) +
  facet_wrap(~ pred_label, nrow = 2) +
  scale_x_continuous(breaks = seq(-0.8, 0.8, by = 0.2), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq_along(levels(plot_df$outcome)),
                     minor_breaks = NULL,
                     labels = str_wrap(levels(plot_df$outcome_label), width = 30),
                     expand = expansion(add = c(0.1, 0.3))) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = rep(brewer.pal(8, "Dark2"), times = 2)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Estimated conditional mean difference (95% CI)", y = NULL)

# Store as table 
tbl6_hlq_all_model <- plot_df %>% select(Outcome = outcome_label, 
                                           Predictor = pred_label, 
                                           Level = level,
                                           Estimate = estimate, 
                                           conf.low, conf.high, 
                                           p.value) %>%
  mutate(Estimate = sprintf("%.2f", Estimate), 
         `95% CI` = paste0("(", sprintf("%.2f", conf.low), ", ", 
                           sprintf("%.2f", conf.high), ")"),
         `p-value` = label_pvalue()(p.value)) %>%
  select(-conf.low, -conf.high, -p.value) %>%
  flextable()



## PART 4: AGE-SEX-EDUCATION MODELLING OF HEALTH SOURCE FREQUENCY ##
# Modelling results just for age, sex, education adjustment
dep_vars <- grep("^ihfinfo_q[1-7]", names(data_ihf), value = TRUE)
dep_labels <- unlist(var_label(data_ihf)[match(dep_vars, names(data_ihf))])
dep_labels_break <- gsub(": ", "\\\\n", dep_labels)
dep_labels_compact <- gsub(":.*", "", dep_labels)

model_res <- dep_vars %>% 
  ## set names so .id = argument works downstream
  setNames(dep_vars) %>% 
  map(\(dv) {
    f <- reformulate(termlabels = c("demo_age_10", "demo_sex", "demo_education"), 
                     response = dv)
    vglm(formula = f,
         family = cumulative(parallel = TRUE, reverse = TRUE),
         data = data_ihf_reg,
         weights = weight_trim,   
         na.action = na.exclude)})
# Generate summary tables for each model
table_res <- model_res %>% map(tbl_regression, tidy_fun = broom.helpers::tidy_vgam) %>% 
  map(modify_column_hide, columns = p.value)


# LRT: proportional (model_res) vs non-proportional (model_res_npo)
fit_npo <- possibly(\(dv){
  f <- reformulate(termlabels = c("demo_age_10", "demo_sex", "demo_education"), 
                   response = dv)
  vglm(formula = f,
       family = cumulative(parallel = FALSE, reverse = TRUE),
       data = data_ihf_reg,
       weights = weight_trim,
       na.action = na.exclude)}, 
  otherwise = NA)
model_res_npo <- dep_vars %>% 
  setNames(dep_vars) %>% 
  map(fit_npo)
run_lrt <- possibly(\(po, npo) {
  if (!is(npo, "vglm")) return(tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_))
  lr <- lrtest(po, npo)@Body
  tibble(df      = lr$Df[2],
         LR_stat = lr$Chisq[2],
         p_value = lr$`Pr(>Chisq)`[2])}, 
  otherwise = tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_))
po_test_res <- map2(model_res, model_res_npo, run_lrt) %>% 
  list_rbind(names_to = "dep_var") %>% 
  mutate(dep_label     = dep_labels_compact[match(dep_var, dep_vars)],
         npo_converged = map_lgl(model_res_npo, \(m) is(m, "vglm")),
         po_violated   = p_value < 0.05)

# Merge tables vertically
tbl7_ihf_agesexedu_model <- tbl_stack(tbls = table_res, group_header = dep_labels)


# Create data frame for parameter estimates in model
coef_df <- imap(model_res, 
                ~{
                  term_labels <- attr(terms(.x), "term.labels")
                  pattern <- stringr::str_c(term_labels, collapse = "|")
                  broom.helpers::tidy_vgam(.x, conf.int = TRUE) %>% 
                    mutate(outcome = .y, 
                           pred  = stringr::str_extract(term, pattern),
                           level  = stringr::str_remove(term, pattern))}) %>%
  list_rbind() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       demo_age_10 = "Age (per 10 years)",
                       demo_sexMale = "Sex - male \n (vs female)",
                       `demo_educationVocational/Technical` = "Education \n Vocational/Techincal \n vs Secondary",
                       demo_educationUndergraduate = "Education \n Undergraduate \n vs Secondary",
                       demo_educationPostgraduate = "Education \n Postgraduate \n vs Secondary"),
         term = fct_rev(term)) %>%
  rowwise() %>%
  mutate(outcome_label = unlist(var_label(data_ihf)[match(outcome, names(data_ihf))]), 
         #outcome_label = gsub(":.*", "", outcome_label),
         pred_label = unlist(var_label(data_ihf_reg)[match(pred, names(data_ihf_reg))]),
         level = if_else(pred == "demo_age_10", "Per 10 years", level)) 

# Forest plot of parameter estimates
fig9_ihf_agesex_model_forest <- ggplot(coef_df %>% filter(grepl("(Age)|(Sex)", term)), aes(x = estimate, y = outcome_label)) +
  # Zero vertical line
  geom_vline(xintercept = 0, linetype = 2) +
  # Point estimates and confidence intervals
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2) +
  facet_wrap(~ term, scales = "free_x") +
  # Make free x-axes symmetric about 0
  geom_blank(aes(x = -conf.low)) +
  geom_blank(aes(x = -conf.high)) +
  # Wrap text to be constrained width on y-axis
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 30)) +
  labs(x = "Estimated log odds-ratio\n(95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))

fig9_ihf_edu_model_forest <- ggplot(coef_df %>% filter(grepl("Education", term)), aes(x = estimate, y = outcome_label)) +
  # Zero vertical line
  geom_vline(xintercept = 0, linetype = 2) +
  # Point estimates and confidence intervals
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2) +
  facet_wrap(~ term, scales = "free_x") +
  # Make free x-axes symmetric about 0
  geom_blank(aes(x = -conf.low)) +
  geom_blank(aes(x = -conf.high)) +
  # Wrap text to be constrained width on y-axis
  scale_y_discrete(name = NULL, labels = function(x) str_wrap(x, width = 30)) +
  labs(x = "Estimated log odds-ratio\n(95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))


## PART 5: MODELLING OF HEALTH SOURCE FREQUENCY WITH OTHER PREDICTOR VARIABLES 
##  ADJUSTING FOR AGE-SEX-EDUCATION
# Outcome variables and predictor variables for inclusion in models
outcomes <- grep("^ihfinfo_q[1-7]", names(data_ihf_reg), value = TRUE)
vary_preds <- c("demo_employment_4cat", "health_illdisab_count", "info_searchfreq_4cat")  
fixed_preds <- c("demo_age_10", "demo_sex", "demo_education")

# Fit models in data frame for different combinations of outcome and predictor variables
model_grid_results <- expand_grid(outcome = outcomes, extra_pred = vary_preds) %>%
  mutate(formula = map2(outcome, extra_pred, ~ reformulate(c(fixed_preds, .y), response = .x)),
         model = map(formula, ~ vglm(.x, family = cumulative(parallel = TRUE, reverse = TRUE),
                                     data = data_ihf_reg,
                                     weights = weight_trim,
                                     na.action = na.exclude)))

# LRT: proportional (model_res) vs non-proportional (model_res_npo)
fit_npo <- possibly(\(dv){
  f <- reformulate(termlabels = c("demo_age_10", "demo_sex", "demo_education"), 
                   response = dv)
  vglm(formula = f,
       family = cumulative(parallel = FALSE, reverse = TRUE),
       data = data_ihf_reg,
       weights = weight_trim,
       na.action = na.exclude)}, 
  otherwise = NA)
model_res_npo <- dep_vars %>% 
  setNames(dep_vars) %>% 
  map(fit_npo)
run_lrt <- possibly(\(po, npo) {
  if (!is(npo, "vglm")) return(tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_))
  lr <- lrtest(po, npo)@Body
  tibble(df      = lr$Df[2],
         LR_stat = lr$Chisq[2],
         p_value = lr$`Pr(>Chisq)`[2])}, 
  otherwise = tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_))
po_test_res <- map2(model_res, model_res_npo, run_lrt) %>% 
  list_rbind(names_to = "dep_var") %>% 
  mutate(dep_label     = dep_labels_compact[match(dep_var, dep_vars)],
         npo_converged = map_lgl(model_res_npo, \(m) is(m, "vglm")),
         po_violated   = p_value < 0.05)

model_grid_results_npo <- model_grid_results %>% 
  mutate(
    model_npo = map(formula, possibly(\(f) 
                                      vglm(f, family = cumulative(parallel = FALSE, reverse = TRUE),
                                           data = data_ihf_reg,
                                           weights = weight_trim,
                                           na.action = na.exclude),
                                      otherwise = NA)),
    npo_converged = map_lgl(model_npo, \(m) is(m, "vglm")),
    lrt = map2(model, model_npo, possibly(\(po, npo) {
      if (!is(npo, "vglm")) return(tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_))
      lr <- lrtest(po, npo)@Body
      tibble(df = lr$Df[2], LR_stat = lr$Chisq[2], p_value = lr$`Pr(>Chisq)`[2])
    }, otherwise = tibble(df = NA_real_, LR_stat = NA_real_, p_value = NA_real_)))
  ) %>% 
  unnest(lrt) %>% 
  mutate(po_violated = p_value < 0.05)
po_summary <- model_grid_results_npo %>% 
  select(outcome, extra_pred, npo_converged, df, LR_stat, p_value, po_violated)
po_summary <- po_summary %>% 
  mutate(p_adj = p.adjust(p_value, method = "BH"),
         po_violated_adj = p_adj < 0.05)

# Tidy summary of models in the same data frame format
# Select parameter estimates only relating to additional predictor variables for each model
extra_term_summaries <- model_grid_results %>%
  mutate(term_summary = map2(model, extra_pred, ~ broom.helpers::tidy_vgam(.x, conf.int = TRUE) %>% 
                               filter(grepl(.y, term)) %>%
                               select(term, estimate, conf.low, conf.high, p.value))) %>%
  unnest(term_summary) %>%
  group_by(outcome, extra_pred) %>%
  rowwise() %>%
  # Derive level variable for each contrast level of each categorical predictor variable
  mutate(level = if_else(term==extra_pred, extra_pred, gsub(extra_pred, "", term)),
         # Apply variable labels to outcome and predictor variables
         pred_label = unlist(var_label(data_ihf_reg)[match(extra_pred, names(data_ihf_reg))]),
         outcome_label = unlist(var_label(data_ihf_reg)[match(outcome, names(data_ihf_reg))]))#,
         #outcome_label = gsub(":.*", "", outcome_label))

# Format for joining
coef_extra_df <- extra_term_summaries %>%
  select(-formula, -model) %>%
  rename(pred = extra_pred)

# Combine
coef_all_df <- bind_rows(coef_df, 
                         coef_extra_df)

# Setup data frame for plotting
plot_df <- coef_all_df %>%
  ungroup() %>%
  # Set variables to factor (no need to specify order)
  mutate(outcome = factor(outcome),
         outcome_num = as.numeric(outcome),
         outcome_num = max(outcome_num) - outcome_num + 1,
         outcome_label = factor(outcome_label),
         outcome_label = fct_rev(outcome_label),
         level = factor(level, 
                        levels = c("Per 10 years", 
                                   "Male", 
                                   "Vocational/Technical", "Undergraduate", "Postgraduate", 
                                   "Employed part-time", "Full-time student",  "Other",
                                   "1", "2 or more",
                                   "Weekly", "Monthly", "Rarely/Never")),
         pred_label = fct_relevel(pred_label,
                                  "Age \n (per 10 years)",
                                  "Sex \n (reference: Female)",
                                  "Highest level of educational attainment \n (reference: Secondary)",
                                  "Employment status \n (reference: Employed full-time)", 
                                  "Number of long-standing health conditions \n (reference: None)",
                                  "Internet search frequency \n (reference: Daily)")) %>%
  # Set vertical position of each level/term within each predictor variable
  group_by(pred, outcome) %>%
  mutate(term_id = dense_rank(as.numeric(level)),
         k = n(),
         # Offsets centred at 0; spread controls degree of separation
         spread = 0.2,
         offset = if_else(k == 1, 0, (term_id - (k + 1) / 2) * (2 * spread / (k - 1))),
         y = outcome_num + offset) %>%
  ungroup() %>%
  # Put label just beyond the CI on the side of the estimate and push text outward
  mutate(x_label = if_else(estimate >= 0, conf.high, conf.low),
         hjust   = if_else(estimate >= 0, -0.1,  1.1))

# Choose where the mini-legend sits inside each facet
x_rng <- range(plot_df$conf.low, plot_df$conf.high, na.rm = TRUE)
dx    <- 0.4 * diff(x_rng)
legend_x <- x_rng[2] - dx                   # near right edge inside facet
y_top    <- max(plot_df$y, na.rm = TRUE)    # numeric y; legend goes above outcomes
dy       <- 0.3                            # vertical spacing between legend rows
# gap between text and point (in x units)
gap <- 0.04 * diff(x_rng)

# Mini/pseudo-legend data: only levels present in each facet
legend_df <- plot_df %>%
  distinct(pred_label, level, .keep_all = TRUE) %>%
  group_by(pred_label) %>%
  arrange(level, .by_group = TRUE) %>%
  mutate(term_id = dense_rank(100-as.numeric(level)),
         x_pt   = legend_x, # point position (right side)
         x_text = legend_x + gap, # text just right of point
         y = y_top + 0.9 - (term_id - 1) * dy) %>%
  ungroup()

# Generate plot
fig10_ihf_all_model_forest <- ggplot(plot_df, aes(x = estimate, y = y, colour = level)) +
  annotate("rect", 
           xmin = x_rng[1], xmax = x_rng[2], ymin = 0.5, ymax = 4.5, 
           fill = "lightgrey", alpha = 0.3) +#, colour = NULL) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.2, show.legend = FALSE) +
  # points
  geom_point(data = legend_df,
             aes(x = x_pt, y = y, colour = level),
             inherit.aes = FALSE,
             size = 2) +
  # Right-aligned text (flush to the points)
  geom_label(data = legend_df,
            aes(x = x_text, y = y, label = level, colour = level),
            inherit.aes = FALSE,
            hjust = 0, size = 2.2) +
  facet_wrap(~ pred_label, nrow = 2) +
  scale_x_continuous(breaks = seq(-2, 2, by = 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq_along(levels(plot_df$outcome)),
                     minor_breaks = NULL,
                     labels = str_wrap(levels(plot_df$outcome_label), width = 30),
                     expand = expansion(add = c(0.1, 0.3))) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = rep(brewer.pal(8, "Dark2"), times = 2)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Estimated log odds-ratio (95% CI)", y = NULL)

# Store as table 
tbl8_ihf_all_model <- plot_df %>% select(Outcome = outcome_label, 
                                           Predictor = pred_label, 
                                           Level = level,
                                           `Estimated log odds-ratio` = estimate, 
                                           conf.low, conf.high, 
                                           p.value) %>%
  mutate(`Estimated log odds-ratio` = sprintf("%.2f", `Estimated log odds-ratio`), 
         `95% CI` = paste0("(", sprintf("%.2f", conf.low), ", ", 
                           sprintf("%.2f", conf.high), ")"),
         `p-value` = label_pvalue()(p.value)) %>%
  select(-conf.low, -conf.high, -p.value) %>%
  flextable()



# Save outputs for inclusion in report
save(tbl5_hlq_agesexedu_model, 
     fig7_hlq_agesex_spline, fig7a_ihffreqprob_agesex_spline, 
     fig8_hlq_all_model_forest, tbl6_hlq_all_model,
     tbl7_ihf_agesexedu_model, 
     fig10_ihf_all_model_forest, tbl8_ihf_all_model,
     file = "outcome_models.RData")
