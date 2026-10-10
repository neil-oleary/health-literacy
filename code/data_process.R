library(tidyverse)
library(readxl)
library(labelled)
library(patchwork)

# Read local data export for variable names - including second row
col_names_1 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                              sheet = "Raw Data", n_max = 1, col_names = FALSE))
col_names_2 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                              sheet = "Raw Data", skip = 1, n_max = 1, col_names = FALSE, na = "NA"))
col_names_1 <- pull(tidyr::fill(tibble(test = unlist(col_names_1)), test, .direction = "down"), test)
col_names_2[is.na(col_names_2)] <- ""
col_names <- paste(col_names_1, col_names_2)


# Create variable labels
var_labels_1 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-basic format.xlsx", 
                                sheet = "Raw Data", n_max = 1, col_names = FALSE))
var_labels_2 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-basic format.xlsx", 
                                sheet = "Raw Data", skip = 1, n_max = 1, col_names = FALSE, na = "NA"))
var_labels_1 <- pull(tidyr::fill(tibble(test = unlist(var_labels_1)), test, .direction = "down"), test)
var_labels_2[is.na(var_labels_2)] <- ""
var_labels <- paste(var_labels_1, var_labels_2)

var_label_list <- as.list(var_labels) %>%
  setNames(col_names)

# Read local data export for labelled data
data_ihf_raw <- read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                            sheet = "Raw Data", skip = 2, col_names = FALSE)
colnames(data_ihf_raw) <- col_names

data_ihf_raw <- data_ihf_raw %>%
  set_variable_labels(.labels = var_label_list)


# Start manually selecting and renaming variables
data_ihf <- data_ihf_raw %>%
  select(response_id = `Response ID `,
         demo_age = `6 - Q34 `,
         demo_sex = `7 - Q35 `,
         demo_genderident = `8 - Q36 `,
         demo_education = `9 - Q43 `,
         demo_employment = `10 - Q10 `,
         demo_location = `15 - Q15 `,
         demo_ethnic = `16 - Q16 `,
         demo_residdur = `17 - Q17 `,
         demo_lang_eng = `18 - Q18 English`,
         demo_lang_irish = `18 - Q18 Irish`,
         demo_lang_polish = `18 - Q18 Polish`,
         demo_lang_romanian = `18 - Q18 Romanian`,
         demo_lang_french = `18 - Q18 French`,
         demo_lang_spanish = `18 - Q18 Spanish`,
         demo_lang_portuguese = `18 - Q18 Portuguese`,
         demo_lang_lithuanian = `18 - Q18 Lithuanian`,
         demo_lang_german = `18 - Q18 German`,
         demo_lang_chinese = `18 - Q18 Chinese (any)`,
         demo_lang_malayalam = `18 - Q18 Malayalam`,
         demo_lang_arabic = `18 - Q18 Arabic`,
         demo_lang_russian = `18 - Q18 Russian`,
         demo_lang_italian = `18 - Q18 Italian`,
         demo_lang_other = `18 - Q18 Other (please specify)`,
         health_illdisab_arth = `11 - Q9 Arthritis`,
         health_illdisab_back = `11 - Q9 Back pain`,
         health_illdisab_heart = `11 - Q9 Heart problems`,
         health_illdisab_asthmalung = `11 - Q9 Asthma or a lung condition`,
         health_illdisab_cancer = `11 - Q9 Cancer`,
         health_illdisab_depanx = `11 - Q9 Depression or anxiety`,
         health_illdisab_diabetes = `11 - Q9 Diabetes`,
         health_illdisab_stroke = `11 - Q9 Stroke`,
         health_illdisab_other = `11 - Q9 Other (please specify)`,
         health_illdisab_none = `11 - Q9 None`, 
         health_insurance = `12 - Q10 `,
         health_medcard = `13 - Q11 `,
         health_hosped12 = `14 - Q38 `,
         info_internet = `19 - Q19 `,
         info_searchfreq = `20 - Q21 `,
         info_device_phone = `21 - Q20 Smartphone`,
         info_device_tablet = `21 - Q20 Tablet`,
         info_device_laptop = `21 - Q20 Laptop`,
         info_device_desktop = `21 - Q20 Desktop computer`,
         info_device_other = `21 - Q20 Other (please specify)`,
         info_device_none = `21 - Q20 None`,
         starts_with("22 - Q24"),
         starts_with("23 - Q25"),
         starts_with("24 - Q26"),
         starts_with("25 - Q27"),
         starts_with("26 - Q28"),
         starts_with("27 - Q29")) %>%
  # Replace non-standard space character
  mutate(across(where(is.character), ~gsub("\u00A0", " ", .x))) %>%
  # Replace " (please specify)" text
  mutate(across(where(is.character), ~gsub(" \\(please specify [[:alpha:]]+\\)", "", .x))) %>%
  # Need to convert responses for multiple selection questions into "yes"/"no"
  mutate(across(c(starts_with("health_illdisab"), 
                  starts_with("demo_lang"),
                  starts_with("info_device")), 
                ~if_else(is.na(.x), "No", "Yes"))) %>%
  # Convert to explicit factors
  mutate(across(c(info_internet,starts_with("demo_lang")), 
                ~factor(.x, levels = c("No", "Yes")))) %>%
  # Trim some factor responses and order
  mutate(demo_education = gsub("\\s*\\([^)]*\\)\\s*$", "", demo_education),
         demo_education = factor(demo_education, 
                                 levels = c("Secondary education",
                                            "Vocational/Technical Training education",
                                            "Undergraduate degree",
                                            "Postgraduate degree"),
                                 labels = c("Secondary", "Vocational/Technical", 
                                            "Undergraduate", "Postgraduate"))) %>%
  mutate(demo_location = factor(demo_location, 
                                levels = c("In a city (e.g., Dublin, Cork, Limerick, Galway, Waterford, Belfast) ",
                                           "In a large town (e.g., Sligo, Drogheda, Dundalk, Navan) ", 
                                           "In a small town or village (e.g., Kinsale, Westport, Enniscorthy) ", 
                                           "In a rural area (countryside, outside of towns or cities) "),
                                labels = c("City", "Large town", "Small town or village", "Rural area"))) %>%
  mutate(info_searchfreq = factor(info_searchfreq, 
                                  levels = c("Daily", "Weekly", "Monthly", "Rarely", "Never"))) %>%
  mutate(demo_genderident = factor(demo_genderident,
                                   levels = c("No - please specify.", "Yes"),
                                   labels = c("No", "Yes"))) %>%
  # Derived variables for count of disability/illness
  rowwise() %>%
  mutate(health_illdisab_count = 
           sum(c_across(starts_with("health_illdisab") & !matches("none|other"))=="Yes", na.rm = TRUE),
         health_illdisab_count = case_when(
           health_illdisab_count == 0 ~ "0",
           health_illdisab_count == 1 ~ "1",
           health_illdisab_count > 1 ~ "2 or more")) %>%
  ungroup() %>%
  # Relabel variable names for instruments
  # HLQ Part 1 (23 questions)
  rename_with(.fn = ~str_c("hlq_p1_q" , 1:23), .cols = starts_with("22 - Q24")) %>%
  # HLQ Part 2 (21 questions)
  rename_with(.fn = ~str_c("hlq_p2_q" , 1:21), .cols = starts_with("23 - Q25")) %>%
  # eHEALS (9 questions)
  rename_with(.fn = ~str_c("eheals_q" , 1:9), .cols = matches("(24 - Q26)|(25 - Q27)|(26 - Q28)")) %>%
  # iHF information (9 questions)
  rename_with(.fn = ~str_c("ihfinfo_q" , 1:8), .cols = starts_with("27 - Q29")) %>%
  # Need to convert to ordered factors with common responses
  mutate(across(starts_with("hlq_p1_q"), 
                ~factor(.x, levels = c("Strongly disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly agree"),
                        ordered = TRUE))) %>%
  mutate(across(starts_with("hlq_p2_q"), 
                ~factor(.x, levels = c("Cannot do or always difficult",
                                          "Usually difficult",
                                          "Sometimes difficult",
                                          "Usually easy",
                                          "Always easy"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q[3-9]$"), 
                ~factor(.x, levels = c("Strongly disagree",
                                          "Disagree", 
                                          "Agree", 
                                          "Strongly agree"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q1$"), 
                ~factor(.x, levels = c("Not useful at all",
                                          "Not useful",
                                          "Unsure",
                                          "Useful",
                                          "Very useful"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q2$"), 
                ~factor(.x, levels = c("Not important at all",
                                          "Not important",
                                          "Unsure",
                                          "Important",
                                          "Very important"),
                           ordered = TRUE))) %>%
  mutate(across(matches("ihfinfo_q[1-8]$"), 
                ~factor(.x, levels = c("Never", 
                                          "Rarely", 
                                          "Sometimes", 
                                          "Often", 
                                          "Very often"),
                           ordered = TRUE))) 

# Generate the domain scores (in separate dataframe to avoid numeric conversion errors)
data_domains <- data_ihf %>%
  mutate(across(starts_with("hlq_p1_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("hlq_p2_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("eheals_q"), ~as.numeric(.x))) %>%
  # Generate the domain scores
  rowwise() %>%
  mutate(hlq1_dom_1 = mean((c_across(matches("hlq_p1_q(2|8|17|22)$")))),
         hlq1_dom_2 = mean((c_across(matches("hlq_p1_q(1|10|14|23)$")))),
         hlq1_dom_3 = mean((c_across(matches("hlq_p1_q(6|9|13|18|21)$")))),
         hlq1_dom_4 = mean((c_across(matches("hlq_p1_q(3|5|11|15|19)$")))),
         hlq1_dom_5 = mean((c_across(matches("hlq_p1_q(4|7|12|16|20)$")))),
         hlq2_dom_1 = mean((c_across(matches("hlq_p2_q(2|4|7|15|22)$")))),
         hlq2_dom_2 = mean((c_across(matches("hlq_p2_q(1|8|11|13|16|19)$")))),
         hlq2_dom_3 = mean((c_across(matches("hlq_p2_q(3|6|10|14|18)$")))),
         hlq2_dom_4 = mean((c_across(matches("hlq_p2_q(5|9|12|17|21)$")))),
         eheals_score = sum((c_across(matches("eheals_q[3-9]$"))))) %>%
  #select(contains("hlq")) %>%
  select(contains("_dom_"), "eheals_score")

# Combine with main dataset
data_ihf <- data_ihf %>%
  cbind(data_domains)

# Add HLQ raw item variable labels
hlq_var_label_list <- var_label_list[c(grep("^22 - Q24", names(var_label_list)), 
                                       grep("^23 - Q25", names(var_label_list)))]
names(hlq_var_label_list) <- c(str_c("hlq_p1_q" , 1:23), str_c("hlq_p2_q" , 1:21))
data_ihf <- data_ihf %>%
  set_variable_labels(.labels = hlq_var_label_list, .strict = FALSE)


# Labels for data
var_label(data_ihf) <- list(
  # Demographics
  demo_age = "Age (years)",
  demo_sex = "Sex",
  demo_genderident = "Gender identity same as the sex at birth",
  demo_education = "Highest level of educational attainment",
  demo_employment = "Employment status",
  demo_location = "Location",
  demo_ethnic = "Ethnicity",
  demo_residdur = "Years living in Ireland",
  demo_lang_eng = "Native language English",
  demo_lang_irish = "Native language Irish",
  demo_lang_polish = "Native language Polish",
  demo_lang_romanian = "Native language Romanian",
  demo_lang_french = "Native language French",
  demo_lang_spanish = "Native language Spanish",
  demo_lang_portuguese = "Native language Portuguese",
  demo_lang_lithuanian = "Native language Lithuanian",
  demo_lang_german = "Native language German",
  demo_lang_chinese = "Native language Chinese",
  demo_lang_malayalam = "Native language Malayalam",
  demo_lang_arabic = "Native language Arabic",
  demo_lang_russian = "Native language Russian",
  demo_lang_italian = "Native language Italian",
  demo_lang_other = "Native language Other",
  # Long-standing illness or disability
  health_illdisab_arth = "Arthritis",
  health_illdisab_back = "Back pain",
  health_illdisab_heart = "Heart problems",
  health_illdisab_asthmalung = "Asthma or lung condition",
  health_illdisab_cancer = "Cancer",
  health_illdisab_depanx = "Depression or anxiety",
  health_illdisab_diabetes = "Diabetes",
  health_illdisab_stroke = "Stroke",
  health_illdisab_other = "Other",
  health_illdisab_none = "None",
  health_illdisab_count = "Number of long-standing health conditions",
  health_insurance = "Health insurance",
  health_medcard = "Medical card",
  health_hosped12 = "Attended hospital in past 12 months",
  # Health information sources
  info_internet = "Regular access to internet",
  info_searchfreq = "Frequency of using internet to search for health information",
  info_device_phone = "Regularly use phone to access health information",
  info_device_tablet = "Regularly use tablet to access health information",
  info_device_laptop = "Regularly use laptop to access health information",
  info_device_desktop = "Regularly use desktop computer to access health information",
  info_device_other = "Regularly use other device to access health information",
  info_device_none = "Don't use any device to access health information",
  # Health literacy questionnaire data
  hlq1_dom_1 = "HLQ P1 D1: Feeling understood and supported by healthcare providers",
  hlq1_dom_2 = "HLQ P1 D2: Having sufficient information to manage my health",
  hlq1_dom_3 = "HLQ P1 D3: Actively managing my health",
  hlq1_dom_4 = "HLQ P1 D4: Social support for health",
  hlq1_dom_5 = "HLQ P1 D5: Appraisal of health information",
  hlq2_dom_1 = "HLQ P2 D1: Ability to actively engage with healthcare providers",
  hlq2_dom_2 = "HLQ P2 D2: Navigating the healthcare system",
  hlq2_dom_3 = "HLQ P2 D3: Ability to find good health information",
  hlq2_dom_4 = "HLQ P2 D4: Understand health information well enough to know what to do",
  # eHEALS responses
  eheals_score = "eHEALs score",
  eheals_q1 = "eHEALS Q1: How useful do you feel the internet is in helping you in making decisions about your health?",
  eheals_q2 = "eHEALS Q2: How important is it for you to be able to access health resources on the internet?",
  eheals_q3 = "eHEALS Q3: I know what health resources are available on the Internet.",
  eheals_q4 = "eHEALS Q4: I know where to find helpful health resources on the Internet.",
  eheals_q5 = "eHEALS Q6: I know how to use the Internet to answer my questions about health.",
  eheals_q6 = "eHEALS Q7: I know how to use the health information I find on the Internet to help me.",
  eheals_q7 = "eHEALS Q8: I have the skills I need to evaluate the health resources I find on the Internet.",
  eheals_q8 = "eHEALS Q9: I can tell high-quality health resources from low-quality health resources on the Internet.",
  eheals_q9 = "eHEALS Q10 I feel confident in using information from the Internet to make health decisions.",
  # iHF Frequency of getting information from different sources
  ihfinfo_q1 = "iHF Q1: Clinical healthcare professional",
  ihfinfo_q2 = "iHF Q2: Health service or public health organisations or officials",
  ihfinfo_q3 = "iHF Q3: Government health advice",
  ihfinfo_q4 = "iHF Q4: News outlets",
  ihfinfo_q5 = "iHF Q5: Independent websites or blogs",
  ihfinfo_q6 = "iHF Q6: Social media",
  ihfinfo_q7 = "iHF Q7: Family, friends, co-workers, or neighbours, including through Group Messaging and Communication Apps",
  ihfinfo_q8 = "iHF Q8: Other"
  )

# Do some regular expression edits of variable labels
df_var_labs <- enframe(var_label(data_ihf), name = "var_name", value = "var_label") %>%
  mutate(var_label = map_chr(var_label, \(v) v[[1]])) %>%
  # Non-standard space character converted to make it easier to edit labels
  mutate(var_label = gsub("\u00A0", " ", var_label)) %>%
  # Remove/edit long question prefaces
  mutate(var_label = gsub("This section asks about how you use the health information available on the internet. Please give your response by ticking the relevant box. Remember to check only one box for each statement.  ",
                          "", var_label),
         var_label = gsub("Please indicate how strongly you disagree or agree with each of the following statements. Remember to check only one box for each statement. In this questionnaire, the term healthcare providers means doctors, nurses, physiotherapists and any other health worker you seek advice or treatment from.  ",
                          "", var_label),
         var_label = gsub("Please indicate how difficult or easy the following tasks are for you now. Remember to check only one box for each statement. In this questionnaire, the term healthcare providers means doctors, nurses, physiotherapists and any other health worker you seek advice or treatment from.  ",
                          "", var_label),
         var_label = gsub("How often do you use the following sources to get health information\\? ",
                          "Frequency of getting health information from", var_label))
# Apply variable labels to data frame
var_label(data_ihf) <- deframe(df_var_labs)



