library(tidyverse)
library(readxl)
library(labelled)

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
data_labelled <- read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                            sheet = "Raw Data", skip = 2, col_names = FALSE)
colnames(data_labelled) <- col_names

data_labelled <- data_labelled %>%
  set_variable_labels(.labels = var_label_list)


# Start manually selecting and renaming variables
data_processed <- data_labelled %>%
  select(demo_age = `6 - Q34 `,
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
  # Need to convert responses for multiple selection questions into "yes"/"no"
  mutate(across(c(starts_with("health_illdisab"), 
                  starts_with("demo_lang"),
                  starts_with("info_device")), 
                ~if_else(is.na(.x), "No", "Yes"))) %>%
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
                ~to_factor(.x, levels = c("Strongly disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly agree"),
                        ordered = TRUE))) %>%
  mutate(across(starts_with("hlq_p2_q"), 
                ~to_factor(.x, levels = c("Cannot do or always difficult",
                                          "Usually difficult",
                                          "Sometime difficult",
                                          "Usually easy",
                                          "Always easy"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q[3-9]$"), 
                ~to_factor(.x, levels = c("Strongly disagree",
                                          "Disagree", 
                                          "Agree", 
                                          "Strongly agree"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q1$"), 
                ~to_factor(.x, levels = c("Not useful at all",
                                          "Not useful",
                                          "Unsure",
                                          "Useful",
                                          "Very useful"),
                           ordered = TRUE))) %>%
  mutate(across(matches("eheals_q2$"), 
                ~to_factor(.x, levels = c("Not important at all",
                                          "Not important",
                                          "Unsure",
                                          "Important",
                                          "Very important"),
                           ordered = TRUE))) %>%
  mutate(across(matches("ihfinfo_q[1-8]$"), 
                ~to_factor(.x, levels = c("Never", 
                                          "Rarely", 
                                          "Sometimes", 
                                          "Often", 
                                          "Very often"),
                           ordered = TRUE))) 

# Generate the domain scores (in separate dataframe to avoid numeric conversion errors)
data_domains <- data_processed %>%
  mutate(across(starts_with("hlq_p1_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("hlq_p2_q"), ~as.numeric(.x))) %>%
  mutate(across(starts_with("eheals_q"), ~as.numeric(.x))) %>%
  # Generate the domain scores
  rowwise() %>%
  mutate(hlq1_dom_1 = mean((c_across(matches("hlq_p1_q((2)|(8)|(17))|(22))$")))),
         hlq1_dom_2 = mean((c_across(matches("hlq_p1_q((1)|(10)|(14))|(23))$")))),
         hlq1_dom_3 = mean((c_across(matches("hlq_p1_q((6)|(9)|(13))|(18)|(21))$")))),
         hlq1_dom_4 = mean((c_across(matches("hlq_p1_q((3)|(5)|(11))|(15)|(19))$")))),
         hlq1_dom_5 = mean((c_across(matches("hlq_p1_q((4)|(7)|(12))|(16)|(20))$")))),
         hlq2_dom_1 = mean((c_across(matches("hlq_p2_q((2)|(4)|(7))|(15)|(22))$")))),
         hlq2_dom_2 = mean((c_across(matches("hlq_p2_q((1)|(8)|(11))|(13)|(16)|(19))$")))),
         hlq2_dom_3 = mean((c_across(matches("hlq_p2_q((3)|(6)|(10))|(14)|(18))$")))),
         hlq2_dom_4 = mean((c_across(matches("hlq_p2_q((5)|(9)|(12))|(17)|(21))$")))),
         eheals_score = sum((c_across(matches("eheals_q[3-9]$"))))) %>%
  select(contains("_dom_"), "eheals_score")

# Combine with main dataset
data_processed <- data_processed %>%
  cbind(data_domains)

