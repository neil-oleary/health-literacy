library(tidyverse)
library(readxl)

# Read local data export - skipping second row
col_names_1 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                              sheet = "Raw Data", n_max = 1, col_names = FALSE))
col_names_2 <- array(read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                              sheet = "Raw Data", skip = 1, n_max = 1, col_names = FALSE, na = "NA"))
col_names_1 <- pull(tidyr::fill(tibble(test = unlist(col_names_1)), test, .direction = "down"), test)
col_names_2[is.na(col_names_2)] <- ""
col_names <- paste(col_names_1, col_names_2)
data_labelled <- read_excel("../../Data Exports/QuestionPro-SR-RawData-display answer and questions format.xlsx", 
                            sheet = "Raw Data", skip = 2, col_names = FALSE)
colnames(data_labelled) <- col_names

