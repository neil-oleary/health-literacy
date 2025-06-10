# Inspect distribution of individual domain scores

data_summary_scores <- data_ihf %>%
  pivot_longer(cols = matches("(_dom_[1-4])|(eheals_score)"),
               values_to = "value",
               names_to = "score_type")

# Histogram of each domain or instrument score - by type
ggplot(data = data_summary_scores, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~score_type, scales = "free_x")


# Tabular summaries of proportion of scores at minimum or maximum
data_summary_scores %>% 
  group_by(score_type) %>%
  summarise(prop_max = mean(value == max(value)),
            prop_min = mean(value == min(value)))

# No obvious floor or ceiling effects, apart from potentially for eHEALS score with 26.3% of participants with the mininmum score