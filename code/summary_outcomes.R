# Outcome variable descriptive summaries
# As per 5.2.2 of study SAP

# Summary statistics will be generated for response variables (items and domain and instrument-aggregated scores) in the following tables along with graphical summaries for the following variable types:
#   •	Descriptive Table 2:
#   o	Categorical variables (e.g., sex, education) → Frequencies & percentages
#   o	Continuous variables (e.g., age) → Mean, SD, median, IQR 
# •	Data visualisation:
#   o	Bar charts for categorical data
# o	Histograms for continuous data
# All descriptive analysis will be presented as unweighted and weighted and include the count and proportions of missing data.


# The following tables will be prepared: 
#   Table 2: Unweighted and weighted* demographic summaries

# HLQ item-level summaries (long format for plotting)
data_hlq_item_long <- data_ihf %>%
  select(matches("^hlq_p[[:digit:]]_q"), weight) %>%
  mutate(across(starts_with("hlq"), ~as.numeric(.x))) %>%
  pivot_longer(cols = starts_with("hlq"), names_to = "item", values_to = "response") %>%
  mutate(item_num = str_split_i(item, "_q", 2),
         section_num = gsub("p", "", str_split_i(item, "_", 2))) %>%
  arrange(section_num, item_num) %>%
  mutate(item = factor(item, levels = c(str_c("hlq_p1_q" , 1:23), str_c("hlq_p2_q" , 1:21)),
         labels = c(str_c("Part 1 - Item " , 1:23), str_c("Part 2 - Item " , 1:21))))


# eHEALS item-level summaries (long format for plotting)
data_eheals_item_long <- data_ihf %>%
  select(matches("^eheals_q"), weight) %>%
  mutate(across(starts_with("eheals_q"), ~as.numeric(.x))) %>%
  pivot_longer(cols = starts_with("eheals_q"), names_to = "item", values_to = "response") %>%
  mutate(item_num = str_split_i(item, "_q", 2)) %>%
  arrange(item_num) %>%
  mutate(item = factor(item, levels = c(str_c("eheals_q" , 1:9)),
                       labels = c(str_c("Item " , 1:9))))

# IHF item-level summaries (long format for plotting)
data_ihf_item_long <- data_ihf %>%
  select(matches("^ihfinfo_q"), weight) %>%
  pivot_longer(cols = starts_with("ihfinfo_q"), names_to = "item", values_to = "response") %>%
  mutate(item_num = str_split_i(item, "_q", 2)) %>%
  arrange(item_num) %>%
  mutate(response = factor(response, levels = c("Never", 
                                            "Rarely", 
                                            "Sometimes", 
                                            "Often", 
                                            "Very often"),
                       ordered = TRUE)) 

# Domain-level and instrument summaries for HLQ and eHEALS
data_domain <- data_ihf %>%
  select(matches("^hlq[[:digit:]]_dom"), eheals_score, weight)
# Creat survey objects - for weighted tabulation
svy_domain_ihf <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_domain
  )
# Long format for plotting
data_domain_long <- data_domain %>%
  pivot_longer(cols = c(matches("^hlq[[:digit:]]_dom"), eheals_score), 
               names_to = "domain", values_to = "response") %>%
  mutate(domain = factor(domain, 
                         levels = c(str_c("hlq1_dom_" , 1:5), str_c("hlq2_dom_" , 1:4), "eheals_score"),
                         labels = c(str_c("HLQ Part 1 - Domain " , 1:5), str_c("HLQ Part 2 - Domain " , 1:4), "eHEALS")))

# IHF frequence of information source item-level summaries
data_freq_item <- data_ihf %>%
  select(matches("^ihfinfo_q"), weight)
# Creat survey objects - for weighted tabulation
svy_freq_item_ihf <-
  survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data_freq_item
  )

# Plot of raw HLQ item response distributions - weighted
fig2_hlq_item_dist <- ggplot(data_hlq_item_long, aes(x = response, weight = weight, fill = section_num)) +
  geom_bar(aes(y = after_stat(count/dim(data_ihf)[1]))) +
  facet_wrap(~item) +
  xlab("HLQ response") + 
  scale_fill_discrete(name = "Section") +
  scale_y_continuous(name = "Percent responses", labels = scales::label_percent(accuracy = 1L)) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.05))

# Plot of raw eHEALS item response distributions - weighted
fig3_eheals_item_dist <- ggplot(data_eheals_item_long, aes(x = response, weight = weight)) +
  geom_bar(aes(y = after_stat(count/dim(data_ihf)[1]))) +
  facet_wrap(~item) +
  xlab("eHEALS response") + 
  scale_y_continuous(name = "Percent responses", labels = scales::label_percent(accuracy = 1L)) +
  theme_minimal()

# Plot of raw eHEALS item response distributions - weighted
fig4_ihf_item_dist <- ggplot(data_ihf_item_long, aes(x = response, weight = weight)) +
  geom_bar(aes(y = after_stat(count/dim(data_ihf)[1]))) +
  facet_wrap(~item, 
             labeller = as_labeller(\(x) label_wrap_gen(width = 40)(sub("^iHF ", "", unlist(var_label(data_ihf))[x])))) +
  xlab("Frequency") + 
  scale_y_continuous(name = "Percent responses", labels = scales::label_percent(accuracy = 1L)) +
  ggtitle("How often do you use the following sources to get health information?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Plot domain/instrument scores for IHF and eHEALS - weighted
fig5a_hlq_domain_dist <- data_domain_long %>% filter(domain != "eHEALS") %>% 
  ggplot(aes(x = response, weight = weight)) +
  geom_histogram(aes(y = after_stat(count/dim(data_ihf)[1])), bins = 20) +
  facet_wrap(~domain) +
  xlab("Score") + 
  scale_y_continuous(name = "Percent responses", labels = scales::label_percent(accuracy = 1L)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fig5b_eheals_dist <- data_domain_long %>% filter(domain == "eHEALS") %>% 
  ggplot(aes(x = response, weight = weight)) +
  geom_histogram(aes(y = after_stat(count/dim(data_ihf)[1])), bins = 20) +
  facet_wrap(~domain) +
  xlab("Score") + 
  scale_y_continuous(name = "Percent responses", labels = scales::label_percent(accuracy = 1L)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fig5_domain_dist <- (fig5a_hlq_domain_dist / fig5b_eheals_dist ) + plot_layout(heights = c(2, 1))


# Tabulate domain scores for HLQ and eHEALS - weighted
tbl_domains_unweighted <- tbl_summary(data_domain,
                                      type = all_continuous() ~ "continuous2",
                                      statistic = all_continuous() ~ c(
                                        "{mean} ({sd})",
                                        "{median} [{p25}, {p75}]"),
                                      include = -weight)
tbl_domains_weighted <- tbl_svysummary(svy_domain_ihf,
               type = all_continuous() ~ "continuous2",
               statistic = list(all_continuous() ~ c(
                 "{mean} ({sd})",
                 "{median} [{p25}, {p75}]"),
                 all_categorical() ~ "{p}%"),
               include = -weight)

tbl4b_domains <- tbl_merge(
  tbls = list(tbl_domains_unweighted, tbl_domains_weighted),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>%
  modify_header(label = "**Variables**")


# Tabulate frequency of health information sources - weighted
tbl_freq_unweighted <- tbl_summary(data_freq_item,
                                   type = all_continuous() ~ "continuous2",
                                   statistic = all_continuous() ~ c(
                                     "{mean} ({sd})",
                                     "{median} [{p25}, {p75}]"),
                                   include = -weight)
tbl_freq_weighted <- tbl_svysummary(svy_freq_item_ihf,
                                    type = all_continuous() ~ "continuous2",
                                    statistic = list(all_continuous() ~ c(
                                      "{mean} ({sd})",
                                      "{median} [{p25}, {p75}]"),
                                      all_categorical() ~ "{p}%"),
                                    include = -weight)


tbl4a_freq <- tbl_merge(
  tbls = list(tbl_freq_unweighted, tbl_freq_weighted),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>%
  modify_header(label = "**Variables**")



# Save outputs for inclusion in report
save(fig2_hlq_item_dist, fig3_eheals_item_dist, fig4_ihf_item_dist, fig5_domain_dist, 
     tbl4b_domains, tbl4a_freq,
     file = "outcome_summaries.RData")
