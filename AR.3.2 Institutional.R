# =============================================================================================================
# Generate Institutional Implementation breakdown table - by implementation level
# =============================================================================================================

institutional_implementation_table_level <- group_roster %>%
  filter(g_conled == 2 | g_conled == 3) %>%
  mutate(
    Use_of_Recommendations = case_when(
      PRO09 == 1 ~ "Using Recommendations",
      PRO09 == 2 ~ "Not Using Recommendations",
      PRO09 == 8 ~ "Undetermined",
      TRUE ~ "Undetermined"
    ),
    Implementation_Level = case_when(
      PRO03B == 1 ~ "Global",
      PRO03B == 2 ~ "Regional",
      PRO03B == 3 ~ "Country",
      PRO03B == 8 ~ "Undetermined",
      TRUE ~ "Undetermined"
    )
  ) %>%
  group_by(Use_of_Recommendations, Implementation_Level, ryear) %>%
  summarise(Total_Examples = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = Total_Examples, values_fill = 0) %>%
  select(c("Use_of_Recommendations", "Implementation_Level", "2021", "2022", "2023", "2024")) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(`2021`:`2024`), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(
    factor(Use_of_Recommendations, levels = c("Using Recommendations", "Not Using Recommendations", "Undetermined")),
    factor(Implementation_Level, levels = c("Global", "Regional", "Country", "Undetermined"))
  ) %>%
  select(`Use of Recommendations` = Use_of_Recommendations, `Implementation Level` = Implementation_Level, `2021`, `2022`, `2023`, `2024`, Total)  # Ensure correct column order

# Calculate the totals for "Global", "Regional", and "Country"
summary_rows <- institutional_implementation_table_level %>%
  filter(`Implementation Level` %in% c("Global", "Regional", "Country")) %>%
  group_by(`Implementation Level`) %>%
  summarise(across(`2021`:`2024`, sum, na.rm = TRUE), Total = sum(Total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`Use of Recommendations` = "Total") %>%  # Add the 'Total' for Use_of_Recommendations
  arrange(
    factor(`Implementation Level`, levels = c("Global", "Regional", "Country"))
  ) %>% 
  select(c("Use of Recommendations", "Implementation Level", "2021", "2022", "2023", "2024", "Total"))

# Add the summary rows at the top of the original table
institutional_implementation_table_level <- bind_rows(summary_rows, institutional_implementation_table_level)

# Beautify and create FlexTable for Word
institutional_level_flextable <- flextable(institutional_implementation_table_level) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = "#f4cccc", j = ~ `2024`) %>%   # Highlight the 2024 column
  bg(bg = "#c9daf8", j = ~ Total) %>%   # Highlight the Total column
  merge_v(j = ~ `Use of Recommendations`) %>%  # Merge vertical cells for Use_of_Recommendations
  merge_v(j = ~ `Implementation Level`) %>%  # Merge vertical cells for Source
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_v(border = fp_border(color = "gray", width = 0.5), part = "body") %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  autofit() %>%
  add_footer_lines(values = "Source: GAIN 2024 Data") %>%
  set_caption(caption = "Institutional Implementation Breakdown by Implementation Level")

institutional_level_flextable                        
