
# =============================================================================================================
# AR.2.3: Generate Institutional Implementation breakdown table - by implementation level
# =============================================================================================================

# institutional_implementation_table_level <- group_roster %>%
#   filter(g_conled == 2 | g_conled == 3) %>%
#   mutate(
#     Use_of_Recommendations = case_when(
#       PRO09 == 1 ~ "Institution-Led Examples Using EGRISS Recommendations",
#       PRO09 == 2 ~ "Institution-Led Examples Not Using EGRISS Recommendations",
#       PRO09 == 8 ~ "No Response",
#       TRUE ~ "No Response"
#     ),
#     Implementation_Level = case_when(
#       PRO03B == 1 ~ "Global",
#       PRO03B == 2 ~ "Regional",
#       PRO03B == 3 ~ "Country",
#       PRO03B == 8 ~ "Undetermined",
#       TRUE ~ "Undetermined"
#     )
#   ) %>%
#   group_by(Use_of_Recommendations, Implementation_Level, ryear) %>%
#   summarise(Total_Examples = n(), .groups = "drop") %>%
#   pivot_wider(names_from = ryear, values_from = Total_Examples, values_fill = 0) %>%
#   select(c("Use_of_Recommendations", "Implementation_Level", "2021", "2022", "2023", "2024")) %>%
#   rowwise() %>%
#   mutate(Total = sum(c_across(`2021`:`2024`), na.rm = TRUE)) %>%
#   ungroup() %>%
#   arrange(
#     factor(Use_of_Recommendations, levels = c("Institution-Led Examples Using EGRISS Recommendations", 
#                                               "Institution-Led Examples Not Using EGRISS Recommendations")),
#     factor(Implementation_Level, levels = c("Global", "Regional", "Country", "Undetermined"))
#   ) %>%
#   select(`Use of Recommendations` = Use_of_Recommendations, `Example Level` = Implementation_Level, `2021`, `2022`, `2023`, `2024`, Total)  # Ensure correct column order

institutional_implementation_table_level <- group_roster %>%
  filter(g_conled == 2 | g_conled == 3) %>%
  mutate(
    Use_of_Recommendations = case_when(
      PRO09 == 1 ~ "Institution-Led Examples Using EGRISS Recommendations",
      PRO09 == 2 ~ "Institution-Led Examples Not Using EGRISS Recommendations",
      PRO09 == 8 ~ "No Response",
      TRUE ~ "No Response"
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
  ungroup()

# Add totals per Use_of_Recommendations 
totals <- institutional_implementation_table_level %>%
  group_by(Use_of_Recommendations) %>%
  summarise(across(c(`2021`:`2024`, Total), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(Implementation_Level = "Total")

# Bind and arrange
institutional_implementation_table_level <- institutional_implementation_table_level %>%
  bind_rows(totals) %>%
  arrange(
    factor(Use_of_Recommendations, levels = c("Institution-Led Examples Using EGRISS Recommendations", 
                                              "Institution-Led Examples Not Using EGRISS Recommendations")),
    factor(Implementation_Level, levels = c("Global", "Regional", "Country", "Undetermined", "Total"))
  ) %>%
  select(`Use of Recommendations` = Use_of_Recommendations, 
         `Example Level` = Implementation_Level, 
         `2021`, `2022`, `2023`, `2024`, Total)

# Calculate the totals for "Global", "Regional", and "Country"
summary_rows <- institutional_implementation_table_level %>%
  filter(`Example Level` %in% c("Global", "Regional", "Country")) %>%
  group_by(`Example Level`) %>%
  summarise(across(`2021`:`2024`, sum, na.rm = TRUE), Total = sum(Total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`Use of Recommendations` = "Overall Institution-Led Examples") %>%
  arrange(factor(`Example Level`, levels = c("Global", "Regional", "Country"))) %>%
  
# add overall total row
bind_rows(
  .,
  summarise(., across(`2021`:`2024`, sum, na.rm = TRUE),
            Total = sum(Total, na.rm = TRUE)) %>%
    mutate(`Use of Recommendations` = "Overall Institution-Led Examples",
           `Example Level` = "Total")
) %>%
  
  select(c("Use of Recommendations", "Example Level", "2021", "2022", "2023", "2024", "Total"))


# Add the summary rows at the top of the original table
institutional_implementation_table_level <- bind_rows(summary_rows, institutional_implementation_table_level)

# Beautify and create FlexTable for Word
ar.2.3 <- flextable(institutional_implementation_table_level) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = "#f4cccc", j = ~ `2024`) %>%   # Highlight the 2024 column
  bg(bg = "#c9daf8", j = ~ Total) %>%   # Highlight the Total column
  merge_v(j = ~ `Use of Recommendations`) %>%  # Merge vertical cells for Use_of_Recommendations
  merge_v(j = ~ `Example Level`) %>%  # Merge vertical cells for Source
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  # border_inner_v(border = fp_border(color = "gray", width = 0.5), part = "body") %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  bg(i = 4, bg = "#3B71B3") %>%
  bg(i = 8, bg = "#3B71B3") %>%
  bg(i = 12, bg = "#3B71B3") %>%
  bg(i = 15, bg = "#3B71B3") %>%
  color(i = 4, color = "white", part = "body") %>%
  color(i = 8, color = "white", part = "body") %>%
  color(i = 12, color = "white", part = "body") %>%
  color(i = 15, color = "white", part = "body") %>%
  fontsize(size = 10, part = "all") %>%  # Set font size
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 2.3 supports analysis on page 49 in the 2024 Annual Report. Table is disaggregated by level of example implementation (global, regional or national) and year."
    ),
    colwidths = ncol(institutional_implementation_table_level)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.3: Institution-led examples, by level of implementation",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

ar.2.3             
