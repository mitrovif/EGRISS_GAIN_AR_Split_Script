
# ======================================================
# Figure 7 - Step 1: Aggregate PRO08 variables into specified categories and count each source by year
# ======================================================

# Step 1: Prepare the data for National Examples (g_conled == 1)

aggregated_national <- group_roster %>%
  filter(g_conled == 1) %>%  
  mutate(across(starts_with("PRO08."), as.integer)) %>%  
  pivot_longer(
    cols = starts_with("PRO08."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>% 
  filter(Value == 1) %>% 
  mutate(
    Source = case_when(
      grepl("PRO08.A", Source_Variable) ~ "Survey",
      grepl("PRO08.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08.C", Source_Variable) ~ "Census",
      grepl("PRO08.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08.E|PRO08.F|PRO08.G|PRO08.H|PRO08.X", Source_Variable) ~ "Other",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using Recommendations and Other",
      TRUE ~ "Not Using Recommendations and Other"
    )
  ) %>% 
  group_by(`Use of Recommendations`, Source, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., `2021`, `2022`, `2023`, `2024`), na.rm = TRUE)) %>%
  mutate(`Example Category` = "Graph Data National Examples")  

# Step 2: Prepare the data for Institutional Examples (g_conled == 2 or g_conled == 3)

aggregated_institutional <- group_roster %>%
  filter(g_conled %in% c(2, 3)) %>%  
  mutate(across(starts_with("PRO08."), as.integer)) %>%  
  pivot_longer(
    cols = starts_with("PRO08."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>% 
  filter(Value == 1) %>% 
  mutate(
    Source = case_when(
      grepl("PRO08.A", Source_Variable) ~ "Survey",
      grepl("PRO08.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08.C", Source_Variable) ~ "Census",
      grepl("PRO08.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08.E|PRO08.F|PRO08.G|PRO08.H|PRO08.X", Source_Variable) ~ "Other",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using Recommendations and Other",
      TRUE ~ "Not Using Recommendations and Other"
    )
  ) %>% 
  group_by(`Use of Recommendations`, Source, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., `2021`, `2022`, `2023`, `2024`), na.rm = TRUE)) %>%
  mutate(`Example Category` = "Overall Institution Examples")  

# Step 3: Combine both datasets

aggregated_data <- bind_rows(aggregated_national, aggregated_institutional) %>%
  mutate(
    `Use of Recommendations` = factor(
      `Use of Recommendations`,
      levels = c("Using Recommendations", "Not Using Recommendations and Other")
    )
  ) %>%
  select(`Example Category`, `Use of Recommendations`, Source, `2021`, `2022`, `2023`, `2024`, Total) %>%
  arrange(
    `Example Category`,
    `Use of Recommendations`,
    factor(Source, levels = c("Survey", "Census", "Administrative Data", "Data Integration", "Other"))
  )

# Define borders
solid_border <- fp_border(color = "#3b71b3", width = 2, style = "solid")  # For "Using Recommendations" (Graph Data)
dashed_border <- fp_border(color = "#3b71b3", width = 2, style = "dashed")  # For "Not Using Recommendations and Other" (Graph Data)
default_border <- fp_border(color = "black", width = 0.5)  # Default border for "Overall Institution Examples"

# Step 4: Beautify and create FlexTable for Word
figure8_flextable <- flextable(aggregated_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%  # Light blue header
  merge_v(j = ~ `Example Category`) %>%  
  merge_v(j = ~ `Use of Recommendations`) %>%  
  bg(bg = "#f4cccc", j = ~ `2024`) %>%   
  bg(bg = "#c9daf8", j = ~ Total) %>%   
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  border_inner(border = fp_border(color = "gray", width = 0.5)) %>%  # Inner borders
  fontsize(size = 8) %>%
  
  # **AutoFit for Better Readability**
  autofit() %>%
  
  # Apply colored borders for Graph Data National Examples
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using Recommendations"), 
         border.top = solid_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using Recommendations"), 
         border.bottom = solid_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using Recommendations and Other"), 
         border.top = dashed_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using Recommendations and Other"), 
         border.bottom = dashed_border) %>%
  
  # Reset to default borders for Overall Institution Examples
  border(i = which(aggregated_data$`Example Category` == "Overall Institution Examples"), 
         border.top = fp_border(color = "gray", width = 0.5)) %>%
  border(i = which(aggregated_data$`Example Category` == "Overall Institution Examples"), 
         border.bottom = fp_border(color = "gray", width = 0.5)) %>%
  
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  
  # Footer details
  add_footer_row(
    values = paste0(
      "Footnote: Graph Data National Examples are based on the implementation of statistical frameworks (IRRS, IRIS, IROSS) in 2024. ",
      "Nationally and institutionally led examples are categorized by the type of data source used. ",
      "• Survey: Data collected through sample surveys. ",
      "• Census: Information obtained through national population censuses. ",
      "• Administrative Data: Official government records and databases. ",
      "• Data Integration: Combination of multiple sources. ",
      "• Other: Sum of responses to PRO08.F, PRO08.G, PRO08.H, and PRO08.X. ",
      "  This is a multiple-response question, meaning one example can feature multiple sources or tools."
    ),
    colwidths = ncol(aggregated_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption("Figure 7: Overview Data Sources and Tools for Country-led Examples 2024")

# Display Updated Table
print(figure8_flextable) #used to be figure 7 so maybe confusing
