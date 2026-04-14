
# ======================================================
# AR.1.4. Figure 7 - Step 1: Aggregate PRO08 variables into specified categories and count each source by year
# ======================================================

# Step 1: Prepare the data for National Examples (g_conled == 1)
contr <- group_roster %>%
  group_by(year, across(starts_with("PRO08."))) %>% 
  tally()

contr <- group_roster %>%
  group_by("PRO08.A", ryear, year, g_conled) %>% 
  tally()

summary(group_roster$ryear)

aggregated_national <- group_roster %>%
  filter(g_conled == 1) %>%  
  mutate(across(starts_with("PRO08B."), as.numeric)) %>%  
  pivot_longer(
    cols = starts_with("PRO08B."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>% 
  filter(Value == 1) %>% 
  mutate(
    Source = case_when(
      grepl("PRO08B.A", Source_Variable) ~ "Survey",
      grepl("PRO08B.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08B.C", Source_Variable) ~ "Census",
      grepl("PRO08B.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08B.E", Source_Variable) ~ "Non-traditional",
      grepl("PRO08B.F|PRO08B.G|PRO08B.H|PRO08B.X", Source_Variable) ~ "Tools and Other**",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using EGRISS Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using EGRISS Recommendations and Other*",
      TRUE ~ "Not Using EGRISS Recommendations and Other*"
    )
  ) %>% 
  group_by(`Use of Recommendations`, Source, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., `2021`, `2022`, `2023`, `2024`, `2025`), na.rm = TRUE)) %>%
  mutate(`Example Lead` = "Country-Led Examples")  

# Step 2: Prepare the data for Institutional Examples (g_conled == 2 or g_conled == 3)
aggregated_institutional <- group_roster %>%
  filter(g_conled %in% c(2, 3)) %>%  
  mutate(across(starts_with("PRO08B."), as.integer)) %>%  
  pivot_longer(
    cols = starts_with("PRO08B."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>% 
  filter(Value == 1) %>% 
  mutate(
    Source = case_when(
      grepl("PRO08B.A", Source_Variable) ~ "Survey",
      grepl("PRO08B.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08B.C", Source_Variable) ~ "Census",
      grepl("PRO08B.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08B.E", Source_Variable) ~ "Non-traditional",
      grepl("PRO08B.F|PRO08B.G|PRO08B.H|PRO08B.X", Source_Variable) ~ "Tools and Other**",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using EGRISS Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using EGRISS Recommendations and Other*",
      TRUE ~ "Not Using EGRISS Recommendations and Other*"
    )
  ) %>% 
  group_by(`Use of Recommendations`, Source, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., `2021`, `2022`, `2023`, `2024`, `2025`), na.rm = TRUE)) %>%
  mutate(`Example Lead` = "Institution-Led Examples")  

# Step 3: Combine both datasets
aggregated_data <- bind_rows(aggregated_national, aggregated_institutional) %>%
  mutate(
    `Use of Recommendations` = factor(
      `Use of Recommendations`,
      levels = c("Using EGRISS Recommendations", "Not Using EGRISS Recommendations and Other*")
    )
  ) %>%
  select(`Example Lead`, `Use of Recommendations`, Source, `2021`, `2022`, `2023`, `2024`, `2025`, Total) %>%
  arrange(
    `Example Lead`,
    `Use of Recommendations`,
    factor(Source, levels = c("Survey", "Census", "Administrative Data", "Data Integration", "Non-traditional", "Tools and Other**"))
  )

# Define borders
solid_border <- fp_border(color = "#3b71b3", width = 2, style = "solid")  # For "Using EGRISS Recommendations" (Graph Data)
dashed_border <- fp_border(color = "#3b71b3", width = 2, style = "dashed")  # For "Not Using EGRISS Recommendations and Other*" (Graph Data)
default_border <- fp_border(color = "black", width = 0.5)  # Default border for "Institution-Led Examples"

# Step 4: Beautify and create FlexTable for Word
ar.1.4.a <- flextable(aggregated_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%  # Light blue header
  merge_v(j = ~ `Example Lead`) %>%  
  merge_v(j = ~ `Use of Recommendations`) %>%  
  bg(bg = "#f4cccc", j = ~ `2025`) %>%   
  bg(bg = "#c9daf8", j = ~ Total) %>%   
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%  # Inner borders
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>% 
  
  # **AutoFit for Better Readability**
  autofit() %>%
  
  # Apply colored borders for Graph DatCountry-Led Examples
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using EGRISS Recommendations"), 
         border.top = solid_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using EGRISS Recommendations"), 
         border.bottom = solid_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using EGRISS Recommendations and Other*"), 
         border.top = dashed_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using EGRISS Recommendations and Other*"), 
         border.bottom = dashed_border) %>%
  
  # Reset to default borders for Overall Institution Examples
  border(i = which(aggregated_data$`Example Lead` == "Institution-Led Examples"), 
         border.top = fp_border(color = "gray", width = 0.5)) %>%
  border(i = which(aggregated_data$`Example Lead` == "Institution-Led Examples"), 
         border.bottom = fp_border(color = "gray", width = 0.5)) %>%
  
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  
  # Footer details
  add_footer_row(
    values = paste0(
      "Table 1.4.a supports Figure 7 in the 2025 Annual Report (replicated above). In addition to Figure 7 data focused on country-led examples, table shows institution-led examples, disaggregated by using and not using EGRISS Recommendations (and other), by source and by year of reporting in GAIN.  
*Other in Not Using EGRISS Recommendations include: Don't Know and Not Reported
** Tools and Other in sources include: Strategy, Guidance/Toolkit, Workshop/Training and Other"
    ),
    colwidths = ncol(aggregated_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption("Figure 7: Overview Data Sources and Tools for Country-Led Examples 2024") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1.4.a: Overview of data sources and tools, by year",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display Updated Table
print(ar.1.4.a)


if (bExport == 1){
  save_as_html(ar.1.4.a, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.1.4.a.html") 
}