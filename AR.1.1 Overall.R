
# ======================================================
# Summary of Country-Led Examples (Figure 4 in New Version of AR)
# ======================================================

summary_table <- group_roster %>%
  group_by(ryear, g_conled, PRO09) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = count, values_fill = 0) %>%
  arrange(g_conled, PRO09)

# Convert PRO09 to numeric for correct calculations
summary_table <- summary_table %>%
  mutate(PRO09 = as.numeric(PRO09))

# Assign labels before suppressing g_conled and PRO09 in display
summary_table <- summary_table %>%
  mutate(
    `Example Lead/Placement` = case_when(
      g_conled == 1 ~ "Country-Led Examples",
      g_conled == 2 ~ "Institution-Led Examples",
      g_conled == 3 ~ "CSO-Led Examples",
      g_conled == 8 ~ "Unknown",
      TRUE ~ ""
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using EGRISS Recommendations",
      PRO09 == 2 ~ "Not Using EGRISS Recommendations",
      PRO09 == 8 ~ "Don't Know if EGRISS Recommendations Used",
      is.na(PRO09) ~ "Not reported Use of EGRISS Recommendations",
      TRUE ~ ""
    )
  ) 

summary_table$`Example Lead/Placement` <- ifelse(duplicated(summary_table$`Example Lead/Placement`), "", summary_table$`Example Lead/Placement`)

numeric_cols <- summary_table %>%  select(where(is.numeric)) %>%names()

# Overall Country-led Example Using Recommendations
overall_country_led_using_recs <- summary_table %>%
  filter(g_conled == 1 & PRO09 == 1) %>%
  summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
  mutate(`Example Lead/Placement` = "Overall Examples", `Use of Recommendations` = "Overall Country-Led Examples Using Recommendations")

# Overall Country-led Example (Now including NA values in PRO09)
overall_country_led <- summary_table %>%
  filter(g_conled == 1 & (PRO09 %in% c(1, 2, 8) | is.na(PRO09))) %>%  # Include NA
  summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
  mutate(`Example Lead/Placement` = "Overall Examples", `Use of Recommendations` = "Overall Country-Led Examples")

# Overall Institution Example (Including NA values in PRO09)
overall_institution_example <- summary_table %>%
  filter(g_conled %in% c(2, 3) & (PRO09 %in% c(1, 2, 8) | is.na(PRO09))) %>%  # Include NA
  summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
  mutate(`Example Lead/Placement` = "Overall Examples", `Use of Recommendations` = "Overall Institution-Led Examples")

# Institution Example Using Recommendations
institution_example_using_recs <- summary_table %>%
  filter(g_conled %in% c(2, 3, 8) & PRO09 == 1) %>%
  summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
  mutate(`Example Lead/Placement` = "Overall Examples", `Use of Recommendations` = "Institution-Led Examples Using Recommendations")

# Combine Graph Data Into a Separate Table
graph_data_table <- bind_rows(
  overall_country_led_using_recs,
  overall_country_led,
  overall_institution_example,
  institution_example_using_recs
)

# Ensure "Graph Data" only appears once
graph_data_table$`Example Lead/Placement` <- ifelse(duplicated(graph_data_table$`Example Lead/Placement`), "", graph_data_table$`Example Lead/Placement`)

# Reorder columns to keep "Example Lead" and "Use of Recommendations" first
graph_data_table <- graph_data_table %>%
  select(`Example Lead/Placement`, `Use of Recommendations`, everything())
  
summary_table <- summary_table %>%
  select(`Example Lead/Placement`, `Use of Recommendations`, everything())

# Create Flextable for Graph Data (Color Rows and Fully Hide g_conled & PRO09)

figure_graph_data <- flextable(graph_data_table) %>%
  set_header_labels(`Example Lead/Placement` = "Example Lead/Placement", `Use of Recommendations` = "Use of Recommendations") %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  autofit() %>%
  color(j = c("g_conled", "PRO09"), color = "#4cc3c9") %>%  # Hide values by matching background
  bg(i = 1:2, bg = "#3b71b3", part = "body") %>%  # First two rows dark blue
  bg(i = 3:4, bg = "#4cc3c9", part = "body")  # Next two rows light blue

# Create Flextable for Summary Table (Fully Hide g_conled & PRO09)

ar.1.1_no_header <- flextable(summary_table) %>%
  set_header_labels(`Example Lead/Placement` = "Example Lead/Placement", `Use of Recommendations` = "Use of Recommendations") %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#3b71b3") %>%
  autofit() %>%
  color(j = c("g_conled", "PRO09"), color = "white") %>%  # Hide values
  delete_part(part = "header")  # Remove header from second table

# Merge Graph Data Table and Summary Table

merged_df <- rbind(graph_data_table, summary_table)

# Summary of Country-Led Examples (Figure 6)

# Ensure Both Tables Have the Same Columns Before Merging
all_columns <- union(colnames(graph_data_table), colnames(summary_table))

graph_data_table <- graph_data_table %>%
  select(all_of(all_columns))

summary_table <- summary_table %>%
  select(all_of(all_columns))

# Merge Graph Data Table and Summary Table
merged_df <- bind_rows(graph_data_table, summary_table) %>%
  mutate(`Example Lead/Placement` = na_if(trimws(`Example Lead/Placement`), "")) %>%  # convert "" or " " to NA
  fill(`Example Lead/Placement`, .direction = "down") %>%
  mutate(Total = rowSums(across(c("2021", "2022", "2023", "2024")), na.rm = TRUE))

# Define Colors
primary_color <- "#4cc3c9"  # Light blue
secondary_color <- "#3b71b3"  # Dark blue

# Summary of Country-Led Examples
# Create Merged Flextable with Enhanced Formatting
ar.1.1 <- flextable(merged_df) %>%
  set_header_labels(
    `Example Lead/Placement` = "Example Lead/Placement",
    `Use of Recommendations` = "Use of Recommendations"
  ) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = primary_color) %>%
  autofit() %>% 
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Added outer border
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  delete_columns(j = c("g_conled", "PRO09")) %>%  # Remove g_conled & PRO09
  color(i = 1:2, color = secondary_color, part = "body") %>%  
  color(i = 3:4, color = primary_color, part = "body") %>%  
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  merge_v(j = ~ `Example Lead/Placement`) %>%
  add_footer_row(
    values = paste0(
      "Table 1.1 supports Figure 4 in the 2024 Annual Report (replicated above). In addition to Figure 4 data, disaggregated examples by example lead (country-led or institution-led, both international or CSO), respondents use of the International Recommendations developed by EGRISS and year of reporting in GAIN."
    ),
    colwidths = ncol(merged_df) - 2
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  bg(bg = "#f4cccc", j = ~ `2024`) %>%
  bg(bg = "#c9daf8", j = ~ Total) %>%
  ## <-- styled caption as a formatted paragraph: 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1.1: Trend of IRRS, IRIS and IROSS implementation examples, by year",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          bold = TRUE,
          italic      = FALSE
        )
      )
    )
  )%>%
  set_table_properties(
    width = 1,           # 100% of page width
    layout = "autofit"   # auto‐adjust column widths
  ) %>%
  fix_border_issues()

# Display Merged Table
ar.1.1
