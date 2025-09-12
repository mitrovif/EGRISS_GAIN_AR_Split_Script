
# ============================================================================================================
# AR.2.4: (text in AR) Components of EGRISS Recommendations Most Frequently Used
# ============================================================================================================

# Step 1: Load the dataset
file_path <- file.path(working_dir, "analysis_ready_repeat_PRO11_PRO12.csv")
repeat_data <- read.csv(file_path, stringsAsFactors = FALSE)  

# Step 2: Rename `_recommendation` to `recommendation` and clean values
repeat_data <- repeat_data %>%
  rename(recommendation = X_recommendation) %>%
  mutate(recommendation = as.character(recommendation)) %>%
  mutate(recommendation = na_if(trimws(recommendation), ""))

# Convert all PRO12 columns to numeric before pivoting
pro12_columns <- grep("^PRO12[A-ZX]", names(repeat_data), value = TRUE)
repeat_data <- repeat_data %>%
  mutate(across(all_of(pro12_columns), ~ as.numeric(.)))

# Step 3: Convert to long format, classify categories, and aggregate
processed_data <- repeat_data %>%
  pivot_longer(
    cols = all_of(pro12_columns),
    names_to = "Category_Variable",
    values_to = "Value"
  ) %>%
  
  filter(Value == 1) %>%
  
  mutate(
    Category = case_when(
      Category_Variable == "PRO12A" ~ "Statistical framework/population group",
      Category_Variable == "PRO12B" ~ "Recommendations on data sources",
      Category_Variable == "PRO12C" ~ "Coordination",
      Category_Variable == "PRO12D" ~ "Data sharing",
      Category_Variable == "PRO12E" ~ "Analysis",
      Category_Variable == "PRO12F" ~ "Indicator selection",
      Category_Variable == "PRO12G" ~ "Data integration",
      Category_Variable == "PRO12H" ~ "Dissemination",
      Category_Variable == "PRO12I" ~ "Institutional or sectoral strategy",
      Category_Variable == "PRO12X" ~ "Other (specify)",
      Category_Variable == "PRO12Z" ~ "Don't know",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Category))  

# Summarize Counts by Category and Recommendation
summarize_table <- function(data, g_conled_value) {
  data %>%
    filter(g_conled == g_conled_value) %>%
    count(Category, recommendation) %>%
    pivot_wider(names_from = recommendation, values_from = n, values_fill = 0)  
}

# Create separate tables for Nationally and Institutionally Led data
nationally_led_data <- summarize_table(processed_data, 1) %>%
  mutate(Total = rowSums(across(c(IRIS, IRRS, IROSS)), na.rm = TRUE))

institutionally_led_data <- summarize_table(processed_data, 2) %>%
  mutate(Total = rowSums(across(c(IRIS, IRRS, IROSS)), na.rm = TRUE))

# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
highlight_red <- "#D73027"  # Correct EGRISS red for highlighting
background_color <- "#f0f8ff"

# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
highlight_red <- "#D73027"  # Correct EGRISS red for highlighting
background_color <- "#f0f8ff"

# Step 4: Merge Nationally and Institutionally Led Data
merged_table <- nationally_led_data %>%
  full_join(institutionally_led_data, by = "Category", suffix = c("_National", "_Institutional"))

highlight_row <- which(merged_table$Category == "Statistical framework/population group")

# Create FlexTable with Enhanced Formatting
ar.2.4 <- flextable(merged_table) %>%
  add_header_row(values = c("", "Country-Led Examples", "Institution-Led Examples"), 
                 colwidths = c(1, 4, 4)) %>%
  set_header_labels(
    Category = "Elements of EGRISS Recommendations Used",
    IRRS_National = "IRRS",
    IRIS_National = "IRIS",
    IROSS_National = "IROSS",
    IRRS_Institutional = "IRRS",
    IRIS_Institutional = "IRIS",
    IROSS_Institutional = "IROSS"
  ) %>%
  bg(i = 1:2, part = "header", bg = primary_color) %>%
  color(i = 1:2, part = "header", color = "black") %>%
  bold(i = 1:2, part = "header") %>%
  border(i = 1, part = "header", border.bottom = fp_border(color = "black", width = 2)) %>%
  
  # Default Outer Border for Entire Table
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  fix_border_issues() %>%
  
  # Inner Borders for Entire Table
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  
  # Highlight total columns
  bg(bg = "#c9daf8", j = ~ Total_National) %>%   # Highlight the Total column
  bg(bg = "#c9daf8", j = ~ Total_Institutional) %>%   # Highlight the Total column
  
  # Rename total columns
  set_header_labels(
    Category = "Elements of EGRISS Recommendations Used",
    Total_National = "Total",
    Total_Institutional = "Total"
  ) %>%
  
  # Red Outer Border for "Statistical framework/population group"
  border(i = highlight_row, j = 1,
         border.left = fp_border(color = highlight_red, width = 2)) %>%
  border(i = highlight_row, j = ncol(merged_table),
         border.right = fp_border(color = highlight_red, width = 2)) %>%
  border(i = highlight_row,
         border.top = fp_border(color = highlight_red, width = 2),
         border.bottom = fp_border(color = highlight_red, width = 2)) %>%
  
  # Define text size
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>% 
  
  # AutoFit for Optimal Sizing
  set_table_properties(width = 0.5, layout = "autofit") %>%
  
  # Improved User-Friendly Footnote
  add_footer_row(
    values = paste0(
      "Table 2.4 supports analysis on page 27 in the 2024 Annual Report, disaggregated by elements of the EGRISS Recommendations used and by example lead (country-led or institution-led, both international or CSO)."
    ),
    colwidths = ncol(merged_table)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  
  # Updated Caption
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.4: Components of IRRS, IRIS and IROSS recommendations most frequently used, by recommendation and by example lead",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display the Final Table
print(ar.2.4)
