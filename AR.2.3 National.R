# ============================================================================================================
# Figure XX (text in AR): Components of EGRISS Recommendations Most Frequently Used
# ============================================================================================================

# Step 1: Load the dataset
file_path <- file.path(working_dir, "analysis_ready_repeat_PRO11_PRO12.csv")
repeat_data <- read.csv(file_path, stringsAsFactors = FALSE)  

# Step 2: Rename `_recommendation` to `recommendation` and clean values
repeat_data <- repeat_data %>%
  rename(recommendation = X_recommendation) %>%
  mutate(recommendation = as.character(recommendation)) %>%
  mutate(recommendation = na_if(trimws(recommendation), ""))



# ✅ Convert all PRO12 columns to numeric before pivoting
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
nationally_led_data <- summarize_table(processed_data, 1)
institutionally_led_data <- summarize_table(processed_data, 2)


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

# Create FlexTable with Enhanced Formatting
merged_flextable <- flextable(merged_table) %>%
  add_header_row(values = c("", "Nationally Led Examples", "Institutionally Led Examples"), 
                 colwidths = c(1, 3, 3)) %>%
  set_header_labels(
    Category = "Elements of International Recommendations Used",  # Updated name
    
    Category = "Elements of International Recommendations Used",
    IRRS_National = "IRRS",
    IRIS_National = "IRIS",
    IROSS_National = "IROSS",
    IRRS_Institutional = "IRRS",
    IRIS_Institutional = "IRIS",
    IROSS_Institutional = "IROSS"
  ) %>%
  
  # Default Outer Border for Entire Table
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  
  # Light Blue Header (First Two Rows)
  bg(i = 1:2, part = "header", bg = primary_color) %>%
  color(i = 1:2, part = "header", color = "black") %>%
  bold(i = 1:2, part = "header") %>%
  
  # Red Outer Border for "Statistical framework/population group" (NO INNER VERTICAL BORDERS)
  
  # Red Outer Border for "Statistical framework/population group"
  border(i = which(merged_table$Category == "Statistical framework/population group"),
         border.top = fp_border(color = highlight_red, width = 2),
         border.bottom = fp_border(color = highlight_red, width = 2),
         border.left = fp_border(color = "transparent", width = 0),
         border.right = fp_border(color = "transparent", width = 0)) %>%
  
  # AutoFit for Optimal Sizing
  set_table_properties(width = 0.5, layout = "autofit") %>%
  
  # Improved User-Friendly Footnote
  add_footer_row(
    values = paste0(
      "Footnote: This table shows which components of EGRISS recommendations are most frequently used. ",
      "Each row represents an 'Element of International Recommendations' applied in data collection. ",
      "The highlighted row with a **red border** marks the foundational element: 'Statistical framework/population group'. ",
      "Columns show counts for data collected through Nationally Led Examples and Institutionally Led Examples. ",
      "Values are based on reported implementation under PRO11/PRO12 variables."
    ),
    colwidths = ncol(merged_table)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  
  # Updated Caption
  set_caption("Figure XX (text in AR): Components of EGRISS Recommendations Most Frequently Used")

# Display the Final Table
print(merged_flextable)