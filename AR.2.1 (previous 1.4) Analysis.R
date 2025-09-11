
# ============================================================================================================
# AR.2.1: Unique Country Count for Use of Recommendations by Leadership Type
# ============================================================================================================

# EGRISS Color Scheme
primary_color <- "#4cc3c9"   # Light Blue Header
secondary_color <- "#3b71b3" # Dark Blue for Total Row
accent_color <- "#072d62"
highlight_red <- "#D73027"
background_color <- "#f0f8ff"

# Function to calculate unique country counts by `g_conled`
calculate_unique_country_count <- function(group_roster, leadership_type) {
  df <- group_roster %>%
    filter(PRO09 == 1, g_conled == leadership_type) %>%
    group_by(region, ryear) %>%
    summarise(unique_countries = n_distinct(mcountry), .groups = "drop") %>%
    pivot_wider(names_from = ryear, values_from = unique_countries, values_fill = 0)
  
  # Ensure all year columns exist
  for (year in c("2021", "2022", "2023", "2024")) {
    if (!(year %in% colnames(df))) {
      df[[year]] <- 0
    }
  }
  
  df <- df %>%
    mutate(Total = rowSums(across(c("2021", "2022", "2023", "2024")), na.rm = TRUE)) %>%
    mutate(`Example Lead` = if_else(leadership_type == 1, "Country-Led Examples", "Institutional Examples")) %>%
    relocate(`Example Lead`, .before = everything())
  
  return(df)
}

# Function to list countries by region
list_countries_by_region <- function(group_roster) {
  df <- group_roster %>%
    filter(PRO09 == 1) %>%
    group_by(region, ryear) %>%
    summarise(countries = paste(unique(mcountry), collapse = ", ")) %>%
    arrange(region, desc(ryear)) %>%
    pivot_wider(names_from = region, values_from = countries, values_fill = "") %>%
    mutate(`Year` = as.character(ryear)) %>%
    relocate(`Year`, .before = everything())
  
  return(df)
}

# Calculate unique country counts for Country-Led examples
nationally_led_count <- calculate_unique_country_count(group_roster, 1) %>%
  rename(Region = region)
# institutionally_led_count <- calculate_unique_country_count(group_roster, 2)

# Combine both tables
# combined_unique_country_count <- bind_rows(nationally_led_count, institutionally_led_count) %>%
#   rename(Region = region)

# Define your region mapping
region_map <- c(
  "Asia" = "Asia and Oceania",
  "Oceania" = "Asia and Oceania",
  "North America" = "Americas",
  "South America" = "Americas"
)

# Apply the mapping and summarize
nationally_led_count <- nationally_led_count %>%
  mutate(Region = recode(Region, !!!region_map)) %>%
  group_by(`Example Lead`, Region) %>%
  summarise(
    `2021` = sum(`2021`, na.rm = TRUE),
    `2022` = sum(`2022`, na.rm = TRUE),
    `2023` = sum(`2023`, na.rm = TRUE),
    `2024` = sum(`2024`, na.rm = TRUE),
    Total  = sum(Total, na.rm = TRUE),
    .groups = "drop"
  )

# Add Total Row
total_unique_summary <- group_roster %>%
  filter(PRO09 == 1, g_conled == 1) %>%
  group_by(ryear) %>%
  summarise(unique_countries = n_distinct(mcountry), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = unique_countries, values_fill = 0)

# Ensure all year columns exist in the summary
for (year in c("2021", "2022", "2023", "2024")) {
  if (!(year %in% colnames(total_unique_summary))) {
    total_unique_summary[[year]] <- 0
  }
}

total_unique_summary <- total_unique_summary %>%
  mutate(Total = rowSums(across(c("2021", "2022", "2023", "2024")), na.rm = TRUE)) %>%
  mutate(`Example Lead` = "Total") %>%
  relocate(`Example Lead`, .before = everything())

# Final table with combined counts and summary
final_unique_country_table <- bind_rows(nationally_led_count, total_unique_summary)

# Beautify and create FlexTable for Word
ar.2.1 <- flextable(final_unique_country_table) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  
  # Highlight Header Row in Light Blue
  bg(part = "header", bg = primary_color) %>%
  color(part = "header", color = "black") %>%
  
  # Highlight Key Year Columns
  bg(bg = "#f4cccc", j = ~ `2024`) %>%
  bg(bg = "#c9daf8", j = ~ Total) %>%
  
  # Highlight Total Row in Dark Blue
  bg(i = nrow(final_unique_country_table), bg = secondary_color) %>%
  color(i = nrow(final_unique_country_table), color = "white") %>%
  
  merge_v(j = ~ `Example Lead`) %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%
  
  # AutoFit for Optimal Sizing
  set_table_properties(width = 0.5, layout = "autofit") %>%
  
  # Set font size
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  
  # Add Metadata Summary in Footnote
  add_footer_row(
    values = paste0(
      "Table 2.1 is not in the 2024 Annual Report. The table is disaggregated by example lead (country-led or institution-led, both international or CSO), by region and by year."
    ),
    colwidths = ncol(final_unique_country_table)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  
  # Updated Caption
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.1: Count of countries reporting IRRS, IRIS and IROSS implementation examples, by region and year",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display Table
ar.2.1
