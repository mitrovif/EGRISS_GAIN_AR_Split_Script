
# ============================================================================================================
# AR.2.2: Figure 6: Implementation of the Recommendations by Region (in new version of AR)
# ============================================================================================================

# Step 1: Extract Country-led Examples Using Recommendations
regional_data_using_recs <- group_roster %>%
  filter(PRO09 == 1, g_conled == 1) %>%
  group_by(region, ryear) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = count, values_fill = 0) %>%
  mutate(`Example Lead` = "Country-Led Example Using EGRISS Recommendations")

# Step 2: Extract Overall Country-led Examples (Including those without use of recommendations)
regional_data_overall <- group_roster %>%
  filter(g_conled == 1) %>%
  group_by(region, ryear) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = count, values_fill = 0) %>%
  mutate(`Example Lead` = "Overall Country-Led Examples")

# Step 3: Combine both datasets
regional_data_combined <- bind_rows(regional_data_using_recs, regional_data_overall) %>%
  rename(Region = region) %>%
  select(`Example Lead`, Region, everything())  # Ensure correct column order

# Define your region mapping
region_map <- c(
  "Asia" = "Asia and Oceania",
  "Oceania" = "Asia and Oceania",
  "North America" = "Americas",
  "South America" = "Americas"
)

# Apply the mapping and summarize
regional_data_combined <- regional_data_combined %>%
  mutate(Region = recode(Region, !!!region_map)) %>%
  group_by(`Example Lead`, Region) %>%
  summarise(
    `2021` = sum(`2021`, na.rm = TRUE),
    `2022` = sum(`2022`, na.rm = TRUE),
    `2023` = sum(`2023`, na.rm = TRUE),
    `2024` = sum(`2024`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Total = rowSums(select(., starts_with("20")))) # add total column

# Define border styles
highlight_border <- fp_border(color = "#3b71b3", width = 1.5)  # Blue for Graph Data section
default_border <- fp_border(color = "black", width = 1)  # Default black border for Overall section
header_color <- "#4cc3c9"  # Primary color for header row
header_border <- fp_border(color = "black", width = 1)  # Black border for header

# Step 4: Create FlexTable
ar.2.2 <- flextable(regional_data_combined) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%  # Set font size 10 for body text
  merge_v(j = ~ `Example Lead`) %>%  # Merge vertical cells for "Example Lead"
  autofit() %>%
  
  # Header Styling
  bg(part = "header", bg = header_color) %>%
  color(part = "header", color = "black") %>%  # Black font in header for better contrast
  border(part = "header", border.top = header_border, border.bottom = header_border) %>%  # Header Border
  
  # Apply blue border styling for "Country-led Example Using EGRISS Recommendations"
  border(i = which(regional_data_combined$`Example Lead` == "Country-Led Example Using EGRISS Recommendations"), 
         border.top = highlight_border, 
         border.bottom = highlight_border) %>% 
  # border.left = highlight_border, 
  # border.right = highlight_border) %>%
  
  # Apply black border styling for "Overall Country-led Examples"
  border(i = which(regional_data_combined$`Example Lead` == "Overall Country-Led Examples"), 
         border.top = fp_border(color = "gray", width = 0.5), 
         border.bottom = fp_border(color = "gray", width = 0.5)) %>% 
  # border.left = fp_border(color = "gray", width = 0.5), 
  # border.right = fp_border(color = "gray", width = 0.5)) %>%
  
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  
  # Highlight Key Year Columns
  bg(bg = "#f4cccc", j = ~ `2024`) %>%
  bg(bg = "#c9daf8", j = ~ Total) %>%
  
  # Footer details
  add_footer_row(
    values = paste0(
      "Table 2.2 supports Figure 6 in the 2024 Annual Report (replicated above). In addition to Figure 6 data on country-led examples using the EGRISS Recommendations, it shows overall country-led examples irrespective of EGRISS Recommendation use, disaggregated by region and by year of reporting in GAIN."
    ),
    colwidths = ncol(regional_data_combined)  # Ensure footer spans full table width
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.2: Country-led examples of IRRS, IRIS and IROSS implementation, by region",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display the table
ar.2.2
