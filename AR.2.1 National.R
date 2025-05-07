
# ============================================================================================================
# AR.2.1: Figure 6: Implementation of the Recommendations by Region (in new version of AR)
# ============================================================================================================

# Step 1: Extract Country-led Examples Using Recommendations
regional_data_using_recs <- group_roster %>%
  filter(PRO09 == 1, g_conled == 1) %>%
  group_by(region, ryear) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = count, values_fill = 0) %>%
  mutate(`Example Category` = "Graph Data: Country-led Example Using Recommendations")

# Step 2: Extract Overall Country-led Examples (Including those without use of recommendations)
regional_data_overall <- group_roster %>%
  filter(g_conled == 1) %>%
  group_by(region, ryear) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = count, values_fill = 0) %>%
  mutate(`Example Category` = "Overall Country-led Example")

# Step 3: Combine both datasets
regional_data_combined <- bind_rows(regional_data_using_recs, regional_data_overall) %>%
  rename(Region = region) %>%
  select(`Example Category`, Region, everything())  # Ensure correct column order

# Define border styles
highlight_border <- fp_border(color = "#3b71b3", width = 1.5)  # Blue for Graph Data section
default_border <- fp_border(color = "black", width = 1)  # Default black border for Overall section
header_color <- "#4cc3c9"  # Primary color for header row
header_border <- fp_border(color = "black", width = 1)  # Black border for header

# Step 4: Create FlexTable
ar.2.1 <- flextable(regional_data_combined) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%  # Set font size 10 for body text
  merge_v(j = ~ `Example Category`) %>%  # Merge vertical cells for "Example Category"
  autofit() %>%
  
  # Header Styling
  bg(part = "header", bg = header_color) %>%
  color(part = "header", color = "black") %>%  # Black font in header for better contrast
  border(part = "header", border.top = header_border, border.bottom = header_border) %>%  # Header Border
  
  # Apply blue border styling for "Graph Data: Country-led Example Using Recommendations"
  border(i = which(regional_data_combined$`Example Category` == "Graph Data: Country-led Example Using Recommendations"), 
         border.top = highlight_border, 
         border.bottom = highlight_border) %>% 
         # border.left = highlight_border, 
         # border.right = highlight_border) %>%
  
  # Apply black border styling for "Overall Country-led Example"
  border(i = which(regional_data_combined$`Example Category` == "Overall Country-led Example"), 
         border.top = fp_border(color = "gray", width = 0.5), 
         border.bottom = fp_border(color = "gray", width = 0.5)) %>% 
         # border.left = fp_border(color = "gray", width = 0.5), 
         # border.right = fp_border(color = "gray", width = 0.5)) %>%
  
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  
  # Footer details
  add_footer_row(
    values = paste0(
      "Footnote: Graph Data: Country-led Example Using Recommendations refers to country-led projects that explicitly use EGRISS recommendations. ",
      "This section highlights the regional distribution of cases where national statistical offices or institutions reported following EGRISS guidance. ",
      "The data is collected based on responses to PRO09, indicating direct implementation of statistical recommendations in forced displacement data collection efforts."
    ),
    colwidths = ncol(regional_data_combined)  # Ensure footer spans full table width
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.2.1: Country-led implementation of the Recommendations by region (Figure 6, AR pg.25)",
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
ar.2.1
