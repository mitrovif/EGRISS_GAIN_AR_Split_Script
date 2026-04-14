
# ============================================================================================================
# AR.1.2: Overview of the Implementation of the IRRS, IRIS, and IROSS (Figure 5) in new version of AR 
# ============================================================================================================

group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.xlsx")
group_roster <- read.xlsx(group_roster_file)

# Define Colors (with transparency for better readability)
iris_color <- "#072D62AA"        # Dark Blue (IRIS)
irrs_color <- "#14234CAA"        # Navy Blue (IRRS)
iross_color <- "#3B71B9AA"       # Medium Blue (IROSS)
undetermined_color <- "#7F7F7FAA" # Grey (Undetermined)
mixed_color <- "#D9D9D9AA"        # Light Grey (Mixed)


contr <- group_roster %>% 
  filter(year == 2025) %>% 
  group_by(PRO09, PRO14B, g_recuse) %>% 
  tally()

contr <- group_roster %>%
  filter(year == 2025) %>% 
  group_by(PRO14B) %>% 
  tally()


contr <- group_roster %>%
  filter(year == 2025) %>% 
  group_by(PRO14C) %>% 
  tally()

group_roster <- group_roster %>%
  mutate(
    g_recuse = case_when(
      PRO09 == 1 & PRO10.A == 1 & (is.na(PRO10.B) | PRO10.B != 1) &
        (is.na(PRO10.C) | PRO10.C != 1) ~ "IRRS",
      PRO09 == 1 & PRO10.A != 1 & PRO10.B == 1 & PRO10.C != 1 ~ "IRIS",
      PRO09 == 1 & PRO10.A != 1 & PRO10.B != 1 & PRO10.C == 1 ~ "IROSS",
      PRO09 == 1 & rowSums(cbind(PRO10.A, PRO10.B, PRO10.C), na.rm = TRUE) > 1 ~ "Mixed",
      PRO09 == 1 & PRO10.Z == 1 ~ "Undetermined",
      PRO09 == 1 & PRO10.A != 1 & PRO10.B != 1 & PRO10.C != 1 & PRO10.Z != 1 ~ "Undetermined",
      TRUE ~ "Undetermined"  # Changed NA entries to "Undetermined"
    )
  )

# Aggregate Use of Recommendations (Figure 5)
recuse_table <- group_roster %>%
  filter(PRO09 == 1,
         year == 2025) %>%
  group_by(PRO14B, g_recuse, g_conled) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    `Stategory/policy document` = case_when(
      PRO14B == 1 ~ "Yes",
      PRO14B == 2 ~ "No",
      PRO14B == 8 ~ "Unknown",
      TRUE ~ "No answer"
    ),
    `Example Lead` = case_when(
      g_conled == 1 ~ "Country-Led Examples",
      g_conled == 2 ~ "Institution-Led Examples",
      g_conled == 3 ~ "CSO Led Examples",
      g_conled == 8 ~ "Unknown",
      TRUE ~ ""
    ),
    ) %>% select(-PRO14B) %>% 
  pivot_wider(names_from = `Stategory/policy document`, values_from = Count, values_fill = 0) %>%
  select(`Example Lead`, `Recommendations` = g_recuse, `Yes`, `No`, `No answer`)

# Ensure year columns are numeric
recuse_table <- recuse_table %>%
  mutate(
    across(c(`Yes`, `No`, `No answer`), as.numeric),
    Total = rowSums(across(c(`Yes`, `No`, `No answer`)), na.rm = TRUE)
  ) %>% 
  arrange(`Example Lead`, factor(Recommendations, levels = c("IRRS", "IRIS", "IROSS", "Mixed", "Undetermined")))




#Overall results
recuse_table_overall <- group_roster %>%
  filter(PRO09 == 1,
         year == 2025) %>%
  group_by(PRO14B, g_recuse) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    `Stategory/policy document` = case_when(
      PRO14B == 1 ~ "Yes",
      PRO14B == 2 ~ "No",
      PRO14B == 8 ~ "Unknown",
      TRUE ~ "No answer"
    ),
    `Example Lead` = 'Overall'
  ) %>% select(-PRO14B) %>% 
  pivot_wider(names_from = `Stategory/policy document`, values_from = Count, values_fill = 0) %>%
  select(`Example Lead`, `Recommendations` = g_recuse, `Yes`, `No`, `No answer`) %>% 
  mutate(
    across(c(`Yes`, `No`, `No answer`), as.numeric),
    Total = rowSums(across(c(`Yes`, `No`, `No answer`)), na.rm = TRUE)
  ) %>% 
  arrange(`Example Lead`, factor(Recommendations, levels = c("IRRS", "IRIS", "IROSS", "Mixed", "Undetermined")))


recuse_table <- rbind(recuse_table_overall, recuse_table)



# Create flextable with consistent styling and colors
ar.1.5.a <- flextable(recuse_table) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  autofit() %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Added outer border
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  # bg(i = 1, bg = iris_color, part = "body") %>%  # IRIS
  # bg(i = 2, bg = irrs_color, part = "body") %>%  # IRRS
  # bg(i = 3, bg = iross_color, part = "body") %>%  # IROSS
  # bg(i = 4, bg = mixed_color, part = "body") %>%  # Mixed
  # bg(i = 5, bg = undetermined_color, part = "body") %>%  # Undetermined
  # bg(i = 1, j = "Example Lead", bg = "white", part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  merge_v(j = ~ `Example Lead`) %>%
  # color(i = 1:5, color = "white", part = "body") %>%  # White text for first five rows
  color(i = 4, color = "black", part = "body") %>%
  # color(j = 7:8, color = "black", part = "body") %>%
  # bg(bg = "#f4cccc", j = ~ `2025`) %>%
  bg(bg = "#c9daf8", j = ~ Total) %>%
  add_footer_row(
    values = paste0(
      "Table 1.5.a. supports Figure X in the 2025 Annual Report (replicated above). In addition to Figure X data, examples are disaggregated by lead (country-led or institution-led examples, both international or CSO) and by the use of recommendations."
    ),
    colwidths = ncol(recuse_table)  # Ensure footer spans the full table width dynamically
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1.5.a. Breakdown of IRRS, IRIS and IROSS implementation examples by strategic and policy implication in 2025",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%  # Add caption 
  fix_border_issues()

# Display Merged Table
ar.1.5.a

if (bExport == 1){
  save_as_html(ar.1.5.a, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.1.5.a.html") 
}
