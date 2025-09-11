
# ============================================================================================================
# AR.1.2: Overview of the Implementation of the IRRS, IRIS, and IROSS (Figure 5) in new version of AR 
# ============================================================================================================

group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.csv")
group_roster <- read.csv(group_roster_file)

# Define Colors (with transparency for better readability)
iris_color <- "#072D62AA"        # Dark Blue (IRIS)
irrs_color <- "#14234CAA"        # Navy Blue (IRRS)
iross_color <- "#3B71B9AA"       # Medium Blue (IROSS)
undetermined_color <- "#7F7F7FAA" # Grey (Undetermined)
mixed_color <- "#D9D9D9AA"        # Light Grey (Mixed)
group_roster <- group_roster %>%
  mutate(
    PRO10.A = as.numeric(gsub("[^0-9]", "", PRO10.A)),
    PRO10.B = as.numeric(gsub("[^0-9]", "", PRO10.B)),
    PRO10.C = as.numeric(gsub("[^0-9]", "", PRO10.C)),
    PRO10.Z = as.numeric(gsub("[^0-9]", "", PRO10.Z)),
    PRO09 = as.numeric(gsub("[^0-9]", "", PRO09)),
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
  filter(PRO09 == 1) %>%
  group_by(g_conled, g_recuse, ryear) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    `Example Lead` = case_when(
      g_conled == 1 ~ "Country-Led Examples",
      g_conled == 2 ~ "Institution-Led Examples",
      g_conled == 3 ~ "CSO Led Examples",
      g_conled == 8 ~ "Unknown",
      TRUE ~ ""
    )
  ) %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(`2021`:`2024`), na.rm = TRUE)) %>%
  select(`Example Lead`, `Use of Recommendations` = g_recuse, `2021`, `2022`, `2023`, `2024`, Total)

# Ensure year columns are numeric
recuse_table <- recuse_table %>%
  mutate(across(`2021`:`2024`, as.numeric),
         Total = rowSums(across(`2021`:`2024`), na.rm = TRUE))

# Remove duplicated Example Lead labels
# recuse_table$`Example Lead` <- ifelse(duplicated(recuse_table$`Example Lead`), "", recuse_table$`Example Lead`)

# Add aggregated rows for IRRS, IRIS, IROSS, Mixed, and Undetermined
aggregated_rows <- recuse_table %>%
  group_by(`Use of Recommendations`) %>%
  summarise(across(`2021`:`Total`, sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Example Lead` = "Overall Examples") %>%
  select(`Example Lead`, `Use of Recommendations`, everything())

# Insert aggregated rows at the top
recuse_table <- bind_rows(aggregated_rows, recuse_table)

# Ensure "Graph Data" only appears once
# recuse_table$`Example Lead` <- ifelse(duplicated(recuse_table$`Example Lead`) & recuse_table$`Example Lead` == "Graph Data", "", recuse_table$`Example Lead`)

# Create flextable with consistent styling and colors
ar.1.2 <- flextable(recuse_table) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  autofit() %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Added outer border
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(i = 1, bg = iris_color, part = "body") %>%  # IRIS
  bg(i = 2, bg = irrs_color, part = "body") %>%  # IRRS
  bg(i = 3, bg = iross_color, part = "body") %>%  # IROSS
  bg(i = 4, bg = mixed_color, part = "body") %>%  # Mixed
  bg(i = 5, bg = undetermined_color, part = "body") %>%  # Undetermined
  bg(i = 1, j = "Example Lead", bg = "white", part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  merge_v(j = ~ `Example Lead`) %>%
  color(i = 2:5, color = "white", part = "body") %>%  # White text for first five rows
  color(i = 4, color = "black", part = "body") %>%
  color(j = 6:7, color = "black", part = "body") %>%
  bg(bg = "#f4cccc", j = ~ `2024`) %>%
  bg(bg = "#c9daf8", j = ~ Total) %>%
  add_footer_row(
    values = paste0(
      "Table 1.2 supports Figure 5 in the 2024 Annual Report (replicated above). In addition to Figure 5 data, examples are disaggregated by lead (country-led or institution-led examples, both international or CSO) and by year of reporting in GAIN."
    ),
    colwidths = ncol(recuse_table)  # Ensure footer spans the full table width dynamically
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1.2 Breakdown of IRRS, IRIS and IROSS implementation examples in 2024",
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
ar.1.2
