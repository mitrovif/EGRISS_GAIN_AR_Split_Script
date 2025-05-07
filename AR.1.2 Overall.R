
# ============================================================================================================
# Overview of the Implementation of the IRRS, IRIS, and IROSS (Figure 5) in new version of AR 
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
      g_conled == 1 ~ "Nationally Led Examples",
      g_conled == 2 ~ "Institutionally Led Examples",
      g_conled == 3 ~ "CSO Led Examples",
      g_conled == 8 ~ "Unknown",
      TRUE ~ ""
    )
  ) %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(`2021`:`2024`), na.rm = TRUE)) %>%
  select(`Example Lead`, `Use of Recommendations by Leads` = g_recuse, `2021`, `2022`, `2023`, `2024`, Total)

# Ensure year columns are numeric
recuse_table <- recuse_table %>%
  mutate(across(`2021`:`2024`, as.numeric),
         Total = rowSums(across(`2021`:`2024`), na.rm = TRUE))

# Remove duplicated Example Lead labels
# recuse_table$`Example Lead` <- ifelse(duplicated(recuse_table$`Example Lead`), "", recuse_table$`Example Lead`)

# Add aggregated rows for IRRS, IRIS, IROSS, Mixed, and Undetermined
aggregated_rows <- recuse_table %>%
  group_by(`Use of Recommendations by Leads`) %>%
  summarise(across(`2021`:`Total`, sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Example Lead` = "Graph Data") %>%
  select(`Example Lead`, `Use of Recommendations by Leads`, everything())

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
  # color(i = 1, j = "Example Lead", color = "black", part = "body") %>%
  add_footer_row(
    values = paste0(
      "Graph Data is based on the implementation of the IRRS, IRIS, and IROSS in 2024. ",
      "Nationally led and institutionally led examples have been categorized into distinct recommendation types (IRRS, IRIS, IROSS, Mixed, Undetermined). ",
      "• IRRS: Cases where only IRRS recommendations were used. ",
      "• IRIS: Cases where only IRIS recommendations were used. ",
      "• IROSS: Cases where only IROSS recommendations were used. ",
      "• Mixed: Cases where more than one recommendation type was used. ",
      "• Undetermined: Cases where respondents were unsure of which recommendations were used or did not report their use. "
    ),
    colwidths = ncol(recuse_table)  # Ensure footer spans the full table width dynamically
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption("Figure 5: Overview of the Implementation of the IRRS, IRIS and IROSS in 2024") %>%  # Add caption 
  fix_border_issues()
  
# Display Merged Table

ar.1.2
