
# ============================================================================================================
# AR.1.3: Figure 7: Overview Data Sources and Tools for Country-led Examples 2024 (in new version fo AR)
# ============================================================================================================

library(dplyr)
library(tidyr)
library(flextable)

# Filter for Mixed Use Entries
mixed_use_table <- group_roster %>%
  filter(g_recuse == "Mixed") %>%
  mutate(
    Mixed_Category = case_when(
      PRO10.A == 1 & PRO10.B == 1 & PRO10.C != 1 ~ "IRRS + IRIS",
      PRO10.A == 1 & PRO10.B != 1 & PRO10.C == 1 ~ "IRRS + IROSS",
      PRO10.A != 1 & PRO10.B == 1 & PRO10.C == 1 ~ "IRIS + IROSS",
      PRO10.A == 1 & PRO10.B == 1 & PRO10.C == 1 ~ "All 3 Combined",
      TRUE ~ "Other"
    ),
    `Example Lead` = case_when(
      g_conled == 1 ~ "Nationally Led Examples",
      g_conled %in% c(2, 3) ~ "Institutionally Led Examples",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(Mixed_Category != "Other") %>%
  group_by(`Example Lead`, Mixed_Category, ryear) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = 0) %>%
  select(`Example Lead`, Mixed_Category, `2021`, `2022`, `2023`, `2024`)  # Removed Total

# Add Overall Summary
overall_summary <- mixed_use_table %>%
  group_by(Mixed_Category) %>%
  summarise(across(`2021`:`2024`, sum, na.rm = TRUE)) %>%
  mutate(`Example Lead` = "Overall")

# Combine Overall, Nationally Led, and Institutionally Led Tables in Correct Order
final_table <- bind_rows(
  overall_summary,                        
  mixed_use_table %>% filter(`Example Lead` == "Nationally Led Examples"),
  mixed_use_table %>% filter(`Example Lead` == "Institutionally Led Examples")
)  %>%
  rename(`Mixed Category` = Mixed_Category) %>%
  select(c("Example Lead", "Mixed Category", "2021", "2022", "2023", "2024"))

# Remove duplicated Example Lead labels for cleaner presentation
# final_table$`Example Lead` <- ifelse(duplicated(final_table$`Example Lead`), "", final_table$`Example Lead`)

# Create flextable with outer border
ar.1.3 <- flextable(final_table) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  merge_v(j = ~ `Example Lead`) %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: Includes only projects flagged `g_recuse == \"Mixed\"` in group_roster. ",
      "Mixed Category defined as: IRRS + IRIS (PRO10.A & PRO10.B == 1), IRRS + IROSS (PRO10.A & PRO10.C == 1), ",
      "IRIS + IROSS (PRO10.B & PRO10.C == 1), All 3 Combined (PRO10.A & PRO10.B & PRO10.C == 1). ",
      "`Example Lead` indicates Nationally Led (g_conled == 1) vs Institutionally Led (g_conled %in% c(2,3)). ",
      "Counts by year (2021–2024) reflect the `ryear` field."
    ),
    colwidths = ncol(final_table)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
 set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.1.3: Overview of the Mixed Implementation of the IRRS, IRIS and IROSS, by year (AR pg.24)",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display the new table
ar.1.3
