library(dplyr)
library(stringr)
library(tidyr)
library(flextable)

group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.csv")
group_roster <- read.csv(group_roster_file)

# Helper: Map raw categories to descriptive labels
map_challenge <- function(df) {
  df %>%
    mutate(
      `Identification Questions` = case_when(
        Category == "PRO13CA" ~ "Forced to flee",
        Category == "PRO13CB" ~ "Habitual residency",
        Category == "PRO13CC" ~ "Crossing a border",
        Category == "PRO13CD" ~ "Citizenship",
        Category == "PRO13CE" ~ "International protection",
        Category == "PRO13CX" ~ "Other",
        TRUE               ~ "Other"
      )
    ) %>%
    group_by(`Identification Questions`) %>%
    summarise(Responses = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Responses))
}

# Summarise PRO13C fields
summarise_pro13c <- function(gr) {
  structured_cols <- grep("^PRO13C[A-Z]$", names(gr), value = TRUE)
  filtered <- gr %>%
    filter(rowSums(select(., all_of(structured_cols)), na.rm = TRUE) > 0)
  structured_summary <- filtered %>%
    select(all_of(structured_cols)) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Category", values_to = "Count")
  map_challenge(structured_summary)
}

# Aggregate by lead type
summarise_by_group_lead <- function(data, group_label) {
  summarise_pro13c(data) %>%
    mutate(`Example Lead` = group_label)
}

overall_df     <- summarise_by_group_lead(group_roster,                            "Overall")
national_df    <- summarise_by_group_lead(filter(group_roster, g_conled == 1),     "Nationally Led Examples")
institution_df <- summarise_by_group_lead(filter(group_roster, g_conled %in% c(2,3)), "Institutionally Led Examples")

# Combine, set factor order so "Overall" comes first, then sort within each
final_pro13c <- bind_rows(overall_df, national_df, institution_df) %>%
  mutate(
    `Example Lead` = factor(
      `Example Lead`,
      levels = c("Overall", "Nationally Led Examples", "Institutionally Led Examples")
    )
  ) %>%
  arrange(`Example Lead`, desc(Responses)) %>%
  select(`Example Lead`, `Identification Questions`, Responses)

# Create FlexTable named ar.8.2 with Example Lead on the left
ar.8.2 <- flextable(final_pro13c) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  merge_v(j = ~ `Example Lead`) %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: Data source = analysis_ready_group_roster.csv; ",
      "Columns: Identification Questions (mapped from PRO13CA–CX), ",
      "Responses (count of structured PRO13C responses), ",
      "Example Lead (group classification); ",
      "Nationally Led Examples: g_conled == 1; ",
      "Institutionally Led Examples: g_conled %in% c(2,3)."
    ),
    colwidths = ncol(final_pro13c)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.8.2: Breakdown of Identification Questions Used by Example Lead",
        props = fp_text(font.family = "Helvetica", font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

# Display the table
ar.8.2
