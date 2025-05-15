# ============================================================================================================
# AR.8.1: Analysis of focal points on GAIN based on their connection to migration statistics
# ============================================================================================================

library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Load dataset
main_roster_file <- "analysis_ready_main_roster.csv"
main_roster <- read.csv(main_roster_file)

# Recode Example Lead and FOC02 categories
main_roster2 <- main_roster %>%
  # ensure FOC02A and FOC02B are numeric
  mutate(across(c(FOC02A, FOC02B), as.numeric)) %>%
  # now recode
  mutate(
    Example_Lead = case_when(
      LOC01 == 1        ~ "Nationally Led Examples",
      LOC01 %in% c(2,3) ~ "Institutionally Led Examples",
      TRUE              ~ "Unknown"
    ),
    Focal_Position = case_when(
      FOC02A == 1 ~ "1: Reporting on migration & forcibly displaced/stateless",
      FOC02A == 2 ~ "2: Reporting only on forcibly displaced/stateless",
      FOC02A == 3 ~ "3: Head or deputy head of organization",
      FOC02A == 8 ~ "8: Generic focal point position",
      FOC02A == 9 ~ "9: No data on focal point position",
      TRUE        ~ NA_character_
    ),
    Migration_Subcat = case_when(
      FOC02A == 1 & FOC02B == 1           ~ "Has migration in title",
      FOC02A == 1 & FOC02B == 2           ~ "No migration in title",
      is.na(FOC02B) | FOC02B == 3         ~ "Has population in title",
      TRUE                                ~ "Has population in title"
    )
  ) %>%
  filter(!is.na(Focal_Position))



# Define ordering
lead_levels <- c("Overall", "Nationally Led Examples", "Institutionally Led Examples")
foc_levels  <- c(
  "1: Reporting on migration & forcibly displaced/stateless",
  "2: Reporting only on forcibly displaced/stateless",
  "3: Head or deputy head of organization",
  "8: Generic focal point position",
  "9: No data on focal point position"
)
sub_levels  <- c("Has migration in title", "No migration in title")

# Summarise overall (all LOC01==1,2,3)
tbl_overall <- main_roster2 %>%
  filter(LOC01 %in% c(1,2,3)) %>%
  group_by(Focal_Position, Migration_Subcat, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = 0) %>%
  mutate(
    Example_Lead     = "Overall",
    Focal_Position   = factor(Focal_Position, levels = foc_levels),
    Migration_Subcat = factor(Migration_Subcat, levels = sub_levels),
    Example_Lead     = factor(Example_Lead, levels = lead_levels)
  )

# Summarise by Example Lead
tbl_by_lead <- main_roster2 %>%
  filter(LOC01 %in% c(1,2,3)) %>%
  group_by(Example_Lead, Focal_Position, Migration_Subcat, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = 0) %>%
  mutate(
    Example_Lead     = factor(Example_Lead, levels = lead_levels),
    Focal_Position   = factor(Focal_Position, levels = foc_levels),
    Migration_Subcat = factor(Migration_Subcat, levels = sub_levels)
  )

# Combine: Overall first, then National, then Institutional
final_tbl <- bind_rows(
  tbl_overall,
  filter(tbl_by_lead, Example_Lead == "Nationally Led Examples"),
  filter(tbl_by_lead, Example_Lead == "Institutionally Led Examples")
) %>%
  arrange(Example_Lead, Focal_Position, Migration_Subcat)

# Build the flextable, specifying column order and labels
ar.8.1 <- flextable(
  final_tbl,
  col_keys = c(
    "Example_Lead",
    "Focal_Position",
    "Migration_Subcat",
    "2021","2022","2023","2024"
  )
) %>%
  set_header_labels(
    Example_Lead     = "Example Lead",
    Focal_Position   = "Focal Point Position",
    Migration_Subcat = "Migration Title Sub‐category",
    `2021`           = "2021",
    `2022`           = "2022",
    `2023`           = "2023",
    `2024`           = "2024"
  ) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  merge_v(j = "Example_Lead") %>%        # suppress repeated Example Lead
  merge_v(j = "Focal_Position") %>%      # suppress repeated Focal Position (incl. the second "1: Reporting…")
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: FOC02A categories are: 1=reporting on migration & forcibly displaced/stateless; ",
      "2=reporting only on forcibly displaced/stateless; 3=head/deputy head; ",
      "8=generic focal point position; 9=no data. For FOC02A == 1, FOC02B: ",
      "1=has migration in title; 2=does not have migration in title."
    ),
    colwidths = ncol(final_tbl)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.8.1: Analysis of focal points on GAIN based on their connection to migration statistics",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  ) %>%
  fix_border_issues()

# Display the table
ar.8.1
