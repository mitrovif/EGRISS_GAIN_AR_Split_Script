
# ============================================================================================================
# AR.3.1: Analysis of focal points on GAIN based on their connection to migration statistics
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
      LOC01 == 1        ~ "Country-Led Examples",
      LOC01 %in% c(2,3) ~ "Institution-Led Examples",
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
      FOC02A == 1 & FOC02B == 1 ~ "Has migration in title",
      FOC02A == 1 & FOC02B == 2 ~ "No migration in title",
      FOC02A == 1 & (is.na(FOC02B) | FOC02B == 3) ~ "Has population in title",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Focal_Position))

# Define ordering
lead_levels <- c("Overall Examples", "Country-Led Examples", "Institution-Led Examples")
foc_levels  <- c(
  "1: Reporting on migration & forcibly displaced/stateless",
  "2: Reporting only on forcibly displaced/stateless",
  "3: Head or deputy head of organization",
  "8: Generic focal point position",
  "9: No data on focal point position"
)
sub_levels  <- c("Has migration in title", "No migration in title", "Has population in title")

# Summarise overall (all LOC01==1,2,3)
tbl_overall <- main_roster2 %>%
  filter(LOC01 %in% c(1,2,3)) %>%
  group_by(Focal_Position, Migration_Subcat, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = 0) %>%
  mutate(
    Example_Lead     = "Overall Examples",
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
  filter(tbl_by_lead, Example_Lead == "Country-Led Examples"),
  filter(tbl_by_lead, Example_Lead == "Institution-Led Examples")
) %>%
  arrange(Example_Lead, Focal_Position, Migration_Subcat) %>%
  mutate(Total = rowSums(across(c(`2021`, `2022`, `2023`, `2024`)), na.rm = TRUE))

# Build the flextable, specifying column order and labels
ar.3.1 <- flextable(
  final_tbl,
  col_keys = c(
    "Example_Lead",
    "Focal_Position",
    "Migration_Subcat",
    "2021","2022","2023","2024","Total"
  )
) %>%
  set_header_labels(
    Example_Lead     = "Example Lead",
    Focal_Position   = "GAIN Respondent Position",
    Migration_Subcat = "GAIN Respondent Position Link to Migration",
    `2021`           = "2021",
    `2022`           = "2022",
    `2023`           = "2023",
    `2024`           = "2024",
    Total            = "Total"
  ) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(j = ~ `Total`, bg = "#c9daf8") %>%
  bg(j = ~ `2024`, bg = "#F4CCCC") %>%
  merge_v(j = "Example_Lead") %>%        # suppress repeated Example Lead
  merge_v(j = "Focal_Position") %>%      # suppress repeated Focal Position (incl. the second "1: Reporting…")
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 3.1 is not featured in the 2024 Annual Report. Table is disaggregated by example lead, position of the GAIN respondent in their organization, GAIN respondent reporting on migration next to forced displacement and by year."
    ),
    colwidths = ncol(final_tbl)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 3.1: Analysis of focal points on GAIN based on their connection to migration statistics",
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
ar.3.1

