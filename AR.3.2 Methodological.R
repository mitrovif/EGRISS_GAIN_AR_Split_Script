
# ============================================================================================================
# AR.3.2: Breakdown of identification questions used by example lead 
# ============================================================================================================

library(dplyr)
library(stringr)
library(tidyr)
library(flextable)

group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.csv")
group_roster <- read.csv(group_roster_file)

test <- group_roster %>%
  filter(!is.na(PRO13B) & PRO13B == 1)

test <- group_roster %>%
  filter(PRO13B == 1) %>%
  count(PRO13B)

# Step 1: Filter and count by year
yearly_counts <- group_roster %>%
  filter(str_detect(PRO08, "ADMINISTRATIVE DATA|SURVEY")) %>%
  count(year) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

# Step 1.1: Filter for ID questions and count by year
id_counts <- group_roster %>%
  filter(PRO13B == 1) %>%
  count(year) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

# Step 2: Add Total column
yearly_counts <- yearly_counts %>%
  mutate(Total = rowSums(across(everything())))

id_counts <- id_counts %>%
  mutate(Total = rowSums(across(everything())))

# Step 3: Add descriptive columns
yearly_final_tbl <- yearly_counts %>%
  mutate(
    `Example Lead` = "Overall",
    `Identification Questions` = "Surveys and/or administrative data sources"
  ) %>%
  select(`Example Lead`, `Identification Questions`, everything()) %>%
  mutate(across(everything(), as.character))

id_final_tbl <- id_counts %>%
  mutate(
    `Example Lead` = "Overall",
    `Identification Questions` = "Surveys and/or administrative data sources using identification questions"
  ) %>%
  select(`Example Lead`, `Identification Questions`, everything()) %>%
  mutate(across(everything(), as.character))

# Step 4: Bind overall tables
final_tbl <- bind_rows(yearly_final_tbl, id_final_tbl)

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
        TRUE                  ~ "Other"
      )
    ) %>%
    group_by(`Identification Questions`, year) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = Count, values_fill = 0) %>%
    arrange(`Identification Questions`)
}

# Summarise PRO13C fields
summarise_pro13c <- function(gr) {
  structured_cols <- grep("^PRO13C[A-Z]$", names(gr), value = TRUE)
  
  filtered <- gr %>%
    filter(rowSums(select(., all_of(structured_cols)), na.rm = TRUE) > 0)
  
  structured_summary <- filtered %>%
    select(year, all_of(structured_cols)) %>%
    pivot_longer(cols = -year, names_to = "Category", values_to = "Value") %>%
    filter(Value == 1) %>%
    group_by(year, Category) %>%
    summarise(Count = n(), .groups = "drop")
  
  map_challenge(structured_summary)
}

# Aggregate by lead type
summarise_by_group_lead <- function(data, group_label) {
  summarise_pro13c(data) %>%
    mutate(`Example Lead` = group_label) %>%
    relocate(`Example Lead`) %>%
    mutate(Total = rowSums(select(., where(is.numeric))))
}

overall_df     <- summarise_by_group_lead(group_roster,                            "Overall")
national_df    <- summarise_by_group_lead(filter(group_roster, g_conled == 1),     "Country-Led Examples")
institution_df <- summarise_by_group_lead(filter(group_roster, g_conled %in% c(2,3)), "Institution-Led Examples")

# Combine, set factor order so "Overall" comes first, then sort within each
final_pro13c <- bind_rows(overall_df, national_df, institution_df) %>%
  mutate(
    `Example Lead` = factor(
      `Example Lead`,
      levels = c("Overall", "Country-Led Examples", "Institution-Led Examples")
    )
  ) %>%
  arrange(`Example Lead`, desc(year)) %>%
  select(`Example Lead`, `Identification Questions`, `2024`, Total) %>%
  mutate(across(everything(), as.character))

# Get column names from original table
col_names <- colnames(final_pro13c)

# Create the new rows as tibbles
overview1 <- tibble::tibble(!!!setNames(rep("Overview of surveys and administrative data", length(col_names)), col_names))
blank_row <- tibble::tibble(!!!setNames(rep("", length(col_names)), col_names))
overview2 <- tibble::tibble(!!!setNames(rep("Overview of identification questions", length(col_names)), col_names))

# Combine with original data
final_pro13c_extended <- bind_rows(overview1, final_tbl, blank_row, overview2, final_pro13c)

# Create FlexTable named ar.8.2 with Example Lead on the left
ar.3.2 <- flextable(final_pro13c_extended) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  merge_v(j = ~ `Example Lead`) %>%
  merge_h(i = 1) %>%
  merge_h(i = 5) %>%
  bg(j = ~ `2024`, bg = "#F4CCCC") %>%
  bg(bg = "gray", i = 1) %>%
  bg(bg = "gray", i = 5) %>%
  bold(i = c(1, 5), bold = TRUE, part = "body") %>%
  bg(j = ~ Total, bg = "#c9daf8") %>%  # Highlight the total column
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 3.2 is not featured in the 2024 Annual Report. Table is disaggregated by example lead, type of identification question and year. Question was first introduced in GAIN in 2024, so previous years are not presented."
    ),
    colwidths = ncol(final_pro13c)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 3.2: Breakdown of identification questions used by example lead",
        props = fp_text(font.family = "Helvetica", font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

# Display the table
ar.3.2
