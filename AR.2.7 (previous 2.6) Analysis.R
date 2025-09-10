
# ============================================================================================================
# AR.2.7: Breakdown of Nationally Led Partnerships
# ============================================================================================================

library(dplyr)
library(tidyr)
library(flextable)

# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
background_color <- "#f0f8ff"

# Load dataset
file_path <- file.path(working_dir, "analysis_ready_group_roster.csv")
data <- read.csv(file_path)

# Define Ordered Partnership Type Labels
partnership_labels <- c(
  "PRO18.A" = "National Organisation Partnerships",
  "PRO18.B" = "International Organisation Partnerships",
  "PRO18.C" = "Academia Partnerships"
)

# Define Year Order
year_order <- c("2021", "2022", "2023", "2024")

# Count total nationally led projects
nationally_led_count <- data %>%
  filter(g_conled == 1) %>%
  count(ryear) %>%
  pivot_wider(names_from = ryear, values_from = n, values_fill = 0) %>%
  mutate(Partnership_Type = "Total Country-Led Projects")

# Count total nationally led projects with partnerships
partnership_count <- data %>%
  filter(g_conled == 1, PRO17 == 1) %>%
  count(ryear) %>%
  pivot_wider(names_from = ryear, values_from = n, values_fill = 0) %>%
  mutate(Partnership_Type = "Total Country-Led Projects with Partnerships")

# Filter for PRO17 == 1 and g_conled == 1
partnership_data <- data %>%
  filter(g_conled == 1, PRO17 == 1) %>%  # Only nationally led projects with partnerships
  select(ryear, PRO18.A, PRO18.B, PRO18.C) %>%  # Keep necessary columns
  mutate(ryear = as.character(ryear)) %>%  # Ensure ryear is treated as character
  pivot_longer(cols = starts_with("PRO18"), names_to = "Partnership_Type", values_to = "Value") %>%
  mutate(Partnership_Type = recode(Partnership_Type, !!!partnership_labels)) %>%  # Apply Partnership Labels
  filter(Value == 1) %>%  # Keep only rows where partnership exists (Value == 1)
  count(Partnership_Type, ryear) %>%  # Count occurrences per year
  pivot_wider(names_from = ryear, values_from = n, values_fill = 0)  # Convert to wide format

# Combine total count with detailed breakdown
partnership_data <- bind_rows(nationally_led_count, partnership_count, partnership_data)

# Ensure Year Order in Columns
partnership_data <- partnership_data %>%
  mutate('Partnership Type' = Partnership_Type) %>%
  select('Partnership Type', all_of(year_order)) %>%
  mutate(Total = rowSums(across(c(`2021`, `2022`, `2023`, `2024`)), na.rm = TRUE)) %>%
  add_row(.after = 2) %>%
  add_row(.after = 6) %>%
  # Insert "Overall:" just below header (row 1 in R indexing)
  add_row(`Partnership Type` = "Overall:", .before = 1) %>%
  # Insert "Type of Partnerships:" below the first NA row (originally row 3, but now shifted)
  add_row(`Partnership Type` = "Type of Partnerships:", .before = 5)

# TABLE ON REGIONS
partnership_regions <- group_roster %>%
  filter(g_conled == 1 & PRO09 == 1) %>%
  mutate(region = case_when(
    region == "Asia" ~ "Asia and Oceania",
    region == "North America" | region == "South America" ~ "Americas",
    region == "Oceania" ~ "Asia and Oceania",
    .default = region
  )) %>%
  group_by(PRO17, region, year) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(PRO17 = case_when(
    PRO17 == 1 ~ "Country-Led Examples with Partnerships",
    PRO17 == 2 ~ "Country-Led Examples without Partnerships",
    TRUE ~ as.character(PRO17)
  )) %>%
  mutate(PRO17 = case_when(
    is.na(PRO17) | PRO17 == "" ~ "Don't Know or No Response Provided",
    TRUE ~ PRO17
  )) %>%
  pivot_wider(
    names_from = year,
    values_from = Count,
    names_prefix = "Count_"
  ) %>%
  mutate(across(starts_with("Count_"), ~replace_na(., 0))) %>%
  rename(`2021` = Count_2021,
         `2022` = Count_2022,
         `2023` = Count_2023,
         `2024` = Count_2024,
         Region = region,
         `Partnerships` = PRO17) %>%
  mutate(Total = rowSums(select(., starts_with("20"))))

# Combine tables
# Add missing column `Region` to partnership_data
partnership_data2 <- partnership_data %>%
  mutate(Region = NA_character_) %>%
  # Reorder columns to match df2
  select(`Partnership Type`, Region, `2021`, `2022`, `2023`, `2024`, Total)

library(zoo)
partnership_data2 <- as.data.frame(t(apply(partnership_data2, 1, na.locf, na.rm = FALSE)))

# Rename to match naming convention
partnership_data2 <- partnership_data2 %>%
  rename(Partnerships = `Partnership Type`)

# Bind the two together
partnership_regions <- partnership_regions %>%
  mutate(across(everything(), as.character))

final_table <- bind_rows(partnership_data2, partnership_regions)

# Create the new row as a tibble with all values as characters
new_row <- tibble(
  Partnerships = "Partnerships",
  Region = "Region",
  `2021` = "2021",
  `2022` = "2022",
  `2023` = "2023",
  `2024` = "2024",
  Total = "Total"
)

# Insert after row 9
final_table2 <- bind_rows(
  final_table[1:9, ],
  new_row,
  final_table[10:nrow(final_table), ]
)

# Create visual table
ar.2.7 <- flextable(final_table2) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 1)) %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  bg(j = ~ `Total`, bg = "#c9daf8") %>%
  bg(j = ~ `2024`, bg = "#F4CCCC") %>%
  bg(i = 1, bg = "#D9D9D9") %>%
  bg(i = 5, bg = "#D9D9D9") %>%
  bg(i = 10, bg = "#4cc3c9") %>%
  bold(i = 10, bold = TRUE) %>%
  hline(i = 10, border = fp_border(color = "black", width = 2)) %>%
  hline(i = 9, border = fp_border(color = "black", width = 2)) %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 0.6) %>%
  merge_at(i = 1, j = 1:7) %>%
  merge_at(i = 5, j = 1:7) %>%
  merge_v(j = ~ `Partnerships`) %>%
  merge_at(i = 2, j = 1:2) %>%  # merge Partnerships + Region in row 2
  merge_at(i = 3, j = 1:2) %>%  # merge Partnerships + Region in row 3
  merge_at(i = 6, j = 1:2) %>%  # merge Partnerships + Region in row 6
  merge_at(i = 7, j = 1:2) %>%  # merge Partnerships + Region in row 7
  merge_at(i = 8, j = 1:2) %>%  # merge Partnerships + Region in row 8
  set_header_labels(Region = "") %>%
  set_header_labels(Partnerships = "Partnership Type") %>%
  add_footer_row(
    values = paste0(
      "Table 2.7 is not in the 2024 Annual Report. Table shows partnerships in country-led examples, disaggregated by type of partnership (with academia, international or national organisations) and year, as well as partnerships per regions per year."
    ),
    colwidths = ncol(final_table2)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.7: Overview of country-led examples with implementation partnerships",
        props = fp_text(font.family = "Helvetica", font.size = 10)
        )
      )
    ) %>%
  fix_border_issues()

ar.2.7

