
# ============================================================================================================
# AR.2.5: Table of Implementation Challenges (PRO22) by Example Lead
# ============================================================================================================

library(dplyr)
library(stringr)
library(tidyr)
library(flextable)

response_labels <- c(
  "Challenges faced" = "Challenges faced",
  "No challenges faced" = "No challenges faced",
  "No Response on Challenges or Don't Know" = "No Response on Challenges or Don't Know"
)

pro21_summary <- group_roster %>%
  filter(ryear %in% c(2023, 2024), g_conled == 1) %>%
  select(ryear, PRO21) %>%
  mutate(`Implementation Challenge` = case_when(
    PRO21 == 2 | PRO21 == "NO" ~ "No challenges faced",
    PRO21 == 1 | PRO21 == "YES" ~ "Challenges faced",
    PRO21 == 9 | PRO21 == "NO RESPONSE" ~ "No Response on Challenges or Don't Know",
    PRO21 == 8 | PRO21 == "DON'T KNOW" ~ "No Response on Challenges or Don't Know",
    is.na(PRO21) ~ "No Response on Challenges or Don't Know",
    TRUE ~ as.character(PRO21)
  )) %>%
  mutate(`Implementation Challenge` = recode(`Implementation Challenge`, !!!response_labels)) %>%
  group_by(`Implementation Challenge`, ryear) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(`Implementation Challenge` = names(response_labels), ryear = c(2023, 2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = ryear, values_from = Count)

pro21_summary <- pro21_summary %>%
  mutate(across(c(`2023`, `2024`), as.character)) %>%
  mutate(`Example Lead` = "Overall") %>%
  relocate(`Example Lead`)

# Helper: Map raw categories to descriptive labels
map_challenge <- function(df) {
  df %>%
    mutate(
      `Implementation Challenge` = case_when(
        Category == "PRO22A" ~ "Lack of clarity (on how to implement)",
        Category == "PRO22B" ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "PRO22C" ~ "Lack of technical support",
        Category == "PRO22D" ~ "Organizational resistance",
        Category == "PRO22E" ~ "Lack of training and capacity building opportunities",
        Category == "PRO22F" ~ "Lack of peer-to-peer learning opportunities",
        Category == "PRO22X" ~ "Other",
        Category == "1) different uses of the idp definition in data collection," ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "1- gap in the understanding of recommendations;" ~ "Lack of training and capacity building opportunities",
        Category == "2) lack of coordination on data collection between national and local level," ~ "Other",
        Category == "2- strengthening the capacity of regional focal points;" ~ "Lack of training and capacity building opportunities",
        Category == "3) lack of a harmonise statistical approach to durable solutions." ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "3- dissemination of the recommendations within member states" ~ "Other",
        Category == "available administrative data do not allow to identify 'persons without a recognized nationality status'. the focus of the press release was therefore on stateless persons." ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "definitional inconsistencies between countries." ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "limited possibility for including all key questions from irrs/iris in the questionnaire given that it is a census." ~ "Lack of further guidance (not sufficient methodological information)",
        Category == "sensitivity of egriss-recommended identification" ~ "Other",
        Category == "sensitivity of of egriss-recommended identification" ~ "Other",
        Category == "some of the recommendations are not currently or fully measured at statistics canada (e.g. stocks of persons returned after having sought international protection abroad or internally displaced persons)or" ~ "Other",
        Category == "timeliness and data quality" ~ "Other",
        Category == "within the government institution such ncri and even non government institution that misunderstood that idps statistics strategic plan is risk to their work" ~ "Lack of clarity (on how to implement)",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(`Implementation Challenge`) %>%
    summarise(Responses = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Responses))
}

# Function to summarise PRO22 fields for a given subset
summarise_pro22 <- function(data) {
  # Identify PRO22 structured columns
  structured_cols <- grep("^PRO22[A-Z]$", names(data), value = TRUE)
  # Filter to only respondents with at least one PRO22 structured response
  data <- data %>%
    filter(rowSums(select(., all_of(structured_cols)), na.rm = TRUE) > 0 | !is.na(PRO22))
  
  # Structured coded responses
  structured_summary <- data %>%
    select(all_of(structured_cols)) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Category", values_to = "Count")
  
  # Free-text responses: Same cleaning steps as your script
  free_text_summary <- data %>%
    filter(!is.na(PRO22)) %>%
    # Exclude all-uppercase entries
    filter(!str_detect(PRO22, "^[[:upper:]\\s[:punct:]]+$")) %>%
    # Lowercase and trim
    mutate(PRO22_clean = str_squish(tolower(PRO22))) %>%
    # Add a fake split marker before "2 -" / "2)" / "3 -" / "3)"
    mutate(PRO22_clean = str_replace_all(PRO22_clean, "(?=\\b[23]\\s*[-\\)])", "|||")) %>%
    # Split on the custom marker
    separate_rows(PRO22_clean, sep = "\\|\\|\\|") %>%
    mutate(PRO22_clean = str_squish(PRO22_clean)) %>%
    filter(PRO22_clean != "") %>%
    count(PRO22_clean, sort = TRUE) %>%
    rename(Category = PRO22_clean, Count = n)
  
  # Combine and map challenges with custom re-categorization
  bind_rows(structured_summary, free_text_summary) %>%
    map_challenge()
}

# Create a helper to run summarise_pro22 by year and group type
summarise_by_group_year <- function(data, group_label) {
  data %>%
    group_by(year) %>%
    group_map(~ summarise_pro22(.x) %>% mutate(`Example Lead` = group_label, year = .y$year), .keep = TRUE) %>%
    bind_rows()
}

# Run for each group type
overall_df <- summarise_by_group_year(group_roster, "Overall")
national_df <- summarise_by_group_year(filter(group_roster, g_conled == 1), "Nationally Led Examples")
institution_df <- summarise_by_group_year(filter(group_roster, g_conled %in% c(2,3)), "Institutionally Led Examples")

# Combine all
final_pro22 <- bind_rows(overall_df, national_df, institution_df) %>%
  select(year, `Example Lead`, `Implementation Challenge`, Responses) %>%
  filter(Responses > 0)

final_wide <- final_pro22 %>%
  pivot_wider(
    names_from = year,
    values_from = Responses
  ) %>%
  mutate(across(starts_with("Responses "), ~ replace_na(.x, 0))) %>%
  arrange(`Example Lead`, `Implementation Challenge`) %>%
  mutate(across(c(`2023`, `2024`), as.character)) %>%
  replace(is.na(.), "0")

# Reorder 'Example Lead' as a factor with desired order
final_wide <- final_wide %>%
  mutate(`Example Lead` = factor(`Example Lead`,
                                 levels = c("Overall", "Nationally Led Examples", "Institutionally Led Examples"))) %>%
  arrange(`Example Lead`)


# Combining Both Tables into One Stacked Table
combined_data <- bind_rows(
  tibble(`Example Lead` = "Count of Respondents Facing Challenges", `2023` = "", `2024` = ""),
  pro21_summary,
  tibble(`Example Lead` = "", `2023` = "", `2024` = ""),  # Spacer row
  tibble(`Example Lead` = "Types of Challenges Identified", `2023` = "", `2024` = ""),
  final_wide) %>%
  relocate(`Example Lead`, `Implementation Challenge`) %>%
  rename(`EGRISS Recommendations Implementation Challenges` = `Implementation Challenge`)

combined_data <- bind_rows(
  tibble(
    `Example Lead` = "Count of Respondents Facing Challenges",
    `2023` = "Count of Respondents Facing Challenges",
    `2024` = "Count of Respondents Facing Challenges",
    `Implementation Challenge` = "Count of Respondents Facing Challenges"
  ),
  pro21_summary,
  tibble(`Example Lead` = "", `2023` = "", `2024` = ""),  # Spacer row
  tibble(
    `Example Lead` = "Types of Challenges Identified",
    `2023` = "Types of Challenges Identified",
    `2024` = "Types of Challenges Identified",
    `Implementation Challenge` = "Types of Challenges Identified"
  ),
  final_wide) %>%
  relocate(`Example Lead`, `Implementation Challenge`) %>%
  rename(`EGRISS Recommendations Implementation Challenges` = `Implementation Challenge`)


# Create FlexTable for Word
ar.2.5 <- flextable(combined_data) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(i = 1, bg = "#D9D9D9") %>%
  bg(i = 6, bg = "#D9D9D9") %>%
  merge_v(j = ~ `Example Lead`) %>%
  merge_h(i = 1) %>%
  merge_h(i = 6) %>%
  bold(i = c(1, 6), bold = TRUE, part = "body") %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: Structured PRO22A–F and cleaned free-text PRO22 entries. ",
      "Split by year and example lead type. Years with no responses are blank."
    ),
    colwidths = ncol(final_wide)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.2.5: Breakdown of Implementation Challenges by Example Lead and Year (Wide Format)",
        props = fp_text(font.family = "Helvetica", font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

# Display the table
ar.2.5
