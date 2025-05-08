# ============================================================================================================
# AR.2.5: Table of Implementation Challenges (PRO22) by Example Lead
# ============================================================================================================

library(dplyr)
library(stringr)
library(tidyr)
library(flextable)

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
    filter(rowSums(select(., all_of(structured_cols)), na.rm = TRUE) > 0)
  
  # Structured coded responses
  structured_summary <- data %>%
    select(all_of(structured_cols)) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Category", values_to = "Count")
  
  # Free-text responses
  free_text <- data %>%
    filter(!is.na(PRO22)) %>%
    filter(!str_detect(PRO22, "^[[:upper:]\\s[:punct:]]+$")) %>%
    mutate(
      txt = str_squish(tolower(PRO22)),
      txt = str_replace_all(txt, "(?=\\b[23]\\s*[-\\)])", "|||")
    ) %>%
    separate_rows(txt, sep = "\\|\\|\\|") %>%
    filter(txt != "") %>%
    count(txt, name = "Count") %>%
    rename(Category = txt)
  
  # Combine and map challenges
  bind_rows(structured_summary, free_text) %>%
    map_challenge()
}

# Generate summaries for each group
overall_df <- summarise_pro22(group_roster) %>% mutate(`Example Lead` = "Overall")
national_df <- summarise_pro22(filter(group_roster, g_conled == 1)) %>% mutate(`Example Lead` = "Nationally Led Examples")
institution_df <- summarise_pro22(filter(group_roster, g_conled %in% c(2,3))) %>% mutate(`Example Lead` = "Institutionally Led Examples")

# Combine and suppress zero counts
final_pro22 <- bind_rows(overall_df, national_df, institution_df) %>%
  select(`Example Lead`, `Implementation Challenge`, Responses) %>%
  filter(Responses > 0)

# Create FlexTable for Word
ar.2.5 <- flextable(final_pro22) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "all", border = fp_border(color = "gray", width = 0.5)) %>%
  merge_v(j = ~ `Example Lead`) %>%
  autofit() %>%
  # add detailed footnote
  add_footer_row(
    values = paste0(
      "Footnote: Counts combine structured PRO22A–F responses (each flag = 1) and free-text PRO22 entries, ",
      "cleaned and aggregated. “Overall” includes all respondents; “Nationally Led Examples” is g_conled == 1; ",
      "“Institutionally Led Examples” is g_conled %in% c(2,3). Only challenges with at least one response are shown."
    ),
    colwidths = ncol(final_pro22)
  ) %>%
  fontsize(size = 7, part = "footer") %>%set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.2.5: Breakdown of Implementation Challenges of Recommendations, by example lead (Not in AR)",
        props = fp_text(font.family = "Helvetica", font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

# Display the table
ar.2.5
