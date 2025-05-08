
# ======================================================
# AR: 7.1: Script: Calculate `example_duration` from PRO04_year and PRO05_year
# Description:
# - If either year is NA → NA
# - If either year is "9999" or "9998" → 99
# - Otherwise → PRO05_year - PRO04_year
# ======================================================

# Load required libraries
library(dplyr)

# Define file path
group_roster_file <- "analysis_ready_group_roster.csv"

# Load dataset
group_roster <- read.csv(group_roster_file, stringsAsFactors = FALSE)

# Create `example_duration`
group_roster <- group_roster %>%
  mutate(
    example_duration = case_when(
      is.na(PRO04_year) | is.na(PRO05_year) ~ NA_real_,
      PRO04_year %in% c("9999", "9998") | PRO05_year %in% c("9999", "9998") ~ 99,
      TRUE ~ as.numeric(PRO05_year) - as.numeric(PRO04_year)
    )
  )

# === Preview before saving ===
cat("\n=== Preview of `example_duration` ===\n")
print(head(group_roster[c("PRO04_year", "PRO05_year", "example_duration")], 20))

cat("\n=== Frequency table of `example_duration` ===\n")
print(table(group_roster$example_duration, useNA = "ifany"))

group_roster <- group_roster %>%
  mutate(
    example_duration_category = case_when(
      is.na(example_duration) ~ "Undetermined",
      example_duration == 99 ~ "No end planned",
      example_duration %in% c(0, 1) ~ "Up to 1 year to finish",
      example_duration %in% c(2, 3) ~ "2 up to 3 years to finish",
      example_duration %in% c(4, 5) ~ "4 up to 5 years to finish",
      example_duration >= 6 ~ "More than 5 years to finish",
      TRUE ~ "Other"  # fallback just in case
    )
  )

cat("\n=== Frequency table of `example_duration_category` ===\n")
print(table(group_roster$example_duration_category, useNA = "ifany"))

library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Load data
group_roster_file <- "analysis_ready_group_roster.csv"
group_roster <- read.csv(group_roster_file, stringsAsFactors = FALSE)

# Step 1: National Examples (g_conled == 1)

# Create example_duration_category if it's not already in the dataset
group_roster <- group_roster %>%
  mutate(
    example_duration = case_when(
      is.na(PRO04_year) | is.na(PRO05_year) ~ NA_real_,
      PRO04_year %in% c("9999", "9998") | PRO05_year %in% c("9999", "9998") ~ 99,
      TRUE ~ as.numeric(PRO05_year) - as.numeric(PRO04_year)
    ),
    example_duration_category = case_when(
      is.na(example_duration) ~ "Undetermined",
      example_duration == 99 ~ "No end planned",
      example_duration %in% c(0, 1) ~ "Up to 1 year to finish",
      example_duration %in% c(2, 3) ~ "2 up to 3 years to finish",
      example_duration %in% c(4, 5) ~ "4 up to 5 years to finish",
      example_duration >= 6 ~ "More than 5 years to finish",
      TRUE ~ "Other"
    )
  )
aggregated_national <- group_roster %>%
  filter(g_conled == 1) %>%
  mutate(across(starts_with("PRO08."), as.integer)) %>%
  pivot_longer(
    cols = starts_with("PRO08."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Source = case_when(
      grepl("PRO08.A", Source_Variable) ~ "Survey",
      grepl("PRO08.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08.C", Source_Variable) ~ "Census",
      grepl("PRO08.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08.E|PRO08.F|PRO08.G|PRO08.H|PRO08.X", Source_Variable) ~ "Other",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using Recommendations and Other",
      TRUE ~ "Not Using Recommendations and Other"
    )
  ) %>%
  group_by(`Use of Recommendations`, Source, example_duration_category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = example_duration_category,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -c(`Use of Recommendations`, Source)), na.rm = TRUE)) %>%
  mutate(`Example Category` = "Graph Data National Examples")

# Step 2: Institutional Examples (g_conled == 2 or 3)
aggregated_institutional <- group_roster %>%
  filter(g_conled %in% c(2, 3)) %>%
  mutate(across(starts_with("PRO08."), as.integer)) %>%
  pivot_longer(
    cols = starts_with("PRO08."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Source = case_when(
      grepl("PRO08.A", Source_Variable) ~ "Survey",
      grepl("PRO08.B", Source_Variable) ~ "Administrative Data",
      grepl("PRO08.C", Source_Variable) ~ "Census",
      grepl("PRO08.D", Source_Variable) ~ "Data Integration",
      grepl("PRO08.E|PRO08.F|PRO08.G|PRO08.H|PRO08.X", Source_Variable) ~ "Other",
      TRUE ~ "Unknown"
    ),
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using Recommendations and Other",
      TRUE ~ "Not Using Recommendations and Other"
    )
  ) %>%
  group_by(`Use of Recommendations`, Source, example_duration_category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = example_duration_category,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -c(`Use of Recommendations`, Source)), na.rm = TRUE)) %>%
  mutate(`Example Category` = "Overall Institution Examples")

# after your bind_rows(...) and arrange(...) step, insert:
aggregated_data <- bind_rows(aggregated_national, aggregated_institutional) %>%
  mutate(
    `Use of Recommendations` = factor(
      `Use of Recommendations`,
      levels = c("Using Recommendations", "Not Using Recommendations and Other")
    )
  ) %>%
  arrange(
    `Example Category`,
    `Use of Recommendations`,
    factor(Source, levels = c("Survey", "Census", "Administrative Data", "Data Integration", "Other"))
  ) %>%
  # ← ensure Example Category is first
  select(`Example Category`, everything())

# now build the flextable with footnote
ar.7.1 <- flextable(aggregated_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  merge_v(j = ~ `Example Category`) %>%
  merge_v(j = ~ `Use of Recommendations`) %>%
  bg(bg = "#f4cccc", j = ~ Total) %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 8) %>%
  autofit() %>%
  # highlight rows as before…
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" &
                     aggregated_data$`Use of Recommendations` == "Using Recommendations"),
         border.top = solid_border, border.bottom = solid_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" &
                     aggregated_data$`Use of Recommendations` == "Not Using Recommendations and Other"),
         border.top = dashed_border, border.bottom = dashed_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Overall Institution Examples"),
         border.top = default_border, border.bottom = default_border) %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  
  # ← new detailed footnote
  add_footer_row(
    values = paste0(
      "Footnote: “Example Category” distinguishes nationally-led vs institutionally-led examples. ",
      "“Use of Recommendations” is PRO09==1 (Using) vs PRO09==2,8 (Not Using/Other). ",
      "“Source” derives from PRO08.* flags (Survey, Census, Administrative Data, Data Integration, Other). ",
      "“Example Duration Category” bins years between PRO04_year and PRO05_year (NA→Undetermined, 9998/9999→No end planned, ",
      "0–1 yrs, 2–3 yrs, 4–5 yrs, >5 yrs). Counts are number of examples per combination."
    ),
    colwidths = ncol(aggregated_data)
  ) %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  fontsize(size = 7, part = "footer") %>%
  
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.7.1: Length of implementation by use of recommendations and example type (Not in AR)",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  ) %>%
  set_table_properties(
    width = 1,           # 100% of page width
    layout = "autofit"   # auto‐adjust column widths
  ) %>%
  fix_border_issues()
# then view
ar.7.1
