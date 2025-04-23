# ======================================================
# Script: Calculate `example_duration` from PRO04_year and PRO05_year
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

# Step 3: Combine national + institutional
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
  )

# Step 4: Create FlexTable
solid_border <- fp_border(color = "#3b71b3", width = 2, style = "solid")
dashed_border <- fp_border(color = "#3b71b3", width = 2, style = "dashed")
default_border <- fp_border(color = "black", width = 0.5)

figure_flextable <- flextable(aggregated_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  merge_v(j = ~ `Example Category`) %>%
  merge_v(j = ~ `Use of Recommendations`) %>%
  bg(bg = "#f4cccc", j = ~ Total) %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner(border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 8) %>%
  autofit() %>%
  # Graph Data National Examples
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" &
                     aggregated_data$`Use of Recommendations` == "Using Recommendations"),
         border.top = solid_border,
         border.bottom = solid_border) %>%
  border(i = which(aggregated_data$`Example Category` == "Graph Data National Examples" &
                     aggregated_data$`Use of Recommendations` == "Not Using Recommendations and Other"),
         border.top = dashed_border,
         border.bottom = dashed_border) %>%
  # Overall Institution Examples
  border(i = which(aggregated_data$`Example Category` == "Overall Institution Examples"),
         border.top = default_border,
         border.bottom = default_border) %>%
  add_footer_row(
    values = paste0(
      "Graph Data National Examples are categorized by time until project conclusion. ",
      "Sources represent data types used in examples: ",
      "• Survey, Census, Administrative Data, Data Integration, and Other (PRO08.*). ",
      "This is a multiple-response question, meaning one example can feature multiple sources or tools."
    ),
    colwidths = ncol(aggregated_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption("Figure: Sources Used by Project Duration Category")

# View the result
print(figure_flextable)


