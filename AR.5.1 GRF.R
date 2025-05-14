# ============================================================================================================
# AR.5.1: Breakdown of GRF Pledges
# ============================================================================================================

# Load necessary libraries
library(readxl)
library(dplyr)
library(flextable)

# File paths
updated_csv_path <- "Statistical_Inclusion_Pledges_Updated.csv"

# Read the updated pledges file
stat_pledges <- read.csv(updated_csv_path, stringsAsFactors = FALSE)

# Clean data: fill missing region or entity‐type
stat_pledges <- stat_pledges %>%
  mutate(
    region                  = ifelse(is.na(region), "Region/Country not Reported", region),
    Submitting.Entity.Type  = ifelse(is.na(Submitting.Entity.Type), "Region/Country not Reported", Submitting.Entity.Type)
  )

# Prepare summaries ----------------------------------------------------------------------------------------

# 1. Source_summary: overall counts by source_pledge
source_summary <- stat_pledges %>%
  group_by(source_pledge) %>%
  summarise(
    `Planning stage`               = sum(stage_final == "Planning stage",    na.rm = TRUE),
    `Implementation (In progress)` = sum(stage_final == "In progress",        na.rm = TRUE),
    `Completed (Fulfilled)`        = sum(stage_final == "Fulfilled",          na.rm = TRUE),
    Reported                       = sum(stage_final %in% c("Planning stage","In progress","Fulfilled"), na.rm = TRUE),
    Total_Pledges                  = n()
  ) %>%
  mutate(
    `GRF Data on Pledges` = case_when(
      source_pledge == 1 ~ "GRF Database",
      source_pledge == 2 ~ "GAIN Survey Data",
      TRUE               ~ "No Data Available"
    )
  ) %>%
  select(`GRF Data on Pledges`, everything(), -source_pledge)

# 2. Region summary
region_summary <- stat_pledges %>%
  group_by(region) %>%
  summarise(
    `Planning stage`               = sum(stage_final == "Planning stage",    na.rm = TRUE),
    `Implementation (In progress)` = sum(stage_final == "In progress",        na.rm = TRUE),
    `Completed (Fulfilled)`        = sum(stage_final == "Fulfilled",          na.rm = TRUE),
    Reported                       = sum(stage_final %in% c("Planning stage","In progress","Fulfilled"), na.rm = TRUE),
    Total_Pledges                  = n()
  ) %>%
  rename(`GRF Data on Pledges` = region)

# 3. Entity summary
entity_summary <- stat_pledges %>%
  group_by(Submitting.Entity.Type) %>%
  summarise(
    `Planning stage`               = sum(stage_final == "Planning stage",    na.rm = TRUE),
    `Implementation (In progress)` = sum(stage_final == "In progress",        na.rm = TRUE),
    `Completed (Fulfilled)`        = sum(stage_final == "Fulfilled",          na.rm = TRUE),
    Reported                       = sum(stage_final %in% c("Planning stage","In progress","Fulfilled"), na.rm = TRUE),
    Total_Pledges                  = n()
  ) %>%
  rename(`GRF Data on Pledges` = Submitting.Entity.Type)

# 4. Total row
total_row <- stat_pledges %>%
  summarise(
    `GRF Data on Pledges`          = "Total",
    `Planning stage`               = sum(stage_final == "Planning stage",    na.rm = TRUE),
    `Implementation (In progress)` = sum(stage_final == "In progress",        na.rm = TRUE),
    `Completed (Fulfilled)`        = sum(stage_final == "Fulfilled",          na.rm = TRUE),
    Reported                       = sum(stage_final %in% c("Planning stage","In progress","Fulfilled"), na.rm = TRUE),
    Total_Pledges                  = n()
  )

# Section headers
source_header <- tibble(`GRF Data on Pledges` = "Source of Data", .rows = 1)
region_header <- tibble(`GRF Data on Pledges` = "Summary by Region", .rows = 1)
entity_header <- tibble(`GRF Data on Pledges` = "Summary by Submitting Entity Type", .rows = 1)

# Build final table ----------------------------------------------------------------------------------------

merged_summary <- bind_rows(
  total_row,
  source_header,
  source_summary,
  region_header,
  region_summary,
  entity_header,
  entity_summary
) %>%
  rename(`Total Pledges` = Total_Pledges)

# Styling constants
egriss_color         <- "#003366"
section_header_color <- "#D9D9D9"

# Create and style flextable -----------------------------------------------------------------------------

ar.5.1 <- flextable(merged_summary) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 0.6) %>%
  # Style total row
  bold(i = 1, part = "body") %>%
  bg(i = 1, bg = egriss_color, part = "body") %>%
  color(i = 1, color = "white", part = "body") %>%
  # Style source header row only
  bg(i = ~ `GRF Data on Pledges` == "Source of Data", bg = section_header_color, part = "body") %>%
  # Optional: bold the section header text
  bold(i = ~ `GRF Data on Pledges` %in% c("Source of Data","Summary by Region","Summary by Submitting Entity Type"), part = "body") %>%
  # Footer
  add_footer_row(
    values = paste0(
      "Footnote: Data is based on GAIN Survey 2024 analysis of Statistical Inclusion Pledges. ",
      "The merged table presents the breakdown by data source, region, and submitting entity type."
    ),
    colwidths = ncol(merged_summary)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.5.1: GRF Data on Pledges Implementation, by source, stage, region, and entity type",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10
        )
      )
    )
  )

# Display the table
print(ar.5.1)
