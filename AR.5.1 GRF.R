
# ============================================================================================================
# AR.5.1: Breakdown of GRF Pledges
# ============================================================================================================

# Load necessary libraries
library(readxl)
library(dplyr)
library(flextable)

# File paths
pledge_data_path <- "Statistical_Inclusion_Pledge_Data.xlsx"
repeat_pledges_path <- "repeat_pledges_cleaned.csv"

# Read the data files
stat_pledges <- read.csv("Statistical_Inclusion_Pledges_Updated.csv")

# Clean data
stat_pledges <- stat_pledges %>%
  mutate(
    `region` = ifelse(is.na(`region`), "Region/Country not Reported", `region`),
    `Submitting.Entity.Type` = ifelse(is.na(`Submitting.Entity.Type`), "Region/Country not Reported", `Submitting.Entity.Type`)
  )

# Summary by Region
region_summary <- stat_pledges %>%
  group_by(`region`) %>%
  summarise(
    `Design/Planning (Planning stage)` = sum(prog_gain == "DESIGN/PLANNING", na.rm = TRUE),
    `Implementation (In progress)` = sum(prog_gain == "IMPLEMENTATION", na.rm = TRUE),
    `Completed (Fulfilled)` = sum(prog_gain == "COMPLETED", na.rm = TRUE),
    Reported = sum(prog_gain %in% c("DESIGN/PLANNING", "IMPLEMENTATION", "COMPLETED"), na.rm = TRUE),
    Total_Pledges = n()
  )

# Summary by Submitting.Entity.Type
entity_summary <- stat_pledges %>%
  group_by(`Submitting.Entity.Type`) %>%
  summarise(
    `Design/Planning (Planning stage)` = sum(prog_gain == "DESIGN/PLANNING", na.rm = TRUE),
    `Implementation (In progress)` = sum(prog_gain == "IMPLEMENTATION", na.rm = TRUE),
    `Completed (Fulfilled)` = sum(prog_gain == "COMPLETED", na.rm = TRUE),
    Reported = sum(prog_gain %in% c("DESIGN/PLANNING", "IMPLEMENTATION", "COMPLETED"), na.rm = TRUE),
    Total_Pledges = n()
  )

# Add total row
total_row <- stat_pledges %>%
  summarise(
    `GFR Data on Pledges` = "Total",
    `Design/Planning (Planning stage)` = sum(prog_gain == "DESIGN/PLANNING", na.rm = TRUE),
    `Implementation (In progress)` = sum(prog_gain == "IMPLEMENTATION", na.rm = TRUE),
    `Completed (Fulfilled)` = sum(prog_gain == "COMPLETED", na.rm = TRUE),
    Reported = sum(prog_gain %in% c("DESIGN/PLANNING", "IMPLEMENTATION", "COMPLETED"), na.rm = TRUE),
    Total_Pledges = n()
  )

# Combine Data with Headers for Sections
region_header <- tibble(`GFR Data on Pledges` = "Summary by Region", .rows = 1)
entity_header <- tibble(`GFR Data on Pledges` = "Summary by Submitting Entity Type", .rows = 1)

# Rename labels and combine data
region_summary <- region_summary %>%
  rename(`GFR Data on Pledges` = `region`)

entity_summary <- entity_summary %>%
  rename(`GFR Data on Pledges` = `Submitting.Entity.Type`)

# Final merged table
merged_summary <- bind_rows(
  total_row,
  region_header,
  region_summary,
  entity_header,
  entity_summary
)

# Find the row index where "South America" appears
south_america_index <- which(merged_summary$`GFR Data on Pledges` == "South America")

# Create a blank row (same structure as the existing data)
blank_row <- merged_summary[1, ]
blank_row[,] <- NA  # Set all values to NA or "" as needed

# Insert the blank row after "South America"
merged_summary <- bind_rows(
  merged_summary[1:south_america_index, ],
  blank_row,
  merged_summary[(south_america_index + 1):nrow(merged_summary), ]
) %>%
  rename('Total Pledges' = Total_Pledges)

# Styling Variables
egriss_color <- "#003366"  # EGRISS dark blue
section_header_color <- "#D9D9D9"

# Create FlexTable with Styling
ar.5.1 <- flextable(merged_summary) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer Border for Entire Table
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  border(i = 1, border.bottom = fp_border(color = "black", width = 2), part = "body") %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 0.6) %>%  # New Table Sizing Control
  bold(i = 1, part = "body") %>%  # Bold Total Row
  bg(i = 1, bg = egriss_color, part = "body") %>%  # Total Row in EGRISS Color
  color(i = 1, color = "white", part = "body") %>%  # Text color for Total row
  bg(i = 2, bg = section_header_color, part = "body") %>%  # Region Section Header
  bg(i = nrow(region_summary) + 4, bg = section_header_color, part = "body") %>%  # Entity Section Header
  add_footer_row(
    values = paste0(
      "Footnote: Data is based on GAIN Survey 2024 analysis of Statistical Inclusion Pledges. ",
      "The merged table presents the breakdown by Region and Submitting Entity Type. ",
      "Regions labeled 'Region/Country not Reported' represent cases where no geographic location or entity was specified."
    ),
    colwidths = ncol(merged_summary)  # Ensure footer spans the full table width
  ) %>%
  fontsize(size = 7, part = "footer") %>%
set_caption(
  caption = as_paragraph(
    as_chunk(
      "AR.5.1: GFR Data on Pledges Implementation, by stage, region, and entity type ",
      props = fp_text(
        font.family = "Helvetica",
        font.size   = 10,
        italic      = FALSE
      )
    )
  )
)
# Display Table
print(ar.5.1)
