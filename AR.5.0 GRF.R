# ============================================================================================================
# AR.5.0: Overview of GRF Pledges
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

source_summary <- stat_pledges %>%
  filter(source_pledge == 2) %>%
  mutate(`GRF Data on Pledges` = "GAIN Survey Data") %>%
  select(`GRF Data on Pledges`, region, Submitting.Entity.Type, stage_final) %>%
  group_by(`GRF Data on Pledges`, region, Submitting.Entity.Type) %>%
  summarise(
    `Planning stage`               = sum(stage_final == "Planning stage",    na.rm = TRUE),
    `Implementation (In progress)` = sum(stage_final == "In progress",       na.rm = TRUE),
    `Completed (Fulfilled)`        = sum(stage_final == "Fulfilled",         na.rm = TRUE),
    Reported                       = sum(stage_final %in% c("Planning stage", "In progress", "Fulfilled"), na.rm = TRUE),
    Total_Pledges                  = n(),
    .groups = "drop"
  ) %>%
  select(c(-"GRF Data on Pledges")) %>%
  relocate(`Submitting.Entity.Type`) %>%
  rename(`Example Lead` = `Submitting.Entity.Type`,
         Region = region,
         `Total Pledges` = Total_Pledges) %>%
  mutate(`Example Lead` = case_when(
    `Example Lead` == "States" ~ "Country-Led Examples",
    .default = "Institutional-Led Examples"
  ))

# Create the flextable
ar.5.0 <- flextable(source_summary) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%   
  bold(part = "header") %>%
  bg(part = "header", bg = header_color) %>%  # Light Blue Header
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer Border for Entire Table
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(bg = "#c9daf8", j = ~ `Total Pledges`) %>%  # Highlight the totals column
  merge_v(j = ~ `Example Lead`) %>%  # Merge vertical cells for Example Lead
  autofit() %>%
  # add_footer_row(
  #   values = paste0(
  #     "..."
  #   ),
  #   colwidths = ncol(source_summary)
  # ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table x.x: Overview of Examples directly related to GRF pledges",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# View flextable
print(ar.5.0)
