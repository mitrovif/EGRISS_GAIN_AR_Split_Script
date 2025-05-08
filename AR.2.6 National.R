
# ============================================================================================================
# AR.2.6: Breakdown of Nationally Led Partnerships
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
  "PRO18.A" = "National Partnership",
  "PRO18.B" = "International Organization Partnership",
  "PRO18.C" = "Academia Partnership"
)

# Define Year Order
year_order <- c("2021", "2022", "2023", "2024")

# Count total nationally led projects
nationally_led_count <- data %>%
  filter(g_conled == 1) %>%
  count(ryear) %>%
  pivot_wider(names_from = ryear, values_from = n, values_fill = 0) %>%
  mutate(Partnership_Type = "Total Nationally Led Projects")

# Count total nationally led projects with partnerships
partnership_count <- data %>%
  filter(g_conled == 1, PRO17 == 1) %>%
  count(ryear) %>%
  pivot_wider(names_from = ryear, values_from = n, values_fill = 0) %>%
  mutate(Partnership_Type = "Total Nationally Led Projects with Partnerships")

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
  select('Partnership Type', all_of(year_order))

# Create FlexTable with EGRISS Color Scheme
ar.2.6 <- flextable(partnership_data) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  fontsize(size = 10, part = "all") %>%
  bg(bg = "white", part = "body") %>%  # Apply background color
  bg(bg = primary_color, part = "header") %>%  # Apply primary color to header
  color(color = "black", part = "header") %>%  # Set header text color to black
  # bold(j = 1, part = "body") %>%  # Bold the first column (Partnership Type)
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  set_caption("Breakdown of Nationally Led Partnerships by Year and Type") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.2.6: Overview of country-led implementation partnerships, by year and type (AR pg 28)",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  add_footer_row(
    values = paste0(
      "Footnote: Counts are based on projects with g_conled == 1 (nationally led) in analysis_ready_group_roster.csv. ",
      "“Total Nationally Led Projects” = all country-led initiatives; “with Partnerships” = subset where PRO17 == 1. ",
      "Partnership types are coded as: National Partnership (PRO18.A), International Organization Partnership (PRO18.B), ",
      "Academia Partnership (PRO18.C). Years 2021–2024 correspond to the ryear field. Inner counts reflect the ",
      "number of nationally led projects each year by partnership type."
    ),
    colwidths = ncol(partnership_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  fix_border_issues()%>%
  autofit()

# Display Table in RStudio Viewer (for verification)
ar.2.6
