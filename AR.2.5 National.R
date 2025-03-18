
# ============================================================================================================
# Breakdown of Nationally Led Partnerships
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
  select(Partnership_Type, all_of(year_order))

# Create FlexTable with EGRISS Color Scheme
partnership_flextable <- flextable(partnership_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = background_color, part = "body") %>%  # Apply background color
  bg(bg = primary_color, part = "header") %>%  # Apply primary color to header
  color(color = "white", part = "header") %>%  # Set header text color to white
  bold(j = 1, part = "body") %>%  # Bold the first column (Partnership Type)
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = accent_color, width = 1.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = secondary_color, width = 1)) %>%
  set_caption("Breakdown of Nationally Led Partnerships by Year and Type") %>%
  autofit()

# Display Table in RStudio Viewer (for verification)
partnership_flextable
