# =============================================================================================================
# Add Memberships - Summary by Year and Organization Type (Merged Groups)
# =============================================================================================================

# Custom Colors
header_color <- "#4cc3c9"
gray_highlight <- "#D9D9D9"
border_style <- fp_border(color = "black", width = 1)

# Load data
main_roster_file <- file.path(working_dir, "analysis_ready_main_roster.csv")
main_roster <- read.csv(main_roster_file)

# Filter and categorize interest data
future_interest_summary <- main_roster %>%
  select(c("ACT02", "mcountry", "morganization", "year", "LOC01")) %>%
  mutate(
    Interest = case_when(
      ACT02 %in% c(1, "YES") ~ "Interested in Learning about Membership",
      ACT02 %in% c(2, "NO", 8, "DON'T KNOW") ~ "Not Interested in Learning about Membership or Don't Know",
      ACT02 == 9 | ACT02 == "Unknown" ~ NA_character_,  
      .default = ACT02
    ),
    Organization_Type = case_when(
      LOC01 == 1 ~ "NSO",
      LOC01 %in% c(2, 3) ~ "International and Other Organizations",
      .default = "Other"
    )
  ) %>%
  filter(!is.na(Interest), year %in% c(2023, 2024)) %>%
  group_by(Interest, Organization_Type, year, .drop = FALSE) %>%
  summarize(`Number of Institutions` = n(), .groups = "drop") %>%
  complete(Interest, Organization_Type, year = c(2023, 2024), fill = list(`Number of Institutions` = 0)) %>%
  pivot_wider(names_from = year, values_from = `Number of Institutions`, values_fill = 0) %>%
  rename(`Year 2023` = `2023`, `Year 2024` = `2024`) %>%
  mutate(across(c(`Year 2023`, `Year 2024`), as.character))  # Convert both to character for consistency

# Suppress redundant interest labels
future_interest_summary <- future_interest_summary %>%
  mutate(Interest = replace(Interest, duplicated(Interest), ""))

# ---------------------------------------------------------------------------------------
# SECOND TABLE: List of Interested Organizations and Countries
# ---------------------------------------------------------------------------------------

# Extract list of interested organizations and countries
interested_organizations <- main_roster %>%
  filter(ACT02 %in% c(1, "YES"), year %in% c(2023, 2024)) %>%
  mutate(
    Interest = "Interested in Learning about Membership",
    Organization_Type = case_when(
      LOC01 == 1 ~ "NSO",
      LOC01 %in% c(2, 3) ~ "International and Other Organizations",
      .default = "Other"
    )
  ) %>%
  group_by(Interest, Organization_Type, year) %>%
  summarize(
    `List of Organizations and Countries` = paste(
      unique(paste(morganization, "(", mcountry, ")", sep = "")), 
      collapse = "; "),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = `List of Organizations and Countries`, values_fill = "None") %>%
  rename(`Year 2023` = `2023`, `Year 2024` = `2024`) %>%
  mutate(across(c(`Year 2023`, `Year 2024`), as.character))  # Convert both to character for consistency

# Combine both data tables
combined_data <- bind_rows(
  future_interest_summary,
  data.frame(
    Interest = "List of Interested Organizations",
    Organization_Type = "",
    `Year 2023` = "",
    `Year 2024` = ""
  ),
  interested_organizations
)

# Create FlexTable for Combined Table - AR.6.1
AR.6.1 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "white", part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 8, part = "body") %>%
  border_outer(border = border_style) %>%
  border_inner(border = border_style) %>%
  delete_columns("Year.2023") %>%    # DELETE unwanted 'Year.2023'
  delete_columns("Year.2024") %>%    # DELETE unwanted 'Year.2024'
  set_table_properties(layout = "autofit") %>%
  add_footer_lines(values = "Source: GAIN 2024 Data") %>%
  set_caption(caption = "Interest in EGRISS Membership by Country and Year")

AR.6.1

