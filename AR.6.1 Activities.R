
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
    ACT02_chr = as.character(ACT02),  # force consistent type
    Interest = case_when(
      ACT02_chr %in% c("1", "YES") ~ "Interested in Learning about Membership",
      ACT02_chr %in% c("2", "NO", "8", "DON'T KNOW") ~ "Not Interested in Learning about Membership or Don't Know",
      ACT02_chr %in% c("9", "Unknown") ~ NA_character_,
      TRUE ~ ACT02_chr  # default fallback, now consistent
    ),
    Organization_Type = case_when(
      LOC01 == 1 ~ "NSO",
      LOC01 %in% c(2, 3) ~ "International and Other Organizations",
      TRUE ~ "Other"
    )
  ) %>%
  select(-ACT02_chr) %>%
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
    `Year 2024` = "",
    stringsAsFactors = FALSE
  ),
  interested_organizations
) %>%
  mutate(Interest = ifelse(Interest == "", NA, Interest)) %>%  # Convert "" to NA
  fill(Interest, .direction = "down") %>%
  rename('Organization Type' = Organization_Type) 

# Identify the row index where 'List of Interested Organizations' appears
insert_index <- which(combined_data$Interest == "List of Interested Organizations")

# Create a blank row with same structure
blank_row <- combined_data[1, ]
blank_row[,] <- NA  # Set all values to NA

# Insert the blank row before that index
combined_data <- bind_rows(
  combined_data[1:(insert_index - 1), ],
  blank_row,
  combined_data[insert_index:nrow(combined_data), ]
)

# Create a new row with 'Interest in EGRISS Membership' in the first column, rest NA
new_row <- combined_data[1, ]
new_row[,] <- NA
new_row$Interest <- "Interest in EGRISS Membership"

# Insert it as the first row
combined_data <- bind_rows(
  new_row,
  combined_data
) %>%
  select(c("Interest", "Organization Type", "Year 2023", "Year 2024"))

# Create FlexTable for Combined Table - AR.6.1
ar.6.1 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "white", part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  set_table_properties(layout = "autofit") %>%
  merge_v(j = ~ Interest) %>%
  #merge_at(i = 1, j = 1:ncol(combined_data_display), part = "body") %>%
  #merge_at(i = 12, j = 1:ncol(combined_data_display), part = "body") %>%
  bg(i = c(1, 9), bg = "#d9d9d9", part = "body") %>%
  align(i = c(1, 12), align = "left", part = "body") %>%
  vline(i = 11, j = 1:ncol(combined_data_display), border = fp_border(width = 0), part = "body") %>%
  add_footer_lines(values = "Source: GAIN 2024 Data") %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(caption = "Interest in EGRISS Membership by Country and Year") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  fix_border_issues()

ar.6.1
