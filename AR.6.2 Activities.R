
# ============================================================================================================
# AR.6.2: Overview of Respondents Facing Challenges and Types of Challenges Identified
# ============================================================================================================

# Load required libraries
library(flextable)
library(dplyr)
library(tidyr)

# Custom Colors
header_color <- "#4cc3c9"
gray_highlight <- "#D9D9D9"
border_style <- fp_border(color = "black", width = 1)

# Load dataset
main_roster_file <- file.path("analysis_ready_main_roster.csv")
main_roster <- read.csv(main_roster_file)

# Step 1: Ensure ACT04 Variables are Uniform in Type
main_roster <- main_roster %>%
  mutate(across(matches("^ACT04"), as.character))

# Step 2: Process ACT03 Responses (Yes/No/Don't Know)
seen_egriss <- main_roster %>%
  filter(year %in% c(2023, 2024)) %>%
  select(year, ACT03) %>%
  mutate(Response = case_when(
    ACT03 %in% c("2", "NO") ~ "Publications Seen/Received",
    ACT03 %in% c("1", "YES") ~ "No Publications Seen/Received",
    ACT03 %in% c("9", "NO RESPONSE", "8", "DON'T KNOW") ~ "No Response or Don't Know",
    is.na(ACT03) ~ "No Response or Don't Know",
    TRUE ~ as.character(ACT03)
  )) %>%
  group_by(Response, year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = c("Publications Seen/Received", "No Publications Seen/Received", "No Response or Don't Know"),
           year = c(2023, 2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = year, values_from = Count)

# Step 3: Process ACT04 Variables with Combined Logic for `.1` Versions
publication_labels <- c(
  "ACT04.A" = "IRRS Recommendations",
  "ACT04.A.1" = "IRRS Recommendations",
  "ACT04.B" = "IRIS Recommendations",
  "ACT04.B.1" = "IRIS Recommendations",
  "ACT04.C" = "IROSS Recommendations",
  "ACT04.C.1" = "IROSS Recommendations",
  "ACT04.D" = "External Publications Showcasing Recommendations",
  "ACT04.D.1" = "External Publications Showcasing Recommendations",
  "ACT04.E" = "Annual Report",
  "ACT04.E.1" = "Annual Report",
  "ACT04.F" = "Promotional Materials",
  "ACT04.F.1" = "Promotional Materials",
  "ACT04.G" = "Revised Compilers Manual",
  "ACT04.G.1" = "Revised Compilers Manual",
  "ACT04.H" = "Methodological Paper",
  "ACT04.H.1" = "Methodological Paper",
  "ACT04.I" = "Institutional or Sectoral Strategy",
  "ACT04.X" = "Other",
  "ACT04.X.1" = "Other",
  "ACT04.Z" = "Don't Know",
  "ACT04.Z.1" = "Don't Know"
)

challenges_data <- main_roster %>%
  filter(year %in% c(2023, 2024)) %>%
  select(year, matches("^ACT04")) %>%
  pivot_longer(cols = matches("^ACT04"), names_to = "Challenge", values_to = "Reported") %>%
  filter(Reported == "1") %>%
  mutate(Challenge = recode(Challenge, !!!publication_labels)) %>%
  group_by(Challenge, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = list(Count = 0))

challenges_data <- challenges_data %>%
  rename(Response = Challenge)

# Step 4: Combine Both Tables
combined_data <- bind_rows(seen_egriss, challenges_data)

# Step 5: Create FlexTable
ar.6.2 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  bold(part = "header") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "black", part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: Rows 1–3 (“Publications Seen/Received”, “No Publications Seen/Received”, “No Response or Don't Know”) are from ACT03 responses in years 2023–2024. ",
      "Subsequent rows list specific challenges identified via ACT04.* variables (including .1 versions merged), recoded to human-readable labels. ",
      "Counts reflect the number of respondents selecting each option per year."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%set_caption(
  caption = as_paragraph(
    as_chunk(
      "AR.6.2: Overview of Respondents Facing Challenges and Types of Challenges Identified, by year",
      props = fp_text(
        font.family = "Helvetica",
        font.size   = 10,
        italic      = FALSE
      )
    )
  )
)%>%  # Add caption 
  fix_border_issues()
# Display Table in RStudio Viewer
ar.6.2
