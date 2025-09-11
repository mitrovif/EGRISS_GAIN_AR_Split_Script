
# ============================================================================================================
# AR.4.1: Overview of Respondents Facing Challenges and Types of Challenges Identified
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
    ACT03 %in% c("2", "NO") ~ "No Publications Seen/Received",
    ACT03 %in% c("1", "YES") ~ "Publications Seen/Received",
    ACT03 %in% c("9", "NO RESPONSE", "8", "DON'T KNOW") ~ "No Response or Don't Know",
    is.na(ACT03) ~ "No Response or Don't Know",
    TRUE ~ as.character(ACT03)
  )) %>%
  group_by(Response, year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = c("Publications Seen/Received", "No Publications Seen/Received", "No Response or Don't Know"),
           year = c(2023, 2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = year, values_from = Count) %>%
  arrange(desc(`2024`)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(c(`2023`, `2024`)), na.rm = TRUE)) %>%
  ungroup() %>%
  add_row(.after = 3)

# Step 3: Process ACT04 Variables with Combined Logic for `.1` Versions
publication_labels <- c(
  "ACT04.B" = "IRRS Recommendations",
  "ACT04.B.1" = "IRRS Recommendations",
  "ACT04.C" = "IRIS Recommendations",
  "ACT04.C.1" = "IRIS Recommendations",
  "ACT04.D" = "IROSS Recommendations",
  "ACT04.D.1" = "IROSS Recommendations",
  "ACT04.E" = "External Publications Showcasing Recommendations",
  "ACT04.E.1" = "External Publications Showcasing Recommendations",
  "ACT04.F" = "Annual Report",
  "ACT04.F.1" = "Annual Report",
  "ACT04.G" = "Promotional Materials",
  "ACT04.G.1" = "Promotional Materials",
  "ACT04.H" = "Revised Compilers Manual",
  "ACT04.H.1" = "Revised Compilers Manual",
  "ACT04.I" = "Methodological Paper",
  "ACT04.I.1" = "Methodological Paper",
  "ACT04.X" = "Other",
  "ACT04.X.1" = "Other",
  "ACT04.Z" = "Don't Know",
  "ACT04.Z.1" = "Don't Know"
)

# challenges_data <- main_roster %>%
#   filter(year %in% c(2023, 2024)) %>%
#   select(year, matches("^ACT04")) %>%
#   pivot_longer(cols = matches("^ACT04"), names_to = "Challenge", values_to = "Reported") %>%
#   filter(Reported == "1") %>%
#   filter(Challenge == "ACT04.A" | Challenge == "ACT04.A.1")

challenges_data <- main_roster %>%
  filter(year %in% c(2023, 2024)) %>%
  select(year, matches("^ACT04"), -ACT04.A) %>% 
  pivot_longer(cols = matches("^ACT04"), names_to = "Challenge", values_to = "Reported") %>%
  filter(Reported == "1") %>%
  mutate(Challenge = recode(Challenge, !!!publication_labels)) %>%
  group_by(Challenge, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = list(Count = 0)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(c(`2023`, `2024`)), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(`2024`)) %>%
  select(c(Challenge, `2023`, `2024`, Total))

challenges_data <- challenges_data %>%
  rename(Response = Challenge)

# Step 4: Combine Both Tables
combined_data <- bind_rows(seen_egriss, challenges_data)

# Step 5: Create FlexTable
ar.4.1 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  bold(part = "header") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "black", part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(j = ~ Total, bg = "#c9daf8") %>%  # Highlight the total column
  bg(j = ~ `2024`, bg = "#F4CCCC") %>%
  bg(bg = "gray", i = 4) %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 4.1 is not featured in the 2024 Annual Report. Table is disaggregated by EGRISS publications seen or received, specific challenges identified and year. Question was first introduced in GAIN in 2023, so previous years are not presented."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 4.1: Visibility of IRRS, IRIS and IROSS publications",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  ) %>%  # Add caption 
  fix_border_issues()

# Display Table in RStudio Viewer
ar.4.1
