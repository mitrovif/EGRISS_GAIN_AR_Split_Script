# ============================================================================================================
# AR.6.3: Overview of Respondents Using Publications and Impact of Publications
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

# Step 1: Ensure ACT05 and ACT06 Variables are Uniform in Type
main_roster <- main_roster %>%
  mutate(across(matches("^ACT05|^ACT06"), as.character))

# Step 2: Process ACT05 Responses (Yes/No/Don't Know)
used_publications <- main_roster %>%
  filter(year == 2024) %>%
  select(year, ACT05) %>%
  mutate(Response = case_when(
    ACT05 %in% c("2", "NO") ~ "Publications Impacted",
    ACT05 %in% c("1", "YES") ~ "Publications Not Impacted",
    ACT05 %in% c("9", "NO RESPONSE", "8", "DON'T KNOW") ~ "No Response or Don't Know about Impact",
    is.na(ACT05) ~ "No Response or Don't Know about Impact",
    TRUE ~ as.character(ACT05)
  )) %>%
  group_by(Response, year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = c("Publications Impacted", "Publications Not Impacted", "No Response or Don't Know about Impact"), year = c(2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = year, values_from = Count)

# Step 3: Process ACT06 Variables
impact_labels <- c(
  "ACT06.B" = "Increased general knowledge on refugees, IDPs, and statelessness statistics",
  "ACT06.C" = "Enhanced data collection and analysis methods",
  "ACT06.D" = "Supported training or capacity-building initiatives",
  "ACT06.E" = "Aided in effective dissemination of information",
  "ACT06.F" = "Facilitated data integration across systems",
  "ACT06.X" = "Other",
  "ACT06.Z" = "Don’t know"
)

impact_data <- main_roster %>%
  filter(year == 2024) %>%
  select(year, matches("^ACT06")) %>%
  pivot_longer(cols = matches("^ACT06"), names_to = "Impact", values_to = "Reported") %>%
  filter(Reported == "1") %>%
  mutate(Impact = recode(Impact, !!!impact_labels)) %>%
  group_by(Impact, year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Count, values_fill = list(Count = 0))

impact_data <- impact_data %>%
  rename(Response = Impact)

# Step 4: Combine Both Tables
combined_data <- bind_rows(used_publications, impact_data)

# Step 5: Create FlexTable
ar.6.3 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "white", part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  autofit() %>%
  # ← Detailed footnote
  add_footer_row(
    values = paste0(
      "Footnote: The first three rows (“Publications Impacted”, “Publications Not Impacted”, “No Response or Don't Know about Impact”) are based on ACT05 responses for year 2024. ",
      "Subsequent rows list impacts identified via ACT06.* variables recoded to descriptive labels (e.g. “Increased general knowledge…”, “Enhanced data collection…”, etc.). ",
      "Counts reflect the number of respondents selecting each option in 2024."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%set_caption(
  caption = as_paragraph(
    as_chunk(
      "AR.6.3: Overview of Respondents Using Publications and Impact of Publications",
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
ar.6.3
