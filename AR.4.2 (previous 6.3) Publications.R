
# ============================================================================================================
# AR.4.2: Overview of Respondents Using Publications and Impact of Publications
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

# test <- main_roster %>%
#   filter(is.na(ACT05) & year == 2024)

# Step 2: Process ACT05 Responses (Yes/No/Don't Know)
used_publications <- main_roster %>%
  filter(year == 2024) %>%
  select(year, ACT05) %>%
  mutate(Response = case_when(
    ACT05 %in% c("2", "NO") ~ "Publications Not Impacted",
    ACT05 %in% c("1", "YES") ~ "Publications Impacted",
    ACT05 %in% c("9", "NO RESPONSE", "8", "DON'T KNOW") ~ "No Response or Don't Know about Impact",
    is.na(ACT05) ~ "No Response or Don't Know about Impact",
    TRUE ~ as.character(ACT05)
  )) %>%
  group_by(Response, year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = c("Publications Impacted", "Publications Not Impacted", "No Response or Don't Know about Impact"), year = c(2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = year, values_from = Count) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(c(`2024`)), na.rm = TRUE)) %>%
  ungroup() %>%
  add_row(.after = 3)

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
  pivot_wider(names_from = year, values_from = Count, values_fill = list(Count = 0)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(c(`2024`)), na.rm = TRUE)) %>%
  ungroup %>%
  arrange(desc(`2024`))

impact_data <- impact_data %>%
  rename(Response = Impact)

# Step 4: Combine Both Tables
combined_data <- bind_rows(used_publications, impact_data)

# Step 5: Create FlexTable
ar.4.2 <- flextable(combined_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "black", part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  bg(j = ~ Total, bg = "#c9daf8") %>%  # Highlight the total column
  bg(j = ~ `2024`, bg = "#F4CCCC") %>%
  bg(bg = "gray", i = 4) %>%
  autofit() %>%
  # ← Detailed footnote
  add_footer_row(
    values = paste0(
      "Table 4.2 is not featured in the 2024 Annual Report. Table is disaggregated by EGRISS publications impact, specific impact reported of EGRISS and year. Question was first introduced in GAIN in 2024, so previous years are not presented."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 4.2: Impact of EGRISS-developed publications on example implementation",
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
ar.4.2
