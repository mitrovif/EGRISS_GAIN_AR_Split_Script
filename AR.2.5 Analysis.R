# ============================================================================================================
# AR.2.5: Overview of Respondents Facing Challenges and Types of Challenges Identified
# ============================================================================================================

library(flextable)
library(dplyr)
library(tidyr)

# Custom Colors
header_color <- "#4cc3c9"      # Light Blue Header
gray_highlight <- "#D9D9D9"   # Gray for Key Rows
border_style <- fp_border(color = "black", width = 1)

# PRO19 Responses - Transposed and with Labels

response_labels <- c(
  "Challenges faced" = "Challenges faced",
  "No challenges faced" = "No challenges faced",
  "No response on challenges or don't know" = "No response on challenges or don't know"
)

pro19_summary <- group_roster %>%
  filter(ryear %in% c(2023, 2024), g_conled == 1, PRO09 == 1) %>%
  select(ryear, PRO19) %>%
  mutate(Response = case_when(
    PRO19 == 2 | PRO19 == "NO" ~ "No challenges faced",
    PRO19 == 1 | PRO19 == "YES" ~ "Challenges faced",
    PRO19 == 9 | PRO19 == "NO RESPONSE" ~ "No response on challenges or don't know",
    PRO19 == 8 | PRO19 == "DON'T KNOW" ~ "No response on challenges or don't know",
    is.na(PRO19) ~ "No response on challenges or don't know",
    TRUE ~ as.character(PRO19)
  )) %>%
  mutate(Response = recode(Response, !!!response_labels)) %>%
  group_by(Response, ryear) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = names(response_labels), ryear = c(2023, 2024), fill = list(Count = 0)) %>%
  pivot_wider(names_from = ryear, values_from = Count) %>%
  mutate(Total = rowSums(across(c(`2023`, `2024`)), na.rm = TRUE))

pro19_summary <- pro19_summary %>%
  mutate(across(c(`2023`, `2024`), as.character)) %>%  # Convert to character for compatibility
  rename(`Challenges in Example Implementation` = Response)

# Challenges Reported (Figure 9) - Transposed and with Labels
challenge_labels <- c(
  "PRO20.A" = "Non-response bias",
  "PRO20.B" = "Sampling errors",
  "PRO20.C" = "Identification of populations",
  "PRO20.D" = "Data confidentiality and privacy",
  "PRO20.E" = "Resource constraints",
  "PRO20.F" = "Political issues",
  "PRO20.G" = "Safety concerns",
  "PRO20.H" = "Timeliness and data quality",
  "PRO20.I" = "Limited technical capacity",
  "PRO20.J" = "Lack of accessible guidance",
  "PRO20.X" = "Other"
)

challenges_data <- group_roster %>%
  filter(ryear %in% c(2023, 2024), g_conled == 1, PRO09 == 1) %>%
  select(ryear, starts_with("PRO20.")) %>%
  pivot_longer(cols = starts_with("PRO20."), names_to = "Challenge", values_to = "Reported") %>%
  filter(Reported == 1) %>%
  mutate(Challenge = recode(Challenge, !!!challenge_labels)) %>%
  group_by(Challenge, ryear) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(Total = rowSums(across(c(`2023`, `2024`)), na.rm = TRUE))

challenges_data <- challenges_data %>%
  rename("Challenges in Example Implementation" = Challenge) %>%
  mutate(across(c(`2023`, `2024`), as.character))  # Convert to character for compatibility

# Combining Both Tables into One Stacked Table
combined_data <- bind_rows(
  tibble(`Challenges in Example Implementation` = "Count of Respondents Facing Challenges", `2023` = "", `2024` = ""),
  pro19_summary,
  tibble(`Challenges in Example Implementation` = "", `2023` = "", `2024` = ""),  # Spacer row
  tibble(`Challenges in Example Implementation` = "Types of Challenges Faced in Examples Implementation", `2023` = "", `2024` = ""),
  challenges_data
)

# Identify row indices dynamically
highlight_rows <- which(combined_data$`Challenges in Example Implementation` %in% 
                          c("Count of Respondents Facing Challenges", 
                            "Types of Challenges Faced in Examples Implementation"))

# Create the flextable
ar.2.5 <- flextable(combined_data) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%   
  bold(part = "header") %>%
  bg(part = "header", bg = header_color) %>%  # Light Blue Header
  bg(bg = "#c9daf8", j = ~ Total) %>%   # Highlight the Total column
  bg(bg = "#f4cccc", j = ~ `2024`) %>%   # Highlight the 2024 column
  bg(i = highlight_rows, bg = gray_highlight, part = "body") %>%  # Highlight Correct Rows
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer Border for Entire Table
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 2.5 supports Figure 8 on page 28 in the 2024 Annual Report (replicated above). In addition to Figure 8 data on types of challenges identified, table provides overall numbers of examples who did/did not face challenges."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  # Updated Caption
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.5: Overview of respondents facing challenges with example implementation, including types of challenges faced ",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

print(ar.2.5)
