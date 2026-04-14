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
  filter(ryear %in% c(2023, 2024, 2025)) %>%
  # filter(ryear %in% c(2023, 2024, 2025), g_conled == 1) %>%
  # filter(ryear %in% c(2023, 2024, 2025), g_conled == 1, PRO09 == 1) %>%
  select(ryear, PRO19, g_conled) %>%
  mutate(Response = case_when(
    PRO19 == 2 | PRO19 == "NO" ~ "No challenges faced",
    PRO19 == 1 | PRO19 == "YES" ~ "Challenges faced",
    PRO19 == 9 | PRO19 == "NO RESPONSE" ~ "No response on challenges or don't know",
    PRO19 == 8 | PRO19 == "DON'T KNOW" ~ "No response on challenges or don't know",
    is.na(PRO19) ~ "No response on challenges or don't know",
    TRUE ~ as.character(PRO19)
  )) %>%
  mutate(Response = recode(Response, !!!response_labels),
         g_conled_name = ifelse(g_conled == 1, 'Country-Led Examples', 
                         ifelse(g_conled %in% c(2, 3), 'Institutional-Led Examples', NA))) %>%
  group_by(Response, ryear, g_conled_name) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  complete(Response = names(response_labels), ryear = c(2023, 2024, 2025), fill = list(Count = 0)) %>%
  pivot_wider(
    names_from = c(g_conled_name, ryear),
    values_from = Count,
    names_sep = "_"
  ) %>%
  mutate(
    `Country-Led Examples_Total` =
      rowSums(
        pick(starts_with("Country-Led Examples_")),
        na.rm = TRUE
      ),
    `Institutional-Led Examples_Total` =
      rowSums(
        pick(starts_with("Institutional-Led Examples_")),
        na.rm = TRUE
      )
  )

pro19_summary <- pro19_summary %>%
  # mutate(across(c(`2023`, `2024`, `2025`), as.character)) %>%  # Convert to character for compatibility
  select(
    Response,
    
    # Country-Led Examples
    `Country-Led Examples_2023`,
    `Country-Led Examples_2024`,
    `Country-Led Examples_2025`,
    `Country-Led Examples_Total`,
    
    # Institutional-Led Examples
    `Institutional-Led Examples_2023`,
    `Institutional-Led Examples_2024`,
    `Institutional-Led Examples_2025`,
    `Institutional-Led Examples_Total`
  ) %>% 
  rename(`Challenges in Example Implementation` = Response) %>% 
  mutate(across(everything(), as.character))

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
  "PRO20.X" = "Other",
  "PRO20.Z" = "Don't know"
)

challenges_data <- group_roster %>%
  filter(ryear %in% c(2023, 2024, 2025)) %>%
  # filter(ryear %in% c(2023, 2024, 2025), g_conled == 1) %>%
  # filter(ryear %in% c(2023, 2024, 2025), g_conled == 1, PRO09 == 1) %>%
  select(ryear, g_conled, starts_with("PRO20.")) %>%
  pivot_longer(cols = starts_with("PRO20."), names_to = "Challenge", values_to = "Reported") %>%
  filter(Reported == 1) %>%
  mutate(Challenge = recode(Challenge, !!!challenge_labels),
         g_conled_name = ifelse(g_conled == 1, 'Country-Led Examples', 
                         ifelse(g_conled %in% c(2, 3), 'Institutional-Led Examples', NA))) %>%
  group_by(Challenge, g_conled_name, ryear) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(g_conled_name, ryear),
    values_from = Count,
    values_fill = 0,
    names_sep = "_"
  ) %>%
  mutate(
    `Country-Led Examples_Total` =
      rowSums(
        pick(starts_with("Country-Led Examples_")),
        na.rm = TRUE
      ),
    `Institutional-Led Examples_Total` =
      rowSums(
        pick(starts_with("Institutional-Led Examples_")),
        na.rm = TRUE
      )
  )




challenges_data <- challenges_data %>%
  select(
    Challenge,
    
    # Country-Led Examples
    `Country-Led Examples_2023`,
    `Country-Led Examples_2024`,
    `Country-Led Examples_2025`,
    `Country-Led Examples_Total`,
    
    # Institutional-Led Examples
    `Institutional-Led Examples_2023`,
    `Institutional-Led Examples_2024`,
    `Institutional-Led Examples_2025`,
    `Institutional-Led Examples_Total`
  ) %>% 
  rename("Challenges in Example Implementation" = Challenge) %>%
  mutate(across(everything(), as.character))  # Convert to character for compatibility

# Combining Both Tables into One Stacked Table
combined_data <- bind_rows(
  tibble(`Challenges in Example Implementation` = "Count of Examples Facing Challenges", `Country-Led Examples_2023` = "",`Country-Led Examples_2024` = "", `Country-Led Examples_2025` = "", `Country-Led Examples_Total` = "",
         `Institutional-Led Examples_2023` = "", `Institutional-Led Examples_2024` = "", `Institutional-Led Examples_2025` = "", `Institutional-Led Examples_Total` = ""),
  pro19_summary,
  tibble(`Challenges in Example Implementation` = "", `Country-Led Examples_2023` = "",`Country-Led Examples_2024` = "", `Country-Led Examples_2025` = "", `Country-Led Examples_Total` = "",
         `Institutional-Led Examples_2023` = "", `Institutional-Led Examples_2024` = "", `Institutional-Led Examples_2025` = "", `Institutional-Led Examples_Total` = ""),  # Spacer row
  tibble(`Challenges in Example Implementation` = "Types of Challenges Faced in Examples Implementation", `Country-Led Examples_2023` = "",`Country-Led Examples_2024` = "", `Country-Led Examples_2025` = "", `Country-Led Examples_Total` = "",
         `Institutional-Led Examples_2023` = "", `Institutional-Led Examples_2024` = "", `Institutional-Led Examples_2025` = "", `Institutional-Led Examples_Total` = ""),
  challenges_data
)

# Identify row indices dynamically
highlight_rows <- which(combined_data$`Challenges in Example Implementation` %in% 
                          c("Count of Examples Facing Challenges", 
                            "Types of Challenges Faced in Examples Implementation"))

# Create the flextable
ar.2.5 <- flextable(combined_data) %>%
  add_header_row(values = c("", "Country-Led Examples", "Institution-Led Examples"), 
                 colwidths = c(1, 4, 4)) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%   
  bold(part = "header") %>%
  bg(part = "header", bg = header_color) %>%  # Light Blue Header
  bg(
    bg = "#c9daf8",
    j = c(
      "Country-Led Examples_Total",
      "Institutional-Led Examples_Total"
    )
  ) %>%
  bg(
    bg = "#f4cccc",
    j = c(
      "Country-Led Examples_2025",
      "Institutional-Led Examples_2025"
    )
  ) %>% 
  bg(i = highlight_rows, bg = gray_highlight, part = "body") %>%  # Highlight Correct Rows
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer Border for Entire Table
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  autofit() %>%
  set_header_labels(
    `Country-Led Examples_2023` = "2023",
    `Country-Led Examples_2024` = "2024",
    `Country-Led Examples_2025` = "2025",
    `Country-Led Examples_Total` = "Total",
    `Institutional-Led Examples_2023` = "2023",
    `Institutional-Led Examples_2024` = "2024",
    `Institutional-Led Examples_2025` = "2025",
    `Institutional-Led Examples_Total` = "Total"
  ) %>%
  add_footer_row(
    values = paste0(
      "Table 2.5 supports Figure 8 on page 28 in the 2025 Annual Report (replicated above). In addition to Figure 8 data on types of challenges identified, table provides overall numbers of examples who did/did not face challenges."
    ),
    colwidths = ncol(combined_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  # Updated Caption
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.5: Overview of country-led examples facing challenges with implementation, including types of challenges faced ",
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


if (bExport == 1){
  save_as_html(ar.2.5, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.2.5_V2.html") 
}