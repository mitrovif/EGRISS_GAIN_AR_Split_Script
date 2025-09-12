
# =============================================================================================================
#  AR.6.1: Future Examples Using Different Types of Data Source or Tool 
# =============================================================================================================

group_roster_file2 <- file.path(working_dir, "analysis_ready_group_roster2.csv")
group_roster2 <- read.csv(group_roster_file2)

fpr05_columns <- grep("^FPR05", names(group_roster2), value = TRUE)

sapply(group_roster2[, fpr05_columns], class)
group_roster2 <- group_roster2 %>%
  mutate(across(all_of(fpr05_columns), ~ as.numeric(.)))

# Step 1: Create the Source column and count the number of times each source is used
source_summary <- group_roster2 %>%
  pivot_longer(
    cols = all_of(fpr05_columns),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    `Future Example Source` = case_when(
      grepl("SURVEY", Source_Variable) ~ "Survey",
      grepl("ADMINISTRATIVE.DATA", Source_Variable) ~ "Administrative Data",
      grepl("CENSUS", Source_Variable) ~ "Census",
      grepl("DATA.INTEGRATION", Source_Variable) ~ "Data Integration",
      grepl("NON.TRADITIONAL", Source_Variable) ~ "Non-Traditional",
      grepl("STRATEGY", Source_Variable) ~ "Strategy",
      grepl("GUIDANCE.TOOLKIT", Source_Variable) ~ "Guidance/Toolkit",
      grepl("H..WORKSHOP.TRAINING", Source_Variable) ~ "Workshop/Training",
      grepl("OTHER", Source_Variable) ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(`Future Example Source`) %>%
  rename(`Future Examples (2025/2026)` = n) %>%
  arrange(desc(`Future Examples (2025/2026)`))

# Create a FlexTable for Word
ar.6.1 <- flextable(source_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = "#c9daf8", j = ~ `Future Examples (2025/2026)`) %>%  # Highlight the Count column
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%  # Set font size
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 6.1 supports Figure 14 on page 50 in the 2024 Annual Report (replicated above). Table is disaggregated by upcoming multiple sources (respondents to GAIN survey answered a multiple-choice question)."
    ),
    colwidths = ncol(source_summary)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 6.1: Future examples using different types of data sources or tools",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )

ar.6.1                          
