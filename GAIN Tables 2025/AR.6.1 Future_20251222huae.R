
# =============================================================================================================
#  AR.6.1: Future Examples Using Different Types of Data Source or Tool 
# =============================================================================================================

group_roster_file2 <- file.path(working_dir, "analysis_ready_group_roster2.xlsx")
group_roster2 <- read.xlsx(group_roster_file2)

fpr05_columns <- grep("^FPR05", names(group_roster2), value = TRUE)

sapply(group_roster2[, fpr05_columns], class)
group_roster2 <- group_roster2 %>%
  mutate(across(all_of(fpr05_columns), ~ as.numeric(.)))

# Step 1: Create the Source column and count the number of times each source is used
source_summary <- group_roster2 %>%
  # pivot_longer(
  #   cols = all_of(fpr05_columns),
  #   names_to = "Source_Variable",
  #   values_to = "Value"
  # ) %>%
  # filter(Value == 1) %>%
  mutate(
    `Future Example Source` = case_when(
      FPR05 == 11 ~ "Survey",
      FPR05 == 12 ~ "Administrative Data",
      FPR05 == 13 ~ "Census",
      FPR05 == 14 ~ "Data Integration",
      FPR05 == 15 ~ "Non-Traditional",
      FPR05 == 16 ~ "Strategy",
      FPR05 == 17 ~ "Guidance/Toolkit",
      FPR05 == 18 ~ "Workshop/Training",
      FPR05 == 96 ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(`Future Example Source`) %>%
  rename(`Future Examples (2026/2027)` = n) %>%
  arrange(desc(`Future Examples (2026/2027)`))

# Create a FlexTable for Word
ar.6.1 <- flextable(source_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = "#c9daf8", j = ~ `Future Examples (2026/2027)`) %>%  # Highlight the Count column
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%  # Set font size
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Table 6.1 supports Figure 14 on page 50 in the 2025 Annual Report (replicated above). Table is disaggregated by upcoming main source (respondents to GAIN survey answered a multiple-choice question)."
    ),
    colwidths = ncol(source_summary)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 6.1: Future examples using main type of data source or tool",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )

ar.6.1         


if (bExport == 1){
  save_as_html(ar.6.1, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.6.1.html") 
}
