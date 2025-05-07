
# =============================================================================================================
#  AR.4.1: Future Examples Using Different Types of Data Source or Tool 
# =============================================================================================================

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
    Source = case_when(
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
  count(Source) %>%
  rename(Count = n) %>%
  bind_rows(tibble(Source = "Total", Count = sum(.$Count)))

# Create a FlexTable for Word
ar.4.1 <- flextable(source_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(bg = "#c9daf8", j = ~ Count) %>%  # Highlight the Count column
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%  # Set font size
  autofit() %>%
  add_footer_row(
    values = paste0(
      "Footnote: Counts are based on FPR05_* fields in `group_roster2` (converted to numeric) where a value of 1 ",
      "indicates the use of that source/tool. “Source” categories derived via grepl on variable names: Survey, ",
      "Administrative Data, Census, Data Integration, Non-Traditional, Strategy, Guidance/Toolkit, Workshop/Training, ",
      "Other, Unknown. “Total” row is the sum across all categories."
    ),
    colwidths = ncol(source_summary)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
set_caption(
  caption = as_paragraph(
    as_chunk(
      "AR.4.1:  Future Examples Using Different Types of Data Source or Tool (Figure 14, AR pg.50)",
      props = fp_text(
        font.family = "Helvetica",
        font.size   = 10,
        italic      = FALSE
      )
    )
  )
)
ar.4.1                          
