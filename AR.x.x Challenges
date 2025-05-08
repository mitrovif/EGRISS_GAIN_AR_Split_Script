# =============================================================================================================
# AR.X.X: Challenges to Rec Implementation
# =============================================================================================================

# Summarize structured columns (PRO22A to PRO22X)
structured_cols <- grep("^PRO22[A-Z]$", colnames(group_roster), value = TRUE)

structured_summary <- group_roster %>%
  select(all_of(structured_cols)) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Count")

# Clean and group free-text issues in PRO22
free_text_summary <- group_roster %>%
  filter(!is.na(PRO22)) %>%
  # Exclude all-uppercase entries
  filter(!str_detect(PRO22, "^[[:upper:]\\s[:punct:]]+$")) %>%
  # Lowercase and trim
  mutate(PRO22_clean = str_squish(tolower(PRO22))) %>%
  # Add a fake split marker before "2 -" / "2)" / "3 -" / "3)"
  mutate(PRO22_clean = str_replace_all(PRO22_clean, "(?=\\b[23]\\s*[-\\)])", "|||")) %>%
  # Split on the custom marker
  separate_rows(PRO22_clean, sep = "\\|\\|\\|") %>%
  mutate(PRO22_clean = str_squish(PRO22_clean)) %>%
  filter(PRO22_clean != "") %>%
  count(PRO22_clean, sort = TRUE) %>%
  rename(Category = PRO22_clean, Count = n)

# Combine summaries
struct_free_summary <- bind_rows(structured_summary, free_text_summary)

# Rename categories and re-categorize some of the free text responses
final_summary <- struct_free_summary %>%
  mutate(
    'Implementation Challenges' = case_when(
      Category == "PRO22A" ~ "Lack of clarity (on how to implement)",
      Category == "PRO22B" ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "PRO22C" ~ "Lack of technical support",
      Category == "PRO22D" ~ "Organizational resistance",
      Category == "PRO22E" ~ "Lack of training and capacity building opportunities",
      Category == "PRO22F" ~ "Lack of peer-to-peer learning opportunities",
      Category == "PRO22X" ~ "Other",
      Category == "1) different uses of the idp definition in data collection," ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "1- gap in the understanding of recommendations;" ~ "Lack of training and capacity building opportunities",
      Category == "2) lack of coordination on data collection between national and local level," ~ "Other",
      Category == "2- strengthening the capacity of regional focal points;" ~ "Lack of training and capacity building opportunities",
      Category == "3) lack of a harmonise statistical approach to durable solutions." ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "3- dissemination of the recommendations within member states" ~ "Other",
      Category == "available administrative data do not allow to identify 'persons without a recognized nationality status'. the focus of the press release was therefore on stateless persons." ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "definitional inconsistencies between countries." ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "limited possibility for including all key questions from irrs/iris in the questionnaire given that it is a census." ~ "Lack of further guidance (not sufficient methodological information)",
      Category == "sensitivity of egriss-recommended identification" ~ "Other",
      Category == "sensitivity of of egriss-recommended identification" ~ "Other",
      Category == "some of the recommendations are not currently or fully measured at statistics canada (e.g. stocks of persons returned after having sought international protection abroad or internally displaced persons)or" ~ "Other",
      Category == "timeliness and data quality" ~ "Other",
      Category == "within the government institution such ncri and even non government institution that misunderstood that idps statistics strategic plan is risk to their work" ~ "Lack of clarity (on how to implement)",
      .default = Category
  )) %>%
  select(c("Implementation Challenges", Count))

final_summary <- final_summary %>%
  group_by(`Implementation Challenges`) %>%
  summarize(Responses = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Responses))

# View results
print(final_summary)

# Beautify and create FlexTable for Word
ar.X.X <- flextable(final_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%
  bg(part = "header", bg = "#4cc3c9") %>%
  fontsize(size = 10, part = "all") %>%  # Set font size
  autofit() %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "AR.X.X: Challenges Experienced during Implementation of Recommendations",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display table
ar.X.X
