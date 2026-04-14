
# ============================================================================================================
# AR.2.8 Partnerships and Organization Mentions in R with FlexTable
# ============================================================================================================

library(dplyr)
library(tidyr)
library(flextable)
library(stringr)

# Load dataset
file_path <- file.path(working_dir, "analysis_ready_group_roster.xlsx")
group_roster <- read.xlsx(file_path)

# Step 1: Partnerships Table (based on PRO18)
partnership_summary <- group_roster %>%
  group_by(ryear, PRO18) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))


# List of terms to sum
terms_to_count <- c("UNHCR", "IOM", "JDC", "UNFPA", "World Bank")

# Function to sum the year values for each term in PRO18 using regex
sum_mentions <- function(term) {
  # Define the regex pattern to match the term, considering different punctuations and spaces around it
  regex_term <- paste0("\\b", term, "\\b")  # Match the exact term as a word (boundary)
  
  # Sum the year values for rows that match the term in PRO18, excluding NA values in PRO18
  partnership_summary %>%
    filter(!is.na(PRO18)) %>%  # Exclude rows with NA in PRO18
    mutate(term_match = str_detect(PRO18, regex(regex_term, ignore_case = TRUE))) %>% # Check if term is in PRO18
    filter(term_match) %>%  # Only keep rows where the term is found in PRO18
    summarise(across(`2021`:`2025`, sum, na.rm = TRUE)) %>% # Sum the values for each year
    mutate(Organization = paste("Mentions of", term)) # Add term label
}



contr_df <- group_roster %>%
  mutate(
    b_UNHCR     = as.integer(str_detect(PRO18, regex("\\bUNHCR\\b", ignore_case = TRUE))),
    b_IOM       = as.integer(str_detect(PRO18, regex("\\bIOM\\b", ignore_case = TRUE))),
    b_JDC       = as.integer(str_detect(PRO18, regex("\\bJDC\\b", ignore_case = TRUE))),
    b_UNFPA     = as.integer(str_detect(PRO18, regex("\\bUNFPA\\b", ignore_case = TRUE))),
    b_WorldBank = as.integer(str_detect(PRO18, regex("\\bWorld Bank\\b", ignore_case = TRUE)))
  ) %>% 
  select(pindex2, year, PRO18, b_UNHCR, b_IOM, b_JDC, b_UNFPA, b_WorldBank, g_conled) %>% 
  group_by(g_conled, year, b_UNHCR, b_IOM, b_JDC, b_UNFPA, b_WorldBank) %>% 
  tally()



test <- contr_df %>% 
  group_by(year) %>% 
  summarise(s_UNHCR     = sum(b_UNHCR, na.rm = T),
            s_IOM       = sum(b_IOM, na.rm = T),
            s_JDC       = sum(b_JDC, na.rm = T),
            s_UNFPA     = sum(b_UNFPA, na.rm = T),
            s_WorldBank = sum(b_WorldBank, na.rm = T)) %>% t()

# if (bExport == 10){
#   write.xlsx(contr_df, 'contr_table_2_8.xlsx')
# }

contr <- group_roster %>% 
  group_by(PRO18) %>% 
  tally()

# Sum mentions for each term
sums <- lapply(terms_to_count, sum_mentions)

# Combine the sums into a single data frame
mention_counts <- bind_rows(sums)

# Print the final sums
mention_counts <- mention_counts %>%
  select(c("Organization", "2021", "2022", "2023", "2024", "2025")) %>%
  mutate(Total = rowSums(across(c(`2021`, `2022`, `2023`, `2024`, `2025`)), na.rm = TRUE)) %>%
  rename(`Partnership Organization Mention` = "Organization")

# Step 3: Create FlexTables
# Styling Variables
section_header_color <- "#f3f3f3"  # Light grey for section headers

# Create FlexTable with Styling for specific orgs mentioned
ar.2.8 <- flextable(mention_counts) %>%
  theme_vanilla() %>%  # Base theme
  fontsize(size = 10, part = "all") %>%  # Set font size
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 1)) %>%
  bold(part = "header") %>%  # Bold the header
  bg(part = "header", bg = "#4cc3c9") %>%  # Set header background color
  bg(j = ~ `Total`, bg = "#c9daf8") %>%
  bg(j = ~ `2025`, bg = "#F4CCCC") %>%
  autofit() %>%  # Auto-adjust column widths
  set_table_properties(layout = "autofit", width = 0.6) %>%  # Adjust table sizing
  add_footer_row(
    values = paste0(
      "Table 2.8 supports analysis on page 28 in the 2025 Annual Report. Table c by most mentions of the international partners supporting examples and year.
      *Errata: Additional data from other languages have been included."
    ),
    colwidths = ncol(mention_counts)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 2.8: Mentions of partnership organizations in country-led examples",
        props = fp_text(font.family = "Helvetica", font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

ar.2.8


if (bExport == 1){
  save_as_html(ar.2.8, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.2.8.html") 
}