# ============================================================================================================
# Partnerships and Organization Mentions in R with FlexTable
# ============================================================================================================

library(dplyr)
library(tidyr)
library(flextable)
library(stringr)

# Load dataset
file_path <- "analysis_ready_group_roster.csv"
group_roster <- read.csv(file_path)

# Step 1: Partnerships Table (based on PRO18)
partnership_summary <- group_roster %>%
  group_by(ryear, PRO18) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = ryear, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Load necessary libraries
library(dplyr)
library(stringr)

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
    summarise(across(`2021`:`2024`, sum, na.rm = TRUE)) %>% # Sum the values for each year
    mutate(Organization = paste("Mentions of", term)) # Add term label
}

# Sum mentions for each term
sums <- lapply(terms_to_count, sum_mentions)

# Combine the sums into a single data frame
mention_counts <- bind_rows(sums)

# Print the final sums
mention_counts <- mention_counts %>%
  select(c("Organization", "2021", "2022", "2023", "2024"))
print(mention_counts)

# Step 3: Create FlexTables

# Styling Variables
section_header_color <- "#f3f3f3"  # Light grey for section headers

# Create FlexTable with Styling for specific orgs mentioned
organization_mentions_flextable <- flextable(mention_counts) %>%
  theme_vanilla() %>%  # Base theme
  fontsize(size = 10, part = "all") %>%  # Set font size
  bold(part = "header") %>%  # Bold the header
  bg(part = "header", bg = "#4cc3c9") %>%  # Set header background color
  autofit() %>%  # Auto-adjust column widths
  set_table_properties(layout = "autofit", width = 0.6)  # Adjust table sizing

# Apply conditional styling (only if table has rows)
if (nrow(mention_counts) > 0) {
  organization_mentions_flextable <- organization_mentions_flextable %>%
    color(i = 1, color = "black", part = "body")  # Keep the text black (default)
}

# Add Caption
organization_mentions_flextable <- organization_mentions_flextable %>%
  set_caption("Organization Mentions by Year")  # Table Caption

# Display the Table
print(organization_mentions_flextable)
