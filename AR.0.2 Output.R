# ======================================================
# Add to Word document
# ======================================================

library(officer)

# Initialize a fresh document
word_doc <- read_docx()

word_doc <- read_docx()  # Initialize a fresh document

# Add structured content to Word (portrait mode)
word_doc <- word_doc %>%
  body_add_par("GAIN 2024 Annual Report", style = "heading 1") %>%
  body_add_flextable(figure6) %>%
  body_add_break() %>%
  body_add_flextable(figure7) %>%
  body_add_break() %>%
  body_add_flextable(final_flextable) %>%
  
  # Switch to landscape mode before Figure 8
  body_end_section_portrait() %>%
  body_add_par("Figure 8: Breakdown by Year, Use of Recommendations, and Source", style = "heading 2") %>%
  body_add_flextable(figure8_flextable) %>%
  body_add_break() %>%
  
  # Switch back to portrait mode after Figure 8
  body_end_section_landscape() %>%
  body_add_flextable(text1) %>%
  body_add_break() %>%
  
  # Updated Section with Merged Table (Portrait Mode)
  body_add_par("Breakdown by Category and Region for PRO11/PRO12 Data", style = "heading 2") %>%
  body_add_flextable(merged_flextable) %>%
  body_add_break() %>%
  body_add_par("Unique Country Count by Region and Year", style = "heading 2") %>%
  body_add_flextable(unique_country_flextable) %>%
  
  # Switch to landscape mode for the country list (Landscape Mode)
  body_end_section_portrait() %>%
  body_add_flextable(country_list_flextable) %>%
  
  # Switch back to portrait mode for the Map Image (Portrait Mode)
  body_end_section_landscape() %>%
  body_add_par("Map of Examples (2024)", style = "heading 2") %>%
  body_add_img(src = "final_combined_maps.png", width = 5.5, height = 7.5) %>%
  body_add_break() %>%
  
  # Resume in portrait mode with tables (Portrait Mode)
  body_add_flextable(figure9) %>%
  body_add_break() %>%
  body_add_par("Institutional Implementation Breakdown", style = "heading 2") %>%
  body_add_flextable(institutional_flextable) %>%
  body_add_break() %>%
  body_add_par("Future Projects Breakdown by Source for 2024", style = "heading 2") %>%
  body_add_flextable(source_summary_flextable) %>%
  body_add_break() %>%
  body_add_par("Breakdown of Nationally Led Partnerships by Year and Type", style = "heading 2") %>%
  body_add_flextable(partnership_flextable) %>%
  body_add_break() %>%
  body_add_flextable(organization_mentions_flextable) %>%
  body_add_break() %>%
  body_add_par("Summary Table: GFR Data on Pledges", style = "heading 2") %>%
  body_add_flextable(grf_flextable) %>%
  
  # Finish the document with continuous section (portrait by default)
  body_end_section_continuous()

# ======================================================
# Save the Word Document
# ======================================================

# Get current date in YYYY-MM-DD format
current_date <- format(Sys.Date(), "%Y-%m-%d")

# Define output file path with date
word_output_file <- file.path(working_dir, paste0("Annual_Report_GAIN_2024_", current_date, ".docx"))

# Save the Word document
print(word_doc, target = word_output_file)

# ✅ Confirm success
message("Updated GAIN 2024 Annual Report saved successfully at: ", word_output_file)