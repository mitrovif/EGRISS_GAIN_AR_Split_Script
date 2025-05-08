# ======================================================
# Add to Word document
# ======================================================
# Load required library
library(officer)

# Define footer with page numbering using correct 'field' argument
footer_section <- fpar(
  ftext("Page ", prop = fp_text(font.family = "Arial", font.size = 10)),
  run_word_field(field = "PAGE"),
  ftext(" of ", prop = fp_text(font.family = "Arial", font.size = 10)),
  run_word_field(field = "NUMPAGES")
)

# Define narrow‑margin layout including footer on all pages
# Wrap footer_section in block_list() as required by prop_section
default_section <- prop_section(
  page_size      = page_size(),
  page_margins   = page_mar(
    top    = 0.3,
    bottom = 0.3,
    left   = 0.5,
    right  = 0.5,
    header = 0.1,
    footer = 0.1
  ),
  footer_default = block_list(footer_section)
)

# Initialize Word document with default section
word_doc <- read_docx() %>%
  body_set_default_section(default_section)

# Add main title on first page
word_doc <- word_doc %>%
  body_add_fpar(
    fpar(
      ftext(
        "GAIN Survey Annex to EGRISS 2024 Annual Report",
        prop = fp_text(font.family = "Arial", font.size = 16, bold = TRUE)
      ),
      fp_p = fp_par(text.align = "center")
    )
  )

# Define list items to add in font size 10
list_items <- c(
  "",
  "",
  "",
  "",
  "",
  "",
  "List of Tables:",
  "",
  "",
  "",
  "",
  "Overall GAIN Data (AR.1):",
  "    AR.1.1: Trend of Country and Institutional-led Implementation Example, by year (Figure 4, AR pg. 24)",
  "    AR.1.2: Overview of the Implementation of the IRRS, IRIS and IROSS in 2024 (Figure 5, AR pg.25)",
  "    AR.1.3: Overview of the Mixed Implementation of the IRRS, IRIS and IROSS, by year (AR pg.24)",
  "    AR.1.4: Count of unique countries with country-led implementation examples, by distinct countries and year (Not in AR)",
  "    AR.1.5: List of countries with country-led implementation using recommendations by region (Not in AR)",
  "",
  "Country-led GAIN Data (AR.2):",
  "    AR.2.1: Country-led implementation of the Recommendations by region (Figure 6, AR pg.25)",
  "    AR.2.2: Overview Data Sources and Tools for Country-led Examples, by year (Figure 7, AR pg. 27)",
  "    AR.2.3: Components of EGRISS Recommendations Most Frequently Used, by recommendation and type (AR pg.27)",
  "    AR.2.4: Overview of Respondents Facing Challenges and Types of Challenges Identified (Figure 8, AR pg.28)",
  "    AR.2.5: Breakdown of Implementation Challenges of Recommendations, by example lead (Not in AR)",
  "    AR.2.7: Mentions of international partners in country-led implementations, by year (AR pg.28)",
  "",
  "Institution-led GAIN Data (AR.3):",
  "    AR.3.1: Institutional Implementation Breakdown, by year (AR pg.49)",
  "    AR.3.2: Institutional Implementation Breakdown, by implementation level (AR pg.49)",
  "",
  "Future Examples GAIN Data (AR.4):",
  "    AR.4.1: Future Examples Using Different Types of Data Source or Tool (Figure 14, AR pg.50)",
  "    AR.4.2: Future Projects Overview, by source or tool, quarter, population and region (Not in AR)",
  "",
  "GRF Pledges GAIN Data (AR.5):",
  "    AR.5.1: GRF Data on Pledges Implementation, by stage, region, and entity type (Not in AR)",
  "",
  "EGRISS Visibility (AR.6):",
  "    AR.6.1: Interest in learning more about EGRISS membership, by country and year (Not in AR)",
  "    AR.6.2: Overview of Respondents Facing Challenges and Types of Challenges Identified, by year (Not in AR)",
  "    AR.6.3: Overview of Respondents Using Publications and Impact of Publications (Not in AR)",
  "",
  "Example Updates and Length (AR.7):",
  "    AR.7.1: Length of implementation by use of recommendations and example type (Not in AR)",
  "",
  "Maps (AR.8):",
  "    AR.8.1: Implementation Map"
)

# Add each list item directly after the title on the first page
for(item in list_items) {
  word_doc <- word_doc %>%
    body_add_fpar(
      fpar(
        ftext(item, prop = fp_text(font.family = "Arial", font.size = 8)),
        fp_p = fp_par(text.align = "left", line_spacing = 1)
      )
    )
}

  # ar.1 tables
word_doc <- word_doc %>% 
  body_add_break() %>% body_add_flextable(ar.1.1) %>% body_add_break() %>%
  body_add_flextable(ar.1.2) %>% body_add_break() %>%
  body_add_flextable(ar.1.3) %>% body_add_break() %>%
  body_add_flextable(ar.1.4) %>% body_add_break() %>%
  body_add_flextable(ar.1.5) %>% body_add_break() %>%
  
  # portrait → landscape for ar.2.2
  body_add_flextable(ar.2.1) %>%
  body_end_section_portrait() %>%
  body_add_flextable(ar.2.2) %>%
  body_add_break() %>%
  body_end_section_landscape() %>%
  # back in portrait
  body_add_flextable(ar.2.3) %>% body_add_break() %>%
  body_add_flextable(ar.2.4) %>% body_add_break() %>%
  body_add_flextable(ar.2.5) %>% body_add_break() %>%
  body_add_flextable(ar.2.6) %>% body_add_break() %>%
  body_add_flextable(ar.2.7) %>%
  
  # ar.3
  body_add_break() %>%
  body_add_flextable(ar.3.1) %>% body_add_break() %>%
  body_add_flextable(ar.3.2) %>%
  
  # portrait → landscape for ar.4.2
  body_add_break() %>%
  body_add_flextable(ar.4.1) %>%
  body_end_section_portrait() %>%
  body_add_flextable(ar.4.2) %>%
  body_add_break() %>%
  body_end_section_landscape() %>%
  # ar.5, ar.6
  body_add_flextable(ar.5.1) %>% body_add_break() %>%
  body_add_flextable(ar.6.1) %>% body_add_break() %>%
  body_add_flextable(ar.6.2) %>% body_add_break() %>%
  body_add_flextable(ar.6.3) %>% body_add_break() %>%
  
  # ar.7 (landscape)
  body_end_section_portrait() %>%
  body_add_flextable(ar.7.1) %>%
  body_add_break() %>%
  body_end_section_landscape() %>%
  # map
  body_add_par("Map of Examples (2024)", style = "Image Caption") %>%
  body_add_img(src = "final_combined_maps.png", width = 5.5, height = 7.5) %>%
  body_add_break() %>%
  
  # finish
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
