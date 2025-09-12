

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

# Helper function to center flextables
center_table <- function(ft) {
  set_table_properties(ft, layout = "autofit", width = 1, align = "center")
}

# Apply to all ar.x.y tables (adjust this list as needed)
table_names <- c(
  "ar.1.1", "ar.1.2", "ar.1.3", "ar.1.4",
  "ar.2.1", "ar.2.2", "ar.2.3", "ar.2.4", "ar.2.5", "ar.2.6", "ar.2.7", "ar.2.8", "ar.2.9",
  "ar.3.1", "ar.3.2",
  "ar.4.1", "ar.4.2",
  "ar.5.1", "ar.5.2",
  "ar.6.1", "ar.6.2",
  "ar.7.1"
)

for (name in table_names) {
  if (exists(name)) {
    assign(name, center_table(get(name)))
  }
}

# Define narrow‑margin layout including footer on all pages
# Wrap footer_section in block_list() as required by prop_section
default_section <- prop_section(
  page_size      = page_size(),
  page_margins   = page_mar(
    top    = 0.3,
    bottom = 0.3,
    left   = 0.7,
    right  = 0.7,
    header = 0.1,
    footer = 0.1
  ),
  footer_default = block_list(footer_section)
)

landscape_section <- prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(
    top    = 0.3,
    bottom = 0.3,
    left   = 0.1,
    right  = 0.1,
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
  "Section 1: Overall Trends:",
  "    Table 1.1: Trend of IRRS, IRIS and IROSS implementation examples, by year (AR: Figure 4, pg. 24)",
  "    Table 1.2 Breakdown of IRRS, IRIS and IROSS implementation examples in 2024 (AR: Figure 5, pg. 25)",
  "    Table 1.3: Breakdown of mixed-use examples of IRRS, IRIS and IROSS implementation by year (AR: pg. 24)",
  "    Table 1.4: Overview of data sources and tools, by year (AR: Figure 7, pg. 27 for country-led examples, pg. 49 for institution-led examples)",
  "",
  "Section 2: Analysis of example implementation:",
  "    Table 2.1: Count of countries reporting IRRS, IRIS and IROSS implementation examples, by region and year (Not in AR)",
  "    Table 2.2: Country-led examples of IRRS, IRIS and IROSS implementation, by region (AR: Figure 6, pg. 25)",
  "    Table 2.3: Institution-led examples, by level of implementation (AR: pg. 49)",
  "    Table 2.4: Components of IRRS, IRIS and IROSS recommendations most frequently used, by recommendation and by example lead (AR: pg. 27)",
  "    Table 2.5: Overview of respondents facing challenges with example implementation, including types of challenges faced (AR: Figure 8, pg. 28)",
  "    Table 2.6: Overview of respondents facing challenges with IRRS, IRIS and IROSS application, including types of challenges faced (Not in AR)",
  "    Table 2.7: Overview of country-led examples with implementation partnerships (AR: pg. 28)",
  "    Table 2.8: Mentions of institution-led partnership organisations in country-led examples (AR: pg. 28)",
  "    Table 2.9: Length of implementation (Not in AR)",
  "",
  "Section 3: Methodological developments:",
  "    Table 3.1: Analysis of focal points on GAIN based on their connection to migration statistics (Not in AR)",
  "    Table 3.2: Breakdown of identification questions used by example lead (Not in AR)",
  "",
  "Section 4: EGRISS publications:",
  "    Table 4.1: Visibility of IRRS, IRIS and IROSS publications (Not in AR)",
  "    Table 4.2: Impact of EGRISS-developed publications on example implementation(Not in AR)",
  "",
  "Section 5: GRF Pledges:",
  "    Table 5.1: Update on implementation of GRF pledges on statistical inclusion (Not in AR)",
  "    Table 5.2: Linkages of GAIN 2024 Examples with GRF pledges on statistical inclusion (Not in AR)",
  "",
  "Section 6: Future Examples:",
  "    Table 6.1: Future examples using different types of data sources or tools (AR: Figure 14, pg. 50)",
  "    Table 6.2 Overview of future example implementation, by planned start date, population of interest, region, example lead, and data sources (Not in AR)",
  "",
  "Section 7: Maps:",
  "    Map 7.1: Map of overall country-led examples (Not in AR)",
  "    7.2: Map of Overall Country-Led Examples Using the Recommendations (Not in AR)"
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
  body_add_break() %>% 
  
  # section 1
  body_add_flextable(ar.1.1) %>% body_add_break() %>%
  body_add_flextable(ar.1.2) %>% body_add_break() %>%
  body_add_flextable(ar.1.3) %>% body_add_break() %>%
  body_add_flextable(ar.1.4) %>% body_add_break() %>%
  
  # section 2
  body_add_flextable(ar.2.1) %>% body_add_break() %>%
  body_add_flextable(ar.2.3) %>% body_add_break() %>%
  body_add_flextable(ar.2.4) %>% body_add_break() %>%
  body_add_flextable(ar.2.5) %>% body_add_break() %>%
  body_add_flextable(ar.2.6) %>% body_add_break() %>%
  body_add_flextable(ar.2.7) %>% body_add_break() %>%
  body_add_flextable(ar.2.8) %>% body_add_break() %>%
  # portrait → landscape for tables 2.9 and 3.1
  body_end_section_portrait() %>%
  body_add_flextable(ar.2.9) %>% body_add_break() %>%
  
  # section 3
  body_add_flextable(ar.3.1) %>% body_add_break() %>%
  # landscape → portrait 
  body_end_section_landscape() %>% 
  body_add_flextable(ar.3.2) %>% body_add_break() %>%
  
  # section 4
  body_add_flextable(ar.4.1) %>% body_add_break() %>%
  body_add_flextable(ar.4.2) %>% body_add_break() %>%
  
  # section 5
  body_add_flextable(ar.5.1) %>% body_add_break() %>%
  body_add_flextable(ar.5.2) %>% body_add_break() %>%
  
  # section 6
  body_add_flextable(ar.6.1) %>% body_add_break() %>%
  # portrait → landscape for tables 6.2
  body_end_section_portrait() %>%
  body_add_flextable(ar.6.2) %>% body_add_break() %>%
  # landscape → portrait
  body_end_section_landscape() %>% 
  
  # section 7: maps
  body_add_par("Map of Examples (2024)", style = "Image Caption") %>%
  body_add_img(src = "final_combined_maps.png", width = 5.5, height = 7.5) %>%

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
