# ======================================================
# Add to Word document
# ======================================================
library(officer)

# narrow‐margin definition
default_section <- prop_section(
  page_size    = page_size(),
  page_margins = page_mar(
    top    = 0.3,
    bottom = 0.3,
    left   = 0.5,
    right  = 0.5,
    header = 0.1,
    footer = 0.1
  )
)

word_doc <- read_docx() %>%
  body_set_default_section(default_section) %>%
  
  # === portrait start ===
  body_add_par("GAIN Survey Annex to EGRISS 2024 Annual Report", style = "centered") %>%
  
  # ar.1 tables
  body_add_flextable(ar.1.1) %>% body_add_break() %>%
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
  body_add_flextable(ar.2.6) %>%
  
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
  body_add_par("Map of Examples (2024)", style = "heading 2") %>%
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
