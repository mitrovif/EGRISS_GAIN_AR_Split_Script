
#============================================================================================================
#AR.1.5 Unique Country List
#============================================================================================================

# Function to list countries by region
list_countries_by_region <- function(group_roster) {
  df <- group_roster %>%
    filter(PRO09 == 1) %>%
    group_by(region, ryear) %>%
    summarise(countries = paste(unique(mcountry), collapse = ", ")) %>%
    arrange(region, desc(ryear)) %>%
    pivot_wider(names_from = region, values_from = countries, values_fill = "") %>%
    mutate(`Year` = as.character(ryear)) %>%
    relocate(`Year`, .before = everything())
  
  return(df)
}

# List of Countries by Region
ar.1.5 <- flextable(list_countries_by_region(group_roster)) %>%
  delete_columns(j = "ryear") %>%
  set_table_properties(width = 0.5, layout = "autofit") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = primary_color) %>%
  color(part = "header", color = "black") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  
  # Add footer row with the footnote text (after deleting the column)
  add_footer_row(values = "Footnote: This table presents the list of countries for each region based on metadata information.",
                 colwidths = ncol(list_countries_by_region(group_roster)) - 1) %>%  # Adjust colwidths due to the column deletion
  fontsize(size = 7, part = "footer") %>%
 set_caption(
  caption = as_paragraph(
    as_chunk(
      "AR.1.5: List of countries with country-led implementation using recommendations by region",
      props = fp_text(
        font.family = "Helvetica",
        font.size   = 10,
        italic      = FALSE
      )
    )
  )
)%>%
  fix_border_issues()
# Display Second Table
ar.1.5
