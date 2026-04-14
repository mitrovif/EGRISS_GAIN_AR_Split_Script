
# ======================================================
# AR.1.4.B Figure 7B - Step 1: Take the main PRO08 data `Population type`
# ======================================================

# Step 1: Prepare the data for National Examples (g_conled == 1)
contr <- group_roster %>%
  filter(year == 2025) %>% 
  group_by(year, PRO07, PRO08C) %>% 
  tally()


summary(group_roster$ryear)


contr <- group_roster %>%
  filter(year     == 2025) %>% 
  filter(g_conled == 1,
         is.na(PRO08C) == TRUE,
         PRO09    == 1) %>%
  # rename(Source = PRO08) %>% 
  # mutate(
  #   Source = ifelse(Source == 'ADMINISTRATIVE DATA', 'Administrative Data',
  #            ifelse(Source == 'CENSUS', 'Census',
  #            ifelse(Source == 'SURVEY', 'Survey',
  #            ifelse(Source == 'DATA INTEGRATION', 'Data Integration',
  #            ifelse(Source == 'NON-TRADITIONAL', 'Non-traditional',
  #            ifelse(Source %in% c('GUIDANCE/TOOLKIT', 'OTHER', 'STRATEGY', 'WORKSHOP/TRAINING'), 'Tools and Other**',
  #            ifelse(is.na(Source), 'Unknown', Source)))))))
  # ) %>% 
  group_by(PRO08, PRO08C) %>% 
  tally()



aggregated_national <- group_roster %>%
  filter(year == 2025) %>% 
  filter(g_conled == 1) %>% 
  
  #Added line to remove unknown
  filter(PRO08 %in% c('SURVEY', 'ADMINISTRATIVE DATA')) %>% 
  
  rename(`Population type` = PRO08C) %>% 
  mutate(
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using EGRISS Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using EGRISS Recommendations and Other*",
      TRUE ~ "Not Using EGRISS Recommendations and Other*"
    ),
    
    `Population type` = ifelse(is.na(`Population type`) == TRUE, 'Unknown',
             ifelse(`Population type` == 1, 'Populations of interest only',
             ifelse(`Population type` == 2, 'National population included',
             ifelse(`Population type` == 8, "Don't know", `Population type`))))
  ) %>% 
  group_by(`Use of Recommendations`, `Population type`, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(`Example Lead` = "Country-Led Examples")  



# Step 2: Prepare the data for Institutional Examples (g_conled == 2 or g_conled == 3)
aggregated_institutional <- group_roster %>%
  filter(year == 2025) %>% 
  filter(g_conled %in% c(2, 3)) %>%
  
  #Added line to remove unknown
  filter(PRO08 %in% c('SURVEY', 'ADMINISTRATIVE DATA')) %>% 
  
  rename(`Population type` = PRO08C) %>% 
  mutate(
    `Use of Recommendations` = case_when(
      PRO09 == 1 ~ "Using EGRISS Recommendations",
      PRO09 %in% c(2, 8) ~ "Not Using EGRISS Recommendations and Other*",
      TRUE ~ "Not Using EGRISS Recommendations and Other*"),
    
    `Population type` = ifelse(is.na(`Population type`) == TRUE, 'Unknown',
             ifelse(`Population type` == 1, 'Populations of interest only',
             ifelse(`Population type` == 2, 'National population included',
             ifelse(`Population type` == 8, "Don't know", `Population type`))))
  ) %>% 
  group_by(`Use of Recommendations`, `Population type`, ryear) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = ryear,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(`Example Lead` = "Institution-Led Examples")  



# Step 3: Combine both datasets
aggregated_data <- bind_rows(aggregated_national, aggregated_institutional) %>%
  mutate(
    `Use of Recommendations` = factor(
      `Use of Recommendations`,
      levels = c("Using EGRISS Recommendations", "Not Using EGRISS Recommendations and Other*")
    )
  ) %>%
  select(`Example Lead`, `Use of Recommendations`, `Population type`, `2025`) %>%
  arrange(
    `Example Lead`,
    `Use of Recommendations`,
    factor(`Population type`, levels = c("Populations of interest only", "National population included", "Don't know", "Unknown"))
  )

# Define borders
solid_border <- fp_border(color = "#3b71b3", width = 2, style = "solid")  # For "Using EGRISS Recommendations" (Graph Data)
dashed_border <- fp_border(color = "#3b71b3", width = 2, style = "dashed")  # For "Not Using EGRISS Recommendations and Other*" (Graph Data)
default_border <- fp_border(color = "black", width = 0.5)  # Default border for "Institution-Led Examples"

# Step 4: Beautify and create FlexTable for Word
ar.1.4.c <- flextable(aggregated_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#4cc3c9") %>%  # Light blue header
  merge_v(j = ~ `Example Lead`) %>%  
  merge_v(j = ~ `Use of Recommendations`) %>%  
  bg(bg = "#f4cccc", j = ~ `2025`) %>%   
  # bg(bg = "#c9daf8", j = ~ Total) %>%   
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%  # Inner borders
  fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>% 
  
  # **AutoFit for Better Readability**
  autofit() %>%
  
  # Apply colored borders for Graph DatCountry-Led Examples
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using EGRISS Recommendations"), 
         border.top = solid_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Using EGRISS Recommendations"), 
         border.bottom = solid_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using EGRISS Recommendations and Other*"), 
         border.top = dashed_border) %>%
  border(i = which(aggregated_data$`Example Lead` == "Country-Led Examples" & 
                     aggregated_data$`Use of Recommendations` == "Not Using EGRISS Recommendations and Other*"), 
         border.bottom = dashed_border) %>%
  
  # Reset to default borders for Overall Institution Examples
  border(i = which(aggregated_data$`Example Lead` == "Institution-Led Examples"), 
         border.top = fp_border(color = "gray", width = 0.5)) %>%
  border(i = which(aggregated_data$`Example Lead` == "Institution-Led Examples"), 
         border.bottom = fp_border(color = "gray", width = 0.5)) %>%
  
  border_outer(border = fp_border(color = "black", width = 2)) %>%  # Outer border
  
  # Footer details
  add_footer_row(
    values = paste0(
      "Table 1.4.c. supports Figure 7 in the 2025 Annual Report (replicated above). In addition to Figure 7 data focused on country-led examples, table shows institution-led examples, disaggregated by using and not using EGRISS Recommendations (and other), by main data source and tool and by year of reporting in GAIN.  
*Other in Not Using EGRISS Recommendations include: Don't Know and Not Reported
** Other in `Population type`s include: Non-Traditional, Strategy, Guidance/Toolkit, Workshop/Training and Other"
    ),
    colwidths = ncol(aggregated_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>%
  set_caption("Figure 1.4.c: Population groups covered in examples, 2025") %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 1.4.c: Population groups covered in examples, 2025",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

# Display Updated Table
print(ar.1.4.c)


if (bExport == 1){
  save_as_html(ar.1.4.c, path= "EGRISS GAIN Survey 2025/08 Output/13 Export Tables/ar.1.4.c.html") 
}