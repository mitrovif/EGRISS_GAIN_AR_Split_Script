# =============================================================================================================
# AR.6.2: Add Future Projects with Quarterly Breakdown
# =============================================================================================================

# Load required libraries
library(dplyr)
library(flextable)
library(readr)
library(writexl)
library(officer)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)  # For arranging plots

# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
background_color <- "#f0f8ff"

# Load dataset
group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.csv")
group_roster <- read.csv(group_roster_file)

group_roster_file2 <- file.path(working_dir, "analysis_ready_group_roster2.csv")
group_roster2 <- read.csv(group_roster_file2)

# =============================================================================================================
# Add Future Projects with Separate Tables for Quarterly and Population Breakdown
# =============================================================================================================

# Rename columns for clarity
group_roster2 <- group_roster2 %>%
  rename(
    FPR05.A = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...SURVEY`,
    FPR05.B = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...ADMINISTRATIVE.DATA`,
    FPR05.C = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...CENSUS`,
    FPR05.D = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...DATA.INTEGRATION`,
    FPR05.E = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...NON.TRADITIONAL`,
    FPR05.F = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...STRATEGY`,
    FPR05.G = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...GUIDANCE.TOOLKIT`,
    FPR05.H = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...H..WORKSHOP.TRAINING`,
    FPR05.X = `FPR05..What.data.sources.or.tools.do.you.plan.to.use.to.collect.or.improve.data.collection.on.these.populations.through.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span...OTHER`,
    FPR04.A = `FPR04..Which.populations.will.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span..focus.on.in.2025..REFUGEES`,
    FPR04.B = `FPR04..Which.populations.will.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span..focus.on.in.2025..IDPS`,
    FPR04.C = `FPR04..Which.populations.will.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span..focus.on.in.2025..STATELESSNESS`,
    FPR04.X = `FPR04..Which.populations.will.the..span.style..color..3b71b9..font.weight..bold....._FPR02...span..focus.on.in.2025..OTHER`
  ) %>%
  mutate(across(starts_with("FPR05."), ~ as.numeric(.))) %>%
  mutate(across(starts_with("FPR04."), ~ as.numeric(.)))

# Table 1: FPR05 as columns and Quarters as rows
quarter_summary <- group_roster2 %>%
  pivot_longer(
    cols = starts_with("FPR05."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Source_Variable = recode(Source_Variable,
                             "FPR05.A" = "Survey",
                             "FPR05.B" = "Administrative Data",
                             "FPR05.C" = "Census",
                             "FPR05.D" = "Data Integration",
                             "FPR05.E" = "Non-Traditional",
                             "FPR05.F" = "Strategy",
                             "FPR05.G" = "Guidance/Toolkit",
                             "FPR05.H" = "Workshop/Training",
                             "FPR05.X" = "Other"
    ),
    Quarter = case_when(
      q2025 == 1 ~ "Q1",
      q2025 == 2 ~ "Q2",
      q2025 == 3 ~ "Q3",
      q2025 == 4 ~ "Q4",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(Quarter, Source_Variable) %>%
  pivot_wider(names_from = Source_Variable, values_from = n, values_fill = 0)

# ==========================================================
# Table 2: FPR05 as columns and Populations as rows
# ==========================================================

population_summary <- group_roster2 %>%
  pivot_longer(
    cols = starts_with("FPR05."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Refugee = if_else(`FPR04.A` == 1, 1, 0),
    IDP = if_else(`FPR04.B` == 1, 1, 0),
    Stateless = if_else(`FPR04.C` == 1, 1, 0)
  ) %>%
  mutate(
    Population_Type = case_when(
      Refugee + IDP + Stateless == 1 & Refugee == 1 ~ "Refugee Only",
      Refugee + IDP + Stateless == 1 & IDP == 1 ~ "IDP Only",
      Refugee + IDP + Stateless == 1 & Stateless == 1 ~ "Stateless Only",
      Refugee + IDP + Stateless > 1 ~ "Mixed",
      TRUE ~ "Other"
    ),
    Source_Variable = recode(Source_Variable,
                             "FPR05.A" = "Survey",
                             "FPR05.B" = "Administrative Data",
                             "FPR05.C" = "Census",
                             "FPR05.D" = "Data Integration",
                             "FPR05.E" = "Non-Traditional",
                             "FPR05.F" = "Strategy",
                             "FPR05.G" = "Guidance/Toolkit",
                             "FPR05.H" = "Workshop/Training",
                             "FPR05.X" = "Other"
    )
  ) %>%
  count(Population_Type, Source_Variable) %>%
  pivot_wider(names_from = Source_Variable, values_from = n, values_fill = 0)

# Create FlexTables for Word
quarter_summary_flextable <- flextable(quarter_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(caption = "Future Projects Breakdown by Quarter")

population_summary_flextable <- flextable(population_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(caption = "Future Projects Breakdown by Population Type")

# Table 3: FPR05 as columns and Region as rows
region_summary <- group_roster2 %>%
  pivot_longer(
    cols = starts_with("FPR05."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Source_Variable = recode(Source_Variable,
                             "FPR05.A" = "Survey",
                             "FPR05.B" = "Administrative Data",
                             "FPR05.C" = "Census",
                             "FPR05.D" = "Data Integration",
                             "FPR05.E" = "Non-Traditional",
                             "FPR05.F" = "Strategy",
                             "FPR05.G" = "Guidance/Toolkit",
                             "FPR05.H" = "Workshop/Training",
                             "FPR05.X" = "Other"
    )
  ) %>%
  count(region, Source_Variable) %>%
  pivot_wider(names_from = Source_Variable, values_from = n, values_fill = 0)

# Create FlexTable for Regional Breakdown
region_summary_flextable <- flextable(region_summary) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(caption = "Future Projects Breakdown by Region")

# Add LOC01 Breakdown
group_roster2 <- group_roster2 %>%
  mutate(LOC01 = as.numeric(LOC01)) %>%
  mutate(
    LOC01_Category = case_when(
      LOC01 == 1 ~ "Country (NSO or Other in NSS)",
      LOC01 %in% c(2, 3) ~ "International (International Organization or CSO)",
      TRUE ~ "Other"
    )
  )

# Table 4: Separate LOC01 Breakdown
loc01_summary <- group_roster2 %>%
  pivot_longer(
    cols = starts_with("FPR05."),
    names_to = "Source_Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  mutate(
    Source_Variable = recode(Source_Variable,
                             "FPR05.A" = "Survey",
                             "FPR05.B" = "Administrative Data",
                             "FPR05.C" = "Census",
                             "FPR05.D" = "Data Integration",
                             "FPR05.E" = "Non-Traditional",
                             "FPR05.F" = "Strategy",
                             "FPR05.G" = "Guidance/Toolkit",
                             "FPR05.H" = "Workshop/Training",
                             "FPR05.X" = "Other"
    )
  ) %>%
  count(LOC01_Category, Source_Variable) %>%
  pivot_wider(names_from = Source_Variable, values_from = n, values_fill = 0)

# ==========================================================
# Combined Table for Quarters, Populations, and Regions
# ==========================================================

# Add common 'Label' column for merged structure
quarter_summary <- quarter_summary %>%
  mutate(Category = "Future Example Planned Start", Label = `Quarter`) %>%
  select(Category, Label, everything(), -`Quarter`)

population_summary <- population_summary %>%
  mutate(Category = "Future Example Population of Interest", Label = Population_Type) %>%  # Corrected to Population_Type
  select(Category, Label, everything(), -Population_Type)

region_summary <- region_summary %>%
  mutate(Category = "Future Example Region", Label = region) %>%
  select(Category, Label, everything(), -region)

loc01_summary <- loc01_summary %>%
  mutate(Category = "Future Example Lead", Label = LOC01_Category) %>%
  select(Category, Label, everything(), -LOC01_Category)

# Combine all tables
merged_data <- bind_rows(quarter_summary, population_summary, region_summary, loc01_summary)

# Clean up Category to show only once per section
merged_data$Category <- ifelse(duplicated(merged_data$Category), "", merged_data$Category)

# ==========================================================
# Create FlexTable for Merged Table with Column Totals Only
# ==========================================================

# Add total columns
merged_data <- merged_data %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(across(1, ~na_if(., ""))) %>%  # Convert empty strings to NA
  fill(1)

merged_summary_flextable <- flextable(merged_data) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  # bold(i = ~ Category == "Quarter", bold = TRUE, part = "body") %>%
  # bold(i = ~ Category == "Population", bold = TRUE, part = "body") %>%
  # bold(i = ~ Category == "Region", bold = TRUE, part = "body") %>%
  # bold(i = ~ Category == "Type of Example", bold = TRUE, part = "body") %>%
  autofit() %>%
  set_caption(caption = "Combined Future Projects Breakdown Including Type of Example with Column Totals")

# Output the tables
list(quarter_summary_flextable, population_summary_flextable, region_summary_flextable, merged_summary_flextable)

# Rename the merged flextable and apply beautification
header_color <- "#4cc3c9"

ar.6.2 <- flextable(merged_data) %>%
  theme_vanilla() %>%
  set_header_labels(
    Category = "Future Example",
    Label = ""
  ) %>%
  bold(part = "header") %>%
  bg(bg = header_color, part = "header") %>%
  color(color = "black", part = "header") %>%
  fontsize(size = 10, part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5), part = "all") %>%  # Keep horizontal borders
  bg(bg = "gray", i = 6:9) %>%
  bg(bg = "gray", i = 17:19) %>%
  bg(bg = "#c9daf8", j = ~ `Total`) %>%  # Highlight the Count column
  merge_v(j = 1) %>%  # Merge vertical cells in first column
  fontsize(size = 9, part = "body") %>%
  set_table_properties(layout = "autofit") %>%
  add_footer_row(
    values = paste0(
      "Table 6.2 is not featured in the 2024 Annual Report. Table is disaggregated by upcoming multiple sources (respondents to GAIN survey answered a multiple-choice question), by quarter, population of interest, region of implementation and future example-lead."
    ),
    colwidths = ncol(merged_data)
  ) %>%
  fontsize(size = 7, part = "footer") %>% 
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 6.2 Overview of future example implementation, by planned start date, population of interest, region, example lead, and data sources",
        props = fp_text(
          font.family = "Helvetica",
          font.size   = 10,
          italic      = FALSE
        )
      )
    )
  )%>%
  fix_border_issues()

ar.6.2 <- autofit(ar.6.2)

ar.6.2
