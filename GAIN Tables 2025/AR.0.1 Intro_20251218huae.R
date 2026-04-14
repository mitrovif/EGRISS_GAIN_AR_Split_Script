# ======================================================
# Set Working Directory Dynamically
# ======================================================

# Confirm the working directory
message("Working directory set to: ", getwd())

# ======================================================
# R Script for Enhanced GAIN 2024 Annual Report (Word)
# ======================================================

# Load required libraries
library(dplyr)
library(flextable)
library(readr)
library(readxl)
library(writexl)
library(officer)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)  # For arranging plots
library(magick)
library(grid)
library(zoo)
library(openxlsx)


#TO EXPORT TABLES\
# = 1 export tables
# = 2 export world doc
# = 0 don't export
bExport = 1


# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
background_color <- "#f0f8ff"


working_dir <- "EGRISS GAIN Survey 2025/10 Data/01 Analysis Ready Files/"

# Load dataset
group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.xlsx")
group_roster <- read.xlsx(group_roster_file)

group_roster_file2 <- file.path(working_dir, "analysis_ready_group_roster2.xlsx")
group_roster2 <- read.xlsx(group_roster_file2)

# Function to create styled flextables
create_flextable <- function(data, title) {
  flextable(data) %>%
    theme_booktabs() %>%
    fontsize(size = 10) %>%
    bold(part = "header") %>%
    color(color = primary_color, part = "header") %>%
    bg(bg = background_color, part = "body") %>%
    border_outer(border = fp_border(color = accent_color, width = 2)) %>%
    border_inner_h(border = fp_border(color = secondary_color, width = 1)) %>%
    autofit() %>%
    add_footer_lines(values = "Source: GAIN 2025 Data") %>%
    set_caption(caption = title)
}

# ======================================================
# Sequentially Source Additional Scripts
# ======================================================

# Define the path for additional scripts
scripts_dir <- "EGRISS GAIN Survey 2025/08 Output/11 Script/02 New version/"

# Overall files
source(file.path(scripts_dir, "AR.1.1 Overall_20251218huae.R"))
source(file.path(scripts_dir, "AR.1.2 Overall_20251218huae.R"))
source(file.path(scripts_dir, "AR.1.3 Overall_20251218huae.R"))
source(file.path(scripts_dir, "AR.1.4.a Overall_20251218huae.R"))
source(file.path(scripts_dir, "AR.1.4.b Overall_20260120huae.R"))
source(file.path(scripts_dir, "AR.1.4.c Overall_20260120huae.R"))
source(file.path(scripts_dir, "AR.1.5.a_Overall_20260121huae.R"))
source(file.path(scripts_dir, "AR.1.5.b_Overall_20260121huae.R"))


# Analysis files
source(file.path(scripts_dir, "AR.2.1 Analysis_20251218huae.R"))
source(file.path(scripts_dir, "AR.2.2 Analysis_20251218huae.R"))
source(file.path(scripts_dir, "AR.2.3 Analysis_20251218huae.R"))
source(file.path(scripts_dir, "AR.2.4 Analysis_20251218huae.R"))
source(file.path(scripts_dir, "AR.2.5 Analysis_NewVersion_long_20260115huae.R"))
# source(file.path(scripts_dir, "AR.2.5 Analysis_20251219huae.R"))
source(file.path(scripts_dir, "AR.2.6 Analysis_20251219huae.R"))
source(file.path(scripts_dir, "AR.2.7 Analysis_20251219huae.R"))
source(file.path(scripts_dir, "AR.2.8 Analysis_20251219huae.R"))
source(file.path(scripts_dir, "AR.2.9 Analysis_20251219huae.R"))

# Methodological files
source(file.path(scripts_dir, "AR.3.1 Methodological_20251219huae.R"))
source(file.path(scripts_dir, "AR.3.2 Methodological_20251219huae.R"))
source(file.path(scripts_dir, "AR.3.2.b_Surveys_AllSources_Methodological_20260311huae.R"))

# Publication files
source(file.path(scripts_dir, "AR.4.1 Publications_20251222huae.R"))
source(file.path(scripts_dir, "AR.4.2 Publications_20251222huae.R"))

# GRF files
source(file.path(scripts_dir, "AR.5.1 GRF_20251222huae.R"))
source(file.path(scripts_dir, "AR.5.2 GRF_20251222huae.R"))

# Future files
source(file.path(scripts_dir, "AR.6.1 Future_20251222huae.R"))
source(file.path(scripts_dir, "AR.6.2 Future_20251222huae.R"))
source(file.path(scripts_dir, "AR.6.3_LOC01_Future_20251222huae.R"))

# Map files
source(file.path(scripts_dir, "AR.7.1 Map_20251223huae.R"))

# Final Output Script
source(file.path(scripts_dir, "AR.0.2 Output_20251218huae.R"))

message("All scripts executed successfully!")
