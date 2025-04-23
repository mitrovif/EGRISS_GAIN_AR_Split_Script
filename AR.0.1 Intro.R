# ======================================================
# Set Working Directory Dynamically
# ======================================================
# Copy-Paste your Windows file path (with backslashes)
working_dir <- "C:\\Users\\mitro\\UNHCR\\EGRISS Secretariat - Documents\\905 - Implementation of Recommendations\\01_GAIN Survey\\Integration & GAIN Survey\\EGRISS GAIN Survey 2024\\10 Data\\Analysis Ready Files\\Backup_2025-03-20_11-21-50"

# Automatically replace backslashes (\) with forward slashes (/)
working_dir <- gsub("\\\\", "/", working_dir)

# Set working directory
setwd(working_dir)

# Confirm the working directory
message("Working directory set to: ", getwd())

# ======================================================
# R Script for Enhanced GAIN 2024 Annual Report (Word)
# ======================================================

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
library(magick)
library(grid)

# EGRISS Color Scheme
primary_color <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color <- "#072d62"
background_color <- "#f0f8ff"

# Load dataset
group_roster_file <- file.path(working_dir, "analysis_ready_group_roster.csv")
group_roster <- read.csv(group_roster_file)

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
    add_footer_lines(values = "Source: GAIN 2024 Data") %>%
    set_caption(caption = title)
}

# ======================================================
# Sequentially Source Additional Scripts
# ======================================================

# Define the path for additional scripts
scripts_dir <- "C:/Users/mitro/UNHCR/EGRISS Secretariat - Documents/905 - Implementation of Recommendations/01_GAIN Survey/Integration & GAIN Survey/EGRISS GAIN Survey 2024/11 Reporting/AR Data Split Script"

# Overall files
source(file.path(scripts_dir, "AR.1.1 Overall.R"))
source(file.path(scripts_dir, "AR.1.2 Overall.R"))
source(file.path(scripts_dir, "AR.1.3 Overall.R"))
source(file.path(scripts_dir, "AR.1.4 Overall.R"))
source(file.path(scripts_dir, "AR.1.5 Overall.R"))

# National files
source(file.path(scripts_dir, "AR.2.1 National.R"))
source(file.path(scripts_dir, "AR.2.2 National.R"))
source(file.path(scripts_dir, "AR.2.3 National.R"))
source(file.path(scripts_dir, "AR.2.4 National.R"))
source(file.path(scripts_dir, "AR.2.5 National.R"))
source(file.path(scripts_dir, "AR.2.6 National.R"))

# Institutional files
source(file.path(scripts_dir, "AR.3.1 Institutional.R"))
source(file.path(scripts_dir, "AR.3.2 Institutional.R"))

# Future, GRF, and Map files
source(file.path(scripts_dir, "AR.4.1 Future.R"))
source(file.path(scripts_dir, "AR.4.2 Future.R"))
source(file.path(scripts_dir, "AR.5.1 GRF.R"))
source(file.path(scripts_dir, "AR.6.1 Activities.R"))
source(file.path(scripts_dir, "AR.6.2 Activities.R"))
source(file.path(scripts_dir, "AR.6.3 Activities.R"))
source(file.path(scripts_dir, "AR.8.1 Map.R"))

# Final Output Script
source(file.path(scripts_dir, "AR.0.2 Output.R"))





message("All scripts executed successfully!")
