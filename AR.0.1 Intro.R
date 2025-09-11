# ======================================================
# Set Working Directory Dynamically
# ======================================================

# Copy-Paste your Windows file path (with backslashes)
working_dir <- "C:\\Users\\mitro\\UNHCR\\EGRISS Secretariat - Documents\\905 - Implementation of Recommendations\\01_GAIN Survey\\Integration & GAIN Survey\\EGRISS GAIN Survey 2024\\10 Data\\Analysis Ready Files\\Backup_Legacy"

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

group_roster_file2 <- file.path(working_dir, "analysis_ready_group_roster2.csv")
group_roster2 <- read.csv(group_roster_file2)

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
scripts_dir <- "C:/Users/Ladina/UNHCR/EGRISS Secretariat - Documents/905 - Implementation of Recommendations/01_GAIN Survey/Integration & GAIN Survey/EGRISS GAIN Survey 2024/11 Reporting/Split-AR-Script"

# Overall files
source(file.path(scripts_dir, "AR.1.1 Overall.R"))
source(file.path(scripts_dir, "AR.1.2 Overall.R"))
source(file.path(scripts_dir, "AR.1.3 Overall.R"))
source(file.path(scripts_dir, "AR.1.4 Overall.R"))

# Analysis files
source(file.path(scripts_dir, "AR.2.1 Analysis.R"))
source(file.path(scripts_dir, "AR.2.2 Analysis.R"))
source(file.path(scripts_dir, "AR.2.3 Analysis.R"))
source(file.path(scripts_dir, "AR.2.4 Analysis.R"))
source(file.path(scripts_dir, "AR.2.5 Analysis.R"))
source(file.path(scripts_dir, "AR.2.6 Analysis.R"))
source(file.path(scripts_dir, "AR.2.7 Analysis.R"))
source(file.path(scripts_dir, "AR.2.8 Analysis.R"))
source(file.path(scripts_dir, "AR.2.9 Analysis.R"))

# Methodological files
source(file.path(scripts_dir, "AR.3.1 Methodological.R"))
source(file.path(scripts_dir, "AR.3.2 Methodological.R"))

# Publication files
source(file.path(scripts_dir, "AR.4.1 Publications.R"))
source(file.path(scripts_dir, "AR.4.2 Publications.R"))

# GRF files
source(file.path(scripts_dir, "AR.5.1 GRF.R"))
source(file.path(scripts_dir, "AR.5.2 GRF.R"))

# Future files
source(file.path(scripts_dir, "AR.6.1 Future.R"))
source(file.path(scripts_dir, "AR.6.2 Future.R"))

# Map files
source(file.path(scripts_dir, "AR.7.1 Map.R"))

# Final Output Script
source(file.path(scripts_dir, "AR.0.2 Output.R"))

message("All scripts executed successfully!")
