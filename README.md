# Split AR Script
 Split AR Script 
# GAIN 2024 Analysis - R Script Instructions

This repository contains the R scripts required to run the **GAIN 2024 Annual Report** analysis. Follow these steps to set up your environment and execute the scripts correctly.

---

## 📋 Prerequisites
Ensure you have the following installed:
- **R (v4.0 or higher)**
- **RStudio (recommended)**
- The following R libraries:

```r
install.packages(c("dplyr", "flextable", "readr", "writexl", "officer",
                   "tidyr", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "patchwork"))
```

---

## 🚀 How to Run the Analysis

### Step 1: Clone the Repository
Clone this repository to your local machine using:
```bash
git clone https://github.com/your-repo-name.git
cd your-repo-name
```

### Step 2: Set the Working Directory
In the main R script (`AR.0.1 Intro.R`), set your working directory by editing this section:
```r
# Set Working Directory
working_dir <- "C:\\Users\\mitro\\UNHCR\\EGRISS Secretariat - Documents\\905 - Implementation of Recommendations\\01_GAIN Survey\\Integration & GAIN Survey\\EGRISS GAIN Survey 2024\\10 Data\\Analysis Ready Files\\Backup_2025-03-12_10-04-14"

# Automatically replace backslashes with forward slashes for compatibility
working_dir <- gsub("\\\\", "/", working_dir)
setwd(working_dir)
```

Ensure the path matches your system's folder structure. Copy your desired path and replace it in the code.

### Step 3: Specify Script Directory Path
In the same script, ensure the path to additional scripts is correctly defined:
```r
scripts_dir <- "C:/Users/mitro/OneDrive/Desktop/AR Script"
```
Modify this path if the folder structure changes.

### Step 4: Run the Main Script
In RStudio, open `AR.0.1 Intro.R` and run the script by clicking **Run** or executing this command in your R console:

```r
source("AR.0.1 Intro.R")
```

The script will automatically:
✅ Set the working directory.  
✅ Load required libraries.  
✅ Source all relevant `.R` files in sequence.  
✅ Execute the final output generation script (`AR.0.2 Output.R`).

### Step 5: Review Outputs
- The processed data and report outputs will be generated in your specified working directory.
- Any generated Word, Excel, or visualization files will follow the naming conventions set in the code.

---

## 📂 File Structure
```
/project-root
 ├── AR.0.1 Intro.R  # Main script to run everything
 ├── AR.0.2 Output.R  # Final output generation
 ├── /AR Script/      # Folder containing other scripts
 └── /Data/           # Folder for analysis-ready data
```

---

## 🛠️ Troubleshooting
- **Error: Library not found?** Ensure you have installed all required packages.
- **Working Directory Error?** Confirm that your working directory path is set correctly in `AR.0.1 Intro.R`.
- **File Not Found?** Verify that your `.R` scripts and dataset files are in the expected folder locations.

If issues persist, please create an issue in this repository.



