
# =====================================================================
# GRF06_roster cleaning & enrichment (GAIN 2024) — v2
#  - Join 1 (by pindex2): analysis_ready_main_roster  -> morganization, mcountry, LOC01
#  - Join 2 (by name):    analysis_ready_group_roster -> region, g_conled, gLOC01, PRO08.*, PRO09, PRO10.*
# =====================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# --------------------------- Paths -----------------------------------
base_dir <- "C:/Users/Ladina/UNHCR/EGRISS Secretariat - Documents/905 - Implementation of Recommendations/01_GAIN Survey/Integration & GAIN Survey/EGRISS GAIN Survey 2024"

in_roster_path  <- file.path(base_dir, "05 Data Collection", "Data Archive", "Final Version", "GRF06_roster.csv")
in_main_path    <- file.path(base_dir, "10 Data", "Analysis Ready Files", "analysis_ready_main_roster.csv")
in_group_path   <- file.path(base_dir, "10 Data", "Analysis Ready Files", "analysis_ready_group_roster.csv")

out_dir         <- file.path(base_dir, "10 Data", "Analysis Ready Files")
out_path        <- file.path(out_dir, "GRF06_roster_clean.csv")

# ----------------------- Read datasets --------------------------------
grf06 <- readr::read_csv(in_roster_path, show_col_types = FALSE, guess_max = 100000)
main  <- readr::read_csv(in_main_path,   show_col_types = FALSE, guess_max = 100000)
group <- readr::read_csv(in_group_path,  show_col_types = FALSE, guess_max = 100000)

# --------------------- Create pindex2 ---------------------------------
stopifnot("_parent_index" %in% names(grf06))
grf06 <- grf06 %>%
  mutate(
    .parent_num = suppressWarnings(as.integer(.data[["_parent_index"]])),
    pindex2     = paste0("2024", sprintf("%04d", .parent_num))
  ) %>%
  select(-.parent_num)

# --------------------- Recode GRF06 -----------------------------------
grf06_col <- names(grf06)[
  str_detect(names(grf06), regex("^GRF06\\b|Please indicate if.*GRF pledges", ignore_case = TRUE))
][1]
stopifnot(!is.na(grf06_col))

map_to_num <- function(x){
  x_trim <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x_trim %in% c("1","yes","y","true")                 ~ 1L,
    x_trim %in% c("2","no","n","false")                 ~ 2L,
    x_trim %in% c("8","don’t know","don't know","dk")   ~ 8L,
    is.na(x) | x_trim %in% c("", "na", "n/a", "9")      ~ 9L,
    TRUE                                                ~ 9L
  )
}
num_to_label <- function(v){
  case_when(
    v == 1L ~ "Yes",
    v == 2L ~ "No",
    v == 8L ~ "Don't know",
    v == 9L ~ "Blank",
    TRUE    ~ NA_character_
  )
}

grf06 <- grf06 %>%
  mutate(
    GRF06_num = map_to_num(.data[[grf06_col]]),
    GRF06     = num_to_label(GRF06_num)
  )

# --------------------- Join 1: by pindex2 (from MAIN) -----------------
if (!("pindex2" %in% names(main))) {
  idx_col <- names(main)[str_detect(names(main), "^(_)?index$|^_?parent_index$", ignore_case = TRUE)][1]
  stopifnot(!is.na(idx_col))
  main <- main %>%
    mutate(
      .idx_num = suppressWarnings(as.integer(.data[[idx_col]])),
      pindex2  = paste0("2024", sprintf("%04d", .idx_num))
    ) %>% select(-.idx_num)
}
# --- Normalize pindex2 types before Join 1 ---
# If pindex2 already exists in MAIN as numeric, coerce to character and left-pad to 8
if ("pindex2" %in% names(main)) {
  main <- main %>%
    mutate(
      pindex2 = as.character(pindex2),
      # ensure width 8 in case something lost padding (e.g., 20240003)
      pindex2 = stringr::str_pad(pindex2, width = 8, side = "left", pad = "0")
    )
} else {
  # (re)create pindex2 from index if missing
  idx_col <- names(main)[stringr::str_detect(names(main), "^(_)?index$|^_?parent_index$", ignore_case = TRUE)][1]
  stopifnot(!is.na(idx_col))
  main <- main %>%
    mutate(
      .idx_num = suppressWarnings(as.integer(.data[[idx_col]])),
      pindex2  = paste0("2024", sprintf("%04d", .idx_num))
    ) %>% select(-.idx_num)
}

# Ensure GRF06 side is character and width 8 too
grf06 <- grf06 %>%
  mutate(
    pindex2 = as.character(pindex2),
    pindex2 = stringr::str_pad(pindex2, width = 8, side = "left", pad = "0")
  )

need1 <- c("morganization","mcountry","LOC01")
by_pindex_cols <- main %>%
  select(pindex2, any_of(need1)) %>%
  distinct()

grf06_enriched <- grf06 %>%
  left_join(by_pindex_cols, by = "pindex2")

# --------------------- Join 2: by name (from GROUP) -------------------
std <- function(x) str_squish(str_to_lower(str_replace_all(as.character(x), "\\s+", " ")))

# Identify name columns
pro_name_col <- names(grf06)[str_detect(names(grf06), regex("^pro_?name$", ignore_case = TRUE))][1]
stopifnot(!is.na(pro_name_col))

pro02a_col <- names(group)[str_detect(names(group), regex("^PRO02A$", ignore_case = TRUE))][1]
stopifnot(!is.na(pro02a_col))

need2 <- c(
  "region","g_conled","gLOC01",
  "PRO08.A","PRO08.B","PRO08.C","PRO08.D","PRO08.E","PRO08.F","PRO08.G","PRO08.H","PRO08.X",
  "PRO09",
  "PRO10.A","PRO10.B","PRO10.C","PRO10.Z"
)
have2 <- intersect(need2, names(group))

# De-dup group by standardized name (if multiple rows per PRO02A exist, take first non-missing per var)
group_by_name <- group %>%
  mutate(PRO02A_std = std(.data[[pro02a_col]])) %>%
  group_by(PRO02A_std) %>%
  summarise(
    across(all_of(have2), ~ {x <- .x; x[match(TRUE, !is.na(x))]} , .names = "{.col}"),
    .groups = "drop"
  )

grf06_enriched <- grf06_enriched %>%
  mutate(pro_name_std = std(.data[[pro_name_col]])) %>%
  left_join(group_by_name, by = c("pro_name_std" = "PRO02A_std")) %>%
  select(-pro_name_std)

# --------------------- Save output ------------------------------------
preferred_order <- c("pindex2","GRF06_num","GRF06","morganization","mcountry","LOC01",
                     "region","g_conled","gLOC01",
                     "PRO08.A","PRO08.B","PRO08.C","PRO08.D","PRO08.E","PRO08.F","PRO08.G","PRO08.H","PRO08.X",
                     "PRO09","PRO10.A","PRO10.B","PRO10.C","PRO10.Z")

cols <- c(intersect(preferred_order, names(grf06_enriched)),
          setdiff(names(grf06_enriched), preferred_order))

grf06_enriched %>%
  select(all_of(cols)) %>%
  write_csv(out_path, na = "")

message("✅ Done. File saved to: ", out_path)

# =====================================================================
# AR.GRF06.1 — Linkage to GRF Pledges (Overall, Country-led, Institution-led)
# Source: GRF06_roster_clean.csv (GAIN 2024)
# =====================================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(flextable); library(officer)  # for fp_border(), fp_text()
})

# ---- Paths ----
base_dir <- "C:/Users/Ladina/UNHCR/EGRISS Secretariat - Documents/905 - Implementation of Recommendations/01_GAIN Survey/Integration & GAIN Survey/EGRISS GAIN Survey 2024"
in_clean <- file.path(base_dir, "10 Data", "Analysis Ready Files", "GRF06_roster_clean.csv")

# ---- Read ----
grf <- read_csv(in_clean, show_col_types = FALSE, guess_max = 100000)

# ---- Safety: ensure needed vars ----
stopifnot("GRF06" %in% names(grf) | "GRF06_num" %in% names(grf))
stopifnot("g_conled" %in% names(grf))

# Standardize GRF06 label if needed
if (!"GRF06" %in% names(grf)) {
  grf <- grf %>%
    mutate(GRF06 = case_when(
      GRF06_num == 1L ~ "Yes",
      GRF06_num == 2L ~ "No",
      GRF06_num == 8L ~ "Don't know",
      GRF06_num == 9L ~ "Blank",
      TRUE            ~ NA_character_
    ))
}

# ---- Keep only valid g_conled (1/2) and map to buckets ----
grf <- grf %>%
  filter(as.character(g_conled) %in% c("1", "2")) %>%
  mutate(
    conled_bucket = if_else(as.character(g_conled) == "1", "Country-Led", "Institution-Led"),
    linkage_bucket = case_when(
      GRF06 == "Yes"                          ~ "GAIN Examples Linked to GRF Pledge",
      GRF06 == "No"                           ~ "GAIN Examples Not Linked to GRF Pledge",
      GRF06 %in% c("Don't know", "Blank") | is.na(GRF06)
      ~ "Don't Know or No Response on Linkage",
      TRUE                                    ~ "Don't Know or No Response on Linkage"
    )
  )

# ---- Build table (rows = linkage buckets within each slice) ----
order_slices <- c("Overall", "Country-Led", "Institution-Led")
order_rows   <- c("GAIN Examples Linked to GRF Pledge",
                  "GAIN Examples Not Linked to GRF Pledge",
                  "Don't Know or No Response on Linkage")

df_long <- bind_rows(
  grf %>% mutate(slice = "Overall"),
  grf %>% filter(conled_bucket == "Country-Led") %>% mutate(slice = "Country-Led"),
  grf %>% filter(conled_bucket == "Institution-Led") %>% mutate(slice = "Institution-Led")
) %>%
  mutate(slice = factor(slice, levels = order_slices)) %>%
  group_by(slice, linkage_bucket) %>%
  summarise(Total = n(), .groups = "drop") %>%
  group_by(slice) %>%
  complete(linkage_bucket = order_rows, fill = list(Total = 0)) %>%
  ungroup() %>%
  mutate(linkage_bucket = factor(linkage_bucket, levels = order_rows)) %>%
  arrange(slice, linkage_bucket)

tbl_rows <- df_long %>%
  transmute(
    `Example Category` = slice,
    `Relation of Example to GRF Pledges of the Organisation` = linkage_bucket,
    Total
  )

# ---- Flextable styling (EGRISS palette) ----
primary_color   <- "#4cc3c9"
secondary_color <- "#3b71b3"
accent_color    <- "#072d62"

header_border <- fp_border(color = "black", width = 1)
heavy_border  <- fp_border(color = "black", width = 2)
# group_border  <- fp_border(color = secondary_color, width = 1.2)

first_rows <- which(!duplicated(tbl_rows$`Example Category`))

ar.5.2 <- flextable(tbl_rows) %>%
  theme_vanilla() %>%
  set_header_labels(
    `Example Category` = "Example Category",
    `Relation of Example to GRF Pledges of the Organisation` =
      "Relation of Example to GRF Pledges of the Organisation",
    Total = "Total"
  ) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  bg(part = "header", bg = primary_color) %>%
  color(part = "header", color = "black") %>%
  merge_v(j = "Example Category") %>%
  border_outer(border = heavy_border) %>%
  bg(j = ~ Total, bg = "#c9daf8") %>%  # Highlight the total column
  autofit() %>%
  set_caption(
    caption = as_paragraph(
      as_chunk(
        "Table 5.2: Linkages of GAIN 2024 Examples with GRF pledges on statistical inclusion",
        props = fp_text(font.size = 10)
      )
    )
  ) %>%
  fix_border_issues()

ar.5.2
