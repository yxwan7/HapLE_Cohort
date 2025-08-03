# ==============================================================================
# TITLE:     LE estimates by sex
# DATE:      2025-07-05
# ==============================================================================

#-----------------------------------------------------
# 1. Load Required Packages
#-----------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#-----------------------------------------------------
# 2. Load the Data
#-----------------------------------------------------
# Set the data path

# All
file_68_73_all <- "../SAS_HapLE/Output/LE/sex_all/LE_68_73_sex_all_b300.txt"
file_74_79_all <- "../SAS_HapLE/Output/LE/sex_all/LE_74_79_sex_all_b300.txt"
file_80_85_all <- "../SAS_HapLE/Output/LE/sex_all/LE_80_85_sex_all_b300.txt"
file_86_91_all <- "../SAS_HapLE/Output/LE/sex_all/LE_86_91_sex_all_b300.txt"

# Read the data files
df_68_73_all <- read.table(file_68_73_all, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_74_79_all <- read.table(file_74_79_all, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_80_85_all <- read.table(file_80_85_all, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_86_91_all <- read.table(file_86_91_all, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

# Add AgeGroup identifier
df_68_73_all <- df_68_73_all %>% mutate(AgeGroup = "68-73", sex = 0)
df_74_79_all <- df_74_79_all %>% mutate(AgeGroup = "74-79", sex = 0)
df_80_85_all <- df_80_85_all %>% mutate(AgeGroup = "80-85", sex = 0)
df_86_91_all <- df_86_91_all %>% mutate(AgeGroup = "86-91", sex = 0)

# Men and Women
file_68_73 <- "../SAS_HapLE/Output/LE/sex/LE_68_73_sex_b300.txt"
file_74_79 <- "../SAS_HapLE/Output/LE/sex/LE_74_79_sex_b300.txt"
file_80_85 <- "../SAS_HapLE/Output/LE/sex/LE_80_85_sex_b300.txt"
file_86_91 <- "../SAS_HapLE/Output/LE/sex/LE_86_91_sex_b300.txt"


# Read the data files
df_68_73 <- read.table(file_68_73, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_74_79 <- read.table(file_74_79, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_80_85 <- read.table(file_80_85, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_86_91 <- read.table(file_86_91, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

# Add AgeGroup identifier
df_68_73 <- df_68_73 %>% mutate(AgeGroup = "68-73")
df_74_79 <- df_74_79 %>% mutate(AgeGroup = "74-79")
df_80_85 <- df_80_85 %>% mutate(AgeGroup = "80-85")
df_86_91 <- df_86_91 %>% mutate(AgeGroup = "86-91")

#-----------------------------------------------------
# 3. Prepare the data for tables
#-----------------------------------------------------

# -------- Combine ALL FOUR data frames --------
combined_df <- bind_rows(df_68_73, df_74_79, df_80_85, df_86_91, df_68_73_all, df_74_79_all, df_80_85_all, df_86_91_all)

# -------- Select raw numbers needed for calculations--------
# Define Z-score for 95% CI
z_score <- qnorm(0.975)

# Helper function for significance stars (VECTORIZED version)
get_stars <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# Select and prepare raw data for processing
raw_data <- combined_df %>%
  filter(State == 0) %>%
  select(
    AgeGroup, sex, cohort,
    # TLE
    TLE, TLE_STD,
    # HapLE
    LEs1, LEs1_STD, LEp1, LEp1_STD,
    # UnHapLE
    LEs2, LEs2_STD, LEp2, LEp2_STD
  ) %>%
  # Convert all selected estimate and STD columns to numeric
  mutate(across(c(TLE:LEp2_STD), as.numeric)) %>%
  mutate(
    Sex = factor(sex, levels = c(0, 1, 2), labels = c("Overall", "Men", "Women")),
    Cohort_Label = factor(cohort, levels = c(1, 2), labels = c("Earlier", "Later")),
    # Update AgeGroup factor levels
    AgeGroup = factor(AgeGroup, levels = c("68-73", "74-79", "80-85", "86-91"))
  ) %>%
  select(-sex, -cohort) # Remove original numeric columns

#-----------------------------------------------------
# 4. Calculate CI and Differences
#-----------------------------------------------------
cohort_results_base <- raw_data %>%
  mutate(
    # --- Calculate CIs for ALL measures ---
    TLE_LCI = TLE - z_score * TLE_STD, TLE_UCI = TLE + z_score * TLE_STD,
    LEs1_LCI = LEs1 - z_score * LEs1_STD, LEs1_UCI = LEs1 + z_score * LEs1_STD,
    LEp1_LCI = LEp1 - z_score * LEp1_STD, LEp1_UCI = LEp1 + z_score * LEp1_STD,
    LEs2_LCI = LEs2 - z_score * LEs2_STD, LEs2_UCI = LEs2 + z_score * LEs2_STD,
    LEp2_LCI = LEp2 - z_score * LEp2_STD, LEp2_UCI = LEp2 + z_score * LEp2_STD
  )

# --- Create LEs Table for Cohorts ---
cohort_results_les <- cohort_results_base %>%
  mutate(
    # --- Format LEs results ---
    `TLE` = sprintf("%.2f (%.2f, %.2f)", TLE, TLE_LCI, TLE_UCI),
    `HapLE` = sprintf("%.2f (%.2f, %.2f)", LEs1, LEs1_LCI, LEs1_UCI),
    `UnHapLE` = sprintf("%.2f (%.2f, %.2f)", LEs2, LEs2_LCI, LEs2_UCI),
    `HapLE%` = sprintf("%.1f (%.1f, %.1f)", LEp1 * 100, LEp1_LCI * 100, LEp1_UCI * 100),
    `UnHapLE%` = sprintf("%.1f (%.1f, %.1f)", LEp2 * 100, LEp2_LCI * 100, LEp2_UCI * 100)
  ) %>%
  select(
    `Age Range` = AgeGroup, Sex, Cohort = Cohort_Label,
    `TLE`, `HapLE`, `HapLE%`, `UnHapLE`, `UnHapLE%`
  )

# -------- Pivot wider to calculate differences --------
wide_df <- raw_data %>%
  pivot_wider(
    names_from = Cohort_Label,
    values_from = c(
      TLE, TLE_STD, LEs1, LEs1_STD, LEp1, LEp1_STD, LEs2, LEs2_STD, LEp2, LEp2_STD
    ),
    names_sep = "_"
  )

# -------- Calculate differences, CIs, and P-values for ALL measures  --------
difference_results_base <- wide_df %>%
  mutate(
    # --- TLE ---
    TLE_Diff = TLE_Later - TLE_Earlier,
    TLE_Diff_STD = sqrt(TLE_STD_Later^2 + TLE_STD_Earlier^2),
    TLE_Diff_Z = TLE_Diff / TLE_Diff_STD,
    TLE_Diff_P = 2 * pnorm(-abs(TLE_Diff_Z)),
    TLE_Diff_LCI = TLE_Diff - z_score * TLE_Diff_STD,
    TLE_Diff_UCI = TLE_Diff + z_score * TLE_Diff_STD,
    TLE_Formatted = paste0(sprintf("%.2f (%.2f, %.2f)", TLE_Diff, TLE_Diff_LCI, TLE_Diff_UCI), get_stars(TLE_Diff_P)),

    # --- HapLE ---
    LEs1_Diff = LEs1_Later - LEs1_Earlier,
    LEs1_Diff_STD = sqrt(LEs1_STD_Later^2 + LEs1_STD_Earlier^2),
    LEs1_Diff_Z = LEs1_Diff / LEs1_Diff_STD,
    LEs1_Diff_P = 2 * pnorm(-abs(LEs1_Diff_Z)),
    LEs1_Diff_LCI = LEs1_Diff - z_score * LEs1_Diff_STD,
    LEs1_Diff_UCI = LEs1_Diff + z_score * LEs1_Diff_STD,
    LEs1_Formatted = paste0(sprintf("%.2f (%.2f, %.2f)", LEs1_Diff, LEs1_Diff_LCI, LEs1_Diff_UCI), get_stars(LEs1_Diff_P)),

    # --- HapLE% ---
    LEp1_Diff = LEp1_Later - LEp1_Earlier,
    LEp1_Diff_STD = sqrt(LEp1_STD_Later^2 + LEp1_STD_Earlier^2),
    LEp1_Diff_Z = LEp1_Diff / LEp1_Diff_STD,
    LEp1_Diff_P = 2 * pnorm(-abs(LEp1_Diff_Z)),
    LEp1_Diff_LCI = LEp1_Diff - z_score * LEp1_Diff_STD,
    LEp1_Diff_UCI = LEp1_Diff + z_score * LEp1_Diff_STD,
    LEp1_Formatted = paste0(sprintf("%.1f (%.1f, %.1f)", LEp1_Diff * 100, LEp1_Diff_LCI * 100, LEp1_Diff_UCI * 100), get_stars(LEp1_Diff_P)),

    # --- UnHapLE ---
    LEs2_Diff = LEs2_Later - LEs2_Earlier,
    LEs2_Diff_STD = sqrt(LEs2_STD_Later^2 + LEs2_STD_Earlier^2),
    LEs2_Diff_Z = LEs2_Diff / LEs2_Diff_STD,
    LEs2_Diff_P = 2 * pnorm(-abs(LEs2_Diff_Z)),
    LEs2_Diff_LCI = LEs2_Diff - z_score * LEs2_Diff_STD,
    LEs2_Diff_UCI = LEs2_Diff + z_score * LEs2_Diff_STD,
    LEs2_Formatted = paste0(sprintf("%.2f (%.2f, %.2f)", LEs2_Diff, LEs2_Diff_LCI, LEs2_Diff_UCI), get_stars(LEs2_Diff_P)),

    # --- UnHapLE% ---
    LEp2_Diff = LEp2_Later - LEp2_Earlier,
    LEp2_Diff_STD = sqrt(LEp2_STD_Later^2 + LEp2_STD_Earlier^2),
    LEp2_Diff_Z = LEp2_Diff / LEp2_Diff_STD,
    LEp2_Diff_P = 2 * pnorm(-abs(LEp2_Diff_Z)),
    LEp2_Diff_LCI = LEp2_Diff - z_score * LEp2_Diff_STD,
    LEp2_Diff_UCI = LEp2_Diff + z_score * LEp2_Diff_STD,
    LEp2_Formatted = paste0(sprintf("%.1f (%.1f, %.1f)", LEp2_Diff * 100, LEp2_Diff_LCI * 100, LEp2_Diff_UCI * 100), get_stars(LEp2_Diff_P))
  )

#-----------------------------------------------------
# 5. Create LEs and LEp Tables
#-----------------------------------------------------

# --- Create LEs Table for Differences ---
difference_results_les <- difference_results_base %>%
  select(
    `Age Range` = AgeGroup, Sex,
    `TLE` = TLE_Formatted,
    `HapLE` = LEs1_Formatted,
    `HapLE%` = LEp1_Formatted,
    `UnHapLE` = LEs2_Formatted,
    `UnHapLE%` = LEp2_Formatted
  ) %>%
  mutate(Cohort = "Diff.") %>%
  select(`Age Range`, Sex, Cohort, everything()) # Ensure correct column order


# --- Combine LEs Table ---
combined_results_les <- bind_rows(
  cohort_results_les,
  difference_results_les
) %>%
  mutate(
    Cohort = factor(Cohort, levels = c("Earlier", "Later", "Diff."))
  ) %>%
  arrange(`Age Range`, Sex, Cohort)


#-----------------------------------------------------
# 6. Construct the results tables using kableExtra
#-----------------------------------------------------
caption_text_base <- paste(
  "(Significance of difference: * p<0.05, ** p<0.01, *** p<0.001)"
)

# --- Construct LEs Table (HTML version) ---
caption_les <- paste("Life Expectancy by Sex: Estimates, Differences, and 95% Confidence Intervals.", caption_text_base)
table_les_kable <- combined_results_les %>%
  kable(caption = caption_les, booktabs = TRUE, linesep = "", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  column_spec(1:2, bold = TRUE) %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  row_spec(which(combined_results_les$Cohort == "Diff."),
    italic = TRUE, background = "#f2f2f2"
  )


# --- Construct LEs Table (LaTeX version) ---
table_les_latex <- combined_results_les %>%
  kable(format = "latex", caption = caption_les, booktabs = TRUE, linesep = "", escape = FALSE) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
  column_spec(1:2, bold = TRUE) %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  row_spec(which(combined_results_les$Cohort == "Diff."),
    italic = TRUE
  )

# --- Display LEs Table ---
print(table_les_kable)

# Add a separator between tables
cat("\n\n---\n\n")


#-----------------------------------------------------
# 7. Save the Tables
#-----------------------------------------------------

# Export HTML tables
filename_les_html <- "R output/1_2_life_expectancy_sex.html"

save_kable(table_les_kable, file = filename_les_html, self_contained = TRUE)

# Export LaTeX tables
filename_les_latex <- "R output/1_2_life_expectancy_sex.tex"

save_kable(table_les_latex, file = filename_les_latex)

# Export underlying data frames to CSV
write.csv(combined_results_les, "R output/1_2_life_expectancy_sex.csv", row.names = FALSE)
