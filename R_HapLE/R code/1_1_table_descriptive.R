# ==============================================================================
# TITLE:     Descriptive analysis
# DATE:      2025-07-05
# ==============================================================================

#-----------------------------------------------------
# 1. Load data and packages
#-----------------------------------------------------
library(tidyverse)

if (!exists("d_full_all")) {
  source(file = "R code/0_1_data_process_HapLE.R")
}

#-----------------------------------------------------
# 2. Cohort overlapping table (Code from original script)
#-----------------------------------------------------
# First, filter samples with lost=0
follow_up_sample_overlap <- d_full_all %>% # Use a distinct name if needed
  filter(lost == 0)

# Count how many IDs appear in both cohorts by age group
overlap_by_age <- follow_up_sample_overlap %>%
  select(id, cohort, age_group) %>%
  # Group by ID and age_group, count distinct cohorts for each ID
  group_by(id, age_group) %>%
  summarize(cohort_count = n_distinct(cohort), .groups = "drop") %>%
  # Filter for IDs that appear in both cohorts (count = 2)
  filter(cohort_count == 2) %>%
  # Count the number of overlapping IDs by age group
  group_by(age_group) %>%
  summarize(overlap_count = n())

# Calculate the total count of unique IDs by age group
total_by_age <- follow_up_sample_overlap %>%
  select(id, age_group) %>%
  distinct() %>%
  group_by(age_group) %>%
  summarize(total_count = n())

# Merge the tables and calculate the overlap percentage
overlap_percentage <- total_by_age %>%
  left_join(overlap_by_age, by = "age_group") %>%
  mutate(
    # Replace NA with 0 for age groups with no overlap
    overlap_count = ifelse(is.na(overlap_count), 0, overlap_count),
    # Calculate the percentage of overlap
    overlap_percentage = round(overlap_count / total_count * 100, 2)
  )

# Print the resulting table
print("Cohort Overlap Percentage by Age Group:")
print(overlap_percentage)


#-----------------------------------------------------
# 3. Descriptive tables
#-----------------------------------------------------

# -------- By cohort --------

# Filter for samples with lost=0
follow_up_sample_desc <- d_full_all %>% # Use a distinct name if needed
  filter(lost == 0)

# Create a detailed breakdown of the sample by cohort, age_group, and sex
sample_breakdown <- follow_up_sample_desc %>%
  # Create gender category
  mutate(
    gender = case_when(
      sex == 1 ~ "men",
      sex == 2 ~ "women",
      TRUE ~ NA_character_
    )
  ) %>%
  # Create indicator variables for different characteristics
  mutate(
    is_urban = ifelse(urban == 1, 1, 0),
    is_literate = ifelse(edu == 1, 1, 0),
    is_happy = ifelse(H == 1, 1, 0),
    is_disabled = ifelse(A == 1, 1, 0)
  ) %>%
  # Group by cohort, age_group, and gender
  group_by(cohort, age_group, gender) %>%
  summarize(
    # Sample size
    n = n(),
    # Mean age
    mean_age = mean(age, na.rm = TRUE),
    # Percentages of different characteristics
    pct_urban = mean(is_urban, na.rm = TRUE) * 100,
    pct_literate = mean(is_literate, na.rm = TRUE) * 100,
    pct_happy = mean(is_happy, na.rm = TRUE) * 100,
    pct_disabled = mean(is_disabled, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  # Round all percentages and mean age to one decimal place
  mutate(across(c(mean_age, starts_with("pct_")), ~ round(., 1)))

# Calculate the combined statistics for men and women (all)
all_genders <- follow_up_sample_desc %>%
  mutate(
    is_urban = ifelse(urban == 1, 1, 0),
    is_literate = ifelse(edu == 1, 1, 0),
    is_happy = ifelse(H == 1, 1, 0),
    is_disabled = ifelse(A == 1, 1, 0)
  ) %>%
  group_by(cohort, age_group) %>%
  summarize(
    gender = "all",
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    pct_urban = mean(is_urban, na.rm = TRUE) * 100,
    pct_literate = mean(is_literate, na.rm = TRUE) * 100,
    pct_happy = mean(is_happy, na.rm = TRUE) * 100,
    pct_disabled = mean(is_disabled, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_age, starts_with("pct_")), ~ round(., 1)))

# Combine both datasets
full_breakdown <- bind_rows(sample_breakdown, all_genders) %>%
  arrange(cohort, age_group, factor(gender, levels = c("men", "women", "all"))) # Ensure 'all' is last

# Calculate the percentage of total within each cohort and age group
full_breakdown_pct <- full_breakdown %>%
  group_by(cohort, age_group) %>%
  mutate(
    total_n = sum(n[gender != "all"]), # Calculate total based on men and women counts
    pct_of_total = ifelse(gender != "all", round(n / total_n * 100, 1), NA)
  ) %>%
  ungroup()

# Select and reorder the columns for better readability
result_table <- full_breakdown_pct %>%
  select(
    cohort, age_group, gender, n, pct_of_total, mean_age,
    pct_urban, pct_literate, pct_happy, pct_disabled
  )

# Print the result table
print("Descriptive Statistics by Cohort (result_table):")
print(result_table, n = Inf)

# -------- By age group --------

# Function to calculate statistics for each cohort
calculate_stats <- function(data, cohort_num) {
  data %>%
    filter(cohort == cohort_num) %>%
    # Create gender category
    mutate(
      gender = case_when(
        sex == 1 ~ "men",
        sex == 2 ~ "women",
        TRUE ~ NA_character_
      )
    ) %>%
    # Create indicator variables for different characteristics
    mutate(
      is_urban = ifelse(urban == 1, 1, 0),
      is_literate = ifelse(edu == 1, 1, 0),
      is_happy = ifelse(H == 1, 1, 0),
      is_disabled = ifelse(A == 1, 1, 0)
    ) %>%
    # Group by age_group and gender
    group_by(age_group, gender) %>%
    summarize(
      # Sample size
      n = n(),
      # Mean age
      mean_age = mean(age, na.rm = TRUE),
      # Percentages of different characteristics
      pct_urban = mean(is_urban, na.rm = TRUE) * 100,
      pct_literate = mean(is_literate, na.rm = TRUE) * 100,
      pct_happy = mean(is_happy, na.rm = TRUE) * 100,
      pct_disabled = mean(is_disabled, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    # Round all percentages and mean age to one decimal place
    mutate(across(c(mean_age, starts_with("pct_")), ~ round(., 1)))
}

# Calculate statistics for all genders combined
calculate_all_gender_stats <- function(data, cohort_num) {
  data %>%
    filter(cohort == cohort_num) %>%
    mutate(
      is_urban = ifelse(urban == 1, 1, 0),
      is_literate = ifelse(edu == 1, 1, 0),
      is_happy = ifelse(H == 1, 1, 0),
      is_disabled = ifelse(A == 1, 1, 0)
    ) %>%
    group_by(age_group) %>%
    summarize(
      gender = "all",
      n = n(),
      mean_age = mean(age, na.rm = TRUE),
      pct_urban = mean(is_urban, na.rm = TRUE) * 100,
      pct_literate = mean(is_literate, na.rm = TRUE) * 100,
      pct_happy = mean(is_happy, na.rm = TRUE) * 100,
      pct_disabled = mean(is_disabled, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(across(c(mean_age, starts_with("pct_")), ~ round(., 1)))
}

# Get stats for both cohorts using the descriptive sample
cohort1_stats <- calculate_stats(follow_up_sample_desc, 1)
cohort2_stats <- calculate_stats(follow_up_sample_desc, 2)
cohort1_all_stats <- calculate_all_gender_stats(follow_up_sample_desc, 1)
cohort2_all_stats <- calculate_all_gender_stats(follow_up_sample_desc, 2)

# Combine stats for gender-specific data
cohort1_stats_full <- bind_rows(cohort1_stats, cohort1_all_stats) %>%
  arrange(age_group, factor(gender, levels = c("men", "women", "all"))) # Ensure 'all' is last
cohort2_stats_full <- bind_rows(cohort2_stats, cohort2_all_stats) %>%
  arrange(age_group, factor(gender, levels = c("men", "women", "all"))) # Ensure 'all' is last

# Calculate percentage of total for each gender within each age group
cohort1_stats_final <- cohort1_stats_full %>%
  group_by(age_group) %>%
  mutate(
    total_n = sum(n[gender != "all"]),
    pct_of_total = ifelse(gender != "all", round(n / total_n * 100, 1), NA)
  ) %>%
  ungroup()

cohort2_stats_final <- cohort2_stats_full %>%
  group_by(age_group) %>%
  mutate(
    total_n = sum(n[gender != "all"]),
    pct_of_total = ifelse(gender != "all", round(n / total_n * 100, 1), NA)
  ) %>%
  ungroup()

# Merge the two cohorts side by side
comparison_table <- cohort1_stats_final %>%
  full_join(
    cohort2_stats_final,
    by = c("age_group", "gender"),
    suffix = c("_c1", "_c2")
  ) %>%
  # Reorder columns for better readability
  select(
    age_group, gender,
    n_c1, n_c2, pct_of_total_c1, pct_of_total_c2,
    mean_age_c1, mean_age_c2,
    pct_urban_c1, pct_urban_c2,
    pct_literate_c1, pct_literate_c2,
    pct_happy_c1, pct_happy_c2,
    pct_disabled_c1, pct_disabled_c2
  ) %>%
  # Arrange rows logically
  arrange(age_group, factor(gender, levels = c("men", "women", "all")))

# Print the comparison table
print("Descriptive Statistics Comparison by Age Group (comparison_table):")
print(comparison_table, n = Inf)

# --- SAVE comparison_table ---
# Save as CSV
write.csv(result_table, "R output/1_1_descriptive_cohort.csv", row.names = FALSE, na = "")
write.csv(comparison_table, "R output/1_1_descriptive_age.csv", row.names = FALSE, na = "")
