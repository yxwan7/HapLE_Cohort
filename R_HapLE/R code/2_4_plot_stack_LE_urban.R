# ==============================================================================
# TITLE:     LE estimates plot by sex
# DATE:      2025-04-28
# UPDATE:    2025-07-05
# ==============================================================================

#-----------------------------------------------------
# 1. Load Required Packages
#-----------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)

#-----------------------------------------------------
# 2. Load the Data
#-----------------------------------------------------
# Set the data path (matching 1_2 structure)
file_68_73 <- "../SAS_HapLE/Output/LE/sex+urban/LE_68_73_sex+urban_b300.txt"
file_74_79 <- "../SAS_HapLE/Output/LE/sex+urban/LE_74_79_sex+urban_b300.txt"
file_80_85 <- "../SAS_HapLE/Output/LE/sex+urban/LE_80_85_sex+urban_b300.txt"
file_86_91 <- "../SAS_HapLE/Output/LE/sex+urban/LE_86_91_sex+urban_b300.txt"

# Read the data files
df_68_73 <- read.table(file_68_73, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_74_79 <- read.table(file_74_79, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_80_85 <- read.table(file_80_85, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
df_86_91 <- read.table(file_86_91, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

# Add AgeGroup identifier (matching 1_2 structure)
df_68_73 <- df_68_73 %>% mutate(AgeGroup = "68-73")
df_74_79 <- df_74_79 %>% mutate(AgeGroup = "74-79")
df_80_85 <- df_80_85 %>% mutate(AgeGroup = "80-85")
df_86_91 <- df_86_91 %>% mutate(AgeGroup = "86-91")

#-----------------------------------------------------
# 3. Prepare the data for plot
#-----------------------------------------------------

# -------- Combine ALL FOUR data frames --------
combined_df <- bind_rows(df_68_73, df_74_79, df_80_85, df_86_91)

# -------- Select raw numbers needed for calculations --------
# Define Z-score for 95% CI
z_score <- qnorm(0.975)

# Select and prepare raw data for processing (matching 1_2 structure)
raw_data <- combined_df %>%
  filter(State == 0) %>%
  select(
    AgeGroup, urban, cohort,
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
    Urban = factor(urban, levels = c(1, 2), labels = c("Urban", "Rural")),
    Cohort_Label = factor(cohort, levels = c(1, 2), labels = c("Earlier", "Later")),
    # Update AgeGroup factor levels
    AgeGroup = factor(AgeGroup, levels = c("68-73", "74-79", "80-85", "86-91"))
  ) %>%
  select(-urban, -cohort) # Remove original numeric columns

# -------- Prepare Data specifically for Plotting --------
plot_data_long <- raw_data %>%
  select(AgeGroup, Urban, Cohort_Label, LEs1, LEs2) %>%
  pivot_longer(
    cols = starts_with("LEs"), names_to = "LE_Type", values_to = "LE_Value"
  ) %>%
  mutate(
    LE_Status = case_when(
      LE_Type == "LEs1" ~ "HapLE",
      LE_Type == "LEs2" ~ "UnHapLE",
      TRUE ~ NA_character_
    ),
    LE_Status = factor(LE_Status, levels = c("HapLE", "UnHapLE"))
  )

# --- Data for Error Bars (Total Life Expectancy) ---
error_data <- raw_data %>%
  mutate(
    TLE_LCI = TLE - z_score * TLE_STD,
    TLE_UCI = TLE + z_score * TLE_STD,
    HapLE_LCI = LEs1 - z_score * LEs1_STD,
    HapLE_UCI = LEs1 + z_score * LEs1_STD
  ) %>%
  select(AgeGroup, Urban, Cohort_Label, TLE, TLE_LCI, TLE_UCI, HapLE_LCI, HapLE_UCI)

# --- Data for Percentage Labels on HapLE sections ---
percentage_data <- raw_data %>%
  select(AgeGroup, Urban, Cohort_Label, LEp1, LEs1) %>%
  mutate(
    HapLE_percentage = round(LEp1 * 100, 0), # Convert to percentage and round
    y_position = LEs1 / 2 # Position at middle of HapLE section
  ) %>%
  select(AgeGroup, Urban, Cohort_Label, HapLE_percentage, y_position)

#-----------------------------------------------------
# 4. Create the Plots
#-----------------------------------------------------

# -------- Define Plotting Elements --------
# Updated harmonious orange color palette (Pantone-inspired)
color_palette <- c(
  "HapLE" = "#FFCC9A", # Light orange (Pantone 16-1546 TPX inspired)
  "UnHapLE" = "#E85D04" # Dark orange (Pantone 17-1462 TPX inspired)
)

# Updated theme to match reference image
plot_theme <- theme_bw(base_size = 12) +
  theme(
    # Panel and grid settings
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 1),

    # Strip settings for facets
    strip.background = element_rect(fill = "white", colour = "black", size = 1),
    strip.text = element_text(face = "bold", size = 12, color = "black"),

    # Legend settings
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.margin = margin(t = 10),

    # Title and axis settings
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black"),

    # Panel spacing
    panel.spacing = unit(0.5, "lines"),

    # Plot margins
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# -------- Create Stacked Plot --------
plot <- ggplot() +
  geom_col(
    data = plot_data_long,
    aes(x = Cohort_Label, y = LE_Value, fill = LE_Status),
    position = position_stack(reverse = TRUE), width = 0.6, color = "white", size = 0.3
  ) +
  geom_errorbar(
    data = error_data,
    aes(x = Cohort_Label, ymin = TLE_LCI, ymax = TLE_UCI),
    width = 0.2, color = "black", size = 0.5
  ) +
  geom_errorbar(
    data = error_data,
    aes(x = Cohort_Label, ymin = HapLE_LCI, ymax = HapLE_UCI),
    width = 0.2, color = "black", size = 0.5
  ) +
  geom_text(
    data = percentage_data,
    aes(x = Cohort_Label, y = y_position, label = paste0(HapLE_percentage, "%")),
    size = 5, color = "black", fontface = "bold"
  ) +
  facet_grid(Urban ~ AgeGroup,
    labeller = labeller(AgeGroup = function(x) paste("Age", x))
  ) +
  scale_fill_manual(
    values = color_palette,
    name = NULL,
    labels = c("HapLE", "UnHapLE")
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Birth Cohort",
    y = "Partial Life Expectancy"
  ) +
  plot_theme

# -------- Display the Plot --------
print(plot)

#-----------------------------------------------------
# 5. Save the Plots
#-----------------------------------------------------
# save the plot
ggsave("R output/2_4_HapLE_stacked_plots_urban.png", plot = plot, width = 8, height = 7, dpi = 300)
