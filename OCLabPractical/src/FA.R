# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import CSV file
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Extract the "fa" table
fa_start <- which(df[,1] == "FA") + 1
fa_columns <- df[fa_start, 2:14] |> as.character()

# Clean column names - replace empty or NA with meaningful names
fa_columns <- ifelse(is.na(fa_columns) | fa_columns == "", 
                     paste0("Param_", seq_along(fa_columns)), 
                     fa_columns)

fa_data <- df[(fa_start+1):(fa_start+3), 2:14]

# Set column names and convert to numeric
colnames(fa_data) <- fa_columns
fa_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), fa_data) %>%
  mutate(across(-Sample, ~ {
    # Convert to numeric, handling any non-numeric values
    num_val <- suppressWarnings(as.numeric(.))
    ifelse(is.na(num_val), 0, num_val) # Replace NA with 0 or handle as needed
  }))

# Convert to long format and maintain order
fa_long <- pivot_longer(fa_data, cols = -Sample, names_to = "Parameter", values_to = "Value") %>%
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
max_value <- max(fa_long$Value, na.rm = TRUE)
rounded_max <- ceiling(max_value / 100) * 100
major_breaks <- c(0, 5, 10, 15, 20, 90, 300, 800, rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 20, by = 2),
    seq(90, 300, by = 20),
    seq(800, rounded_max, by = 500)),
  major_breaks
)

# Create plot
ggplot(fa_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(20, 90), scales = 0.3) +
  scale_y_break(c(300, 1000), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "Parameter", 
       title = "fa analysis with triple-axis break") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")