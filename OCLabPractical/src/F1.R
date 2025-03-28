# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import and prepare data
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)
f1_start <- which(df[,1] == "F1") + 2
f1_columns <- df[f1_start, 2:8] |> as.character()
f1_data <- df[(f1_start+1):(f1_start+3), 2:8]

# Set column names and convert to numeric
colnames(f1_data) <- f1_columns
f1_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), f1_data) |>
  mutate(across(-Sample, as.numeric))

# Convert to long format and maintain order
f1_long <- pivot_longer(f1_data, cols = -Sample, names_to = "Parameter", values_to = "Value") |>
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
max_value <- max(f1_long$Value, na.rm = TRUE)
rounded_max <- ceiling(max_value / 100) * 100
major_breaks <- c(0, 5, 10, 15, 20, 25, 50, 100, 150, 300, 1500, rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 20, by = 2),
    seq(25, 50, by = 6),
    seq(50, 100, by = 10),
    seq(150, 300, by = 20),
    seq(1500, rounded_max, by = 500)),
  major_breaks
)

# Create plot
ggplot(f1_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(20, 25), scales = 0.3) +
  scale_y_break(c(100, 150), scales = 0.3) +
  scale_y_break(c(300, 1500), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "Parameter", 
       title = "F1 analysis with triple-axis break") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")