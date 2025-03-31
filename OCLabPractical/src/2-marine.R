# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import and prepare data
df <- read_csv("data/PlotsInGroups.csv", col_names = FALSE)
marine_start <- which(df[,1] == "2 Marine") + 1
marine_columns <- df[marine_start, 2:16] |> as.character()
marine_data <- df[(marine_start+1):(marine_start+3), 2:16]

# Set column names and convert to numeric
colnames(marine_data) <- marine_columns
marine_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), marine_data) |>
  mutate(across(-Sample, as.numeric))

# Convert to long format and maintain order
marine_long <- pivot_longer(marine_data, cols = -Sample, names_to = "Parameter", values_to = "Value") |>
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
marine_max_value <- max(marine_long$Value, na.rm = TRUE)
marine_rounded_max <- 9700 #ceiling(marine_max_value / 100) * 100
major_breaks <- c(0, 300, 350, 750, 1000, 2000, 2500, marine_rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 300, by = 50),
    seq(350, 750, by = 100),
    seq(1000, 2000, by = 250),
    seq(2500, marine_rounded_max, by = 1200)),
  major_breaks
)

# Create plot
ggplot(marine_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(300, 350), scales = 0.3) +
  scale_y_break(c(750, 1000), scales = 0.3) +
  scale_y_break(c(2000, 2500), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "biomarkers", 
       title = "Marine primary production and planktonic origin") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")

