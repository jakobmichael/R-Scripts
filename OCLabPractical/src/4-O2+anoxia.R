# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import and prepare data
df <- read_csv("data/PlotsInGroups.csv", col_names = FALSE)
oxygen_start <- which(df[,1] == "4 O2 + anoxia") + 1
oxygen_columns <- df[oxygen_start, 2:11] |> as.character()
oxygen_data <- df[(oxygen_start+1):(oxygen_start+3), 2:11]

# Set column names and convert to numeric
colnames(oxygen_data) <- oxygen_columns
oxygen_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), oxygen_data) |>
  mutate(across(-Sample, as.numeric))

# Convert to long format and maintain order
oxygen_long <- pivot_longer(oxygen_data, cols = -Sample, names_to = "Parameter", values_to = "Value") |>
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
oxygen_max_value <- max(oxygen_long$Value, na.rm = TRUE)
oxygen_rounded_max <- ceiling(oxygen_max_value / 100) * 100
major_breaks <- c(0, 250, 350, 650, 3600, 3800, 6000, oxygen_rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 250, by = 25),
    seq(350, 650, by = 100),
    seq(3600, 3800, by = 50),
    seq(6000, oxygen_rounded_max, by = 200)),
  major_breaks
)

# Create plot
ggplot(oxygen_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(250, 350), scales = 0.3) +
  scale_y_break(c(650, 3600), scales = 0.3) +
  scale_y_break(c(3800, 6000), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, limits = c(0, oxygen_rounded_max), expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "biomarkers", 
       title = "Oxygenation state and anoxic conditions") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")

