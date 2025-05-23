# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import and prepare data
df <- read_csv("data/PlotsInGroups.csv", col_names = FALSE)
terrestrial_start <- which(df[,1] == "1 Terrestrial") + 1
terrestrial_columns <- df[terrestrial_start, 2:15] |> as.character()
terrestrial_data <- df[(terrestrial_start+1):(terrestrial_start+3), 2:15]

# Set column names and convert to numeric
colnames(terrestrial_data) <- terrestrial_columns
terrestrial_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), terrestrial_data) |>
  mutate(across(-Sample, as.numeric))

# Convert to long format and maintain order
terrestrial_long <- pivot_longer(terrestrial_data, cols = -Sample, names_to = "Parameter", values_to = "Value") |>
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
terrestrial_max_value <- max(terrestrial_long$Value, na.rm = TRUE)
terrestrial_rounded_max <- ceiling(terrestrial_max_value / 100) * 100
major_breaks <- c(0, 300, 400, 900, 1300, terrestrial_rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 300, by = 25),
    seq(400, 900, by = 100),
    seq(1300, terrestrial_rounded_max, by = 800)),
  major_breaks
)

# Create plot
ggplot(terrestrial_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(300, 400), scales = 0.3) +
  scale_y_break(c(900, 1300), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "biomarkers", 
       title = "Terrestrial") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")

