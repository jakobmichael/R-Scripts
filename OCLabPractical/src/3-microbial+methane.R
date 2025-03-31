# Load required packages
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# Import and prepare data
df <- read_csv("data/PlotsInGroups.csv", col_names = FALSE)
microbial_start <- which(df[,1] == "3 Microbial + Methane") + 1
microbial_columns <- df[microbial_start, 2:12] |> as.character()
microbial_data <- df[(microbial_start+1):(microbial_start+3), 2:12]

# Set column names and convert to numeric
colnames(microbial_data) <- microbial_columns
microbial_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), microbial_data) |>
  mutate(across(-Sample, as.numeric))

# Convert to long format and maintain order
microbial_long <- pivot_longer(microbial_data, cols = -Sample, names_to = "Parameter", values_to = "Value") |>
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Calculate scale breaks
microbial_max_value <- max(microbial_long$Value, na.rm = TRUE)
microbial_rounded_max <- ceiling(microbial_max_value / 100) * 100
major_breaks <- c(0, 160, 200, 450, 700, microbial_rounded_max)

# Create minor lines (excluding those that match major breaks)
minor_lines <- setdiff(
  c(seq(0, 160, by = 10),
    seq(200, 450, by = 50),
    seq(700, microbial_rounded_max, by = 200)),
  major_breaks
)

# Create plot
ggplot(microbial_long, aes(x = Parameter, y = Value, fill = Sample)) +
  # Add minor grid lines (automatically excludes major break positions)
  geom_hline(yintercept = minor_lines, color = "gray80", linewidth = 0.3) +
  
  # Add bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Add axis breaks
  scale_y_break(c(160, 200), scales = 0.3) +
  scale_y_break(c(450, 700), scales = 0.3) +
  
  # Set scales
  scale_y_continuous(breaks = major_breaks, limits = c(0, microbial_rounded_max), expand = c(0, 0)) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Set1") +
  
  # Theme and labels
  labs(y = "concentration [ng/g]", x = "biomarkers", 
       title = "Microbial activity and methane cycles") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.9, color = "black"),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(l = 40, r = 5, t = 5, b = 5),
    legend.position = "top"
  ) +
  coord_cartesian(clip = "off")

