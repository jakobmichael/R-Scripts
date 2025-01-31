source("./src/utils.R")

# Read shear strength data
file_path_shear_strength <- "./data/ShearStrength.txt"
shear_strength_data <- read.delim(file_path_shear_strength, sep = "\t", header = TRUE)

head(shear_strength_data)

# Define shared depth range
min_depth <- 0
max_depth <- max(shear_strength_data$Depth..mbsf.)

# Create the shear strength plot
shear_strength_plot <- plot_data(
  data = shear_strength_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 0.01,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Shear_Strength",
  gap_threshold = 10,
  x_label = "Shear Strength (kPa)",
  y_label = "Depth (mbsf)",
  plot_title = "Shear Strength Profile",
  line_color = "green",
  dot_color = "green"
)

shear_strength_plot