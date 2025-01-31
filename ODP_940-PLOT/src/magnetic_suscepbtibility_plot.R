source("./src/utils.R")

# Read magnetic susceptibility data
file_path_magnetic_susceptibility <- "./data/MagneticSusceptibility.txt"

col_names <- c("Leg", "Site", "H", "Core", "T", "Sec", "Top (cm)", "Depth (mbsf)", "Magnetic Suscept.", "Corrected Suscept.", ".")

magnetic_susceptibility_data <- read.delim(file_path_magnetic_susceptibility, 
                                           header = TRUE,
                                           sep = "\t")

magnetic_susceptibility_data <- magnetic_susceptibility_data[, 1:(ncol(magnetic_susceptibility_data) - 2)]

# Check structure of the data
head(magnetic_susceptibility_data)

# Define shared depth range
min_depth <- 0
max_depth <- max(magnetic_susceptibility_data$Depth..mbsf., na.rm = TRUE)

# Create the magnetic susceptibility plot
magnetic_susceptibility_plot <- plot_data(
  data = magnetic_susceptibility_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Magnetic.Suscept.",
  gap_threshold = 2,
  x_label = "Magnetic Susceptibility (SI)",
  y_label = "Depth (m)",
  plot_title = "Magnetic Susceptibility Profile",
  line_color = "purple",
  dot_color = "purple"
)

magnetic_susceptibility_plot
