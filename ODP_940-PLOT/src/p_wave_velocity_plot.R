source("./src/utils.R")



file_path_p_wave_velocity =   "data/P-WaveVelocity_PWLWholeCoreSystem_Data.txt"

p_wave_velocity_data <- read.table(file_path_p_wave_velocity, header = TRUE, sep = "\t")

head(p_wave_velocity_data)

min_depth <- 0
max_depth <- max(p_wave_velocity_data$Depth..mbsf.)

p_wave_velocity_plot <- plot_data(
  data = p_wave_velocity_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 0.25,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Vel..m.s.",
  gap_threshold = 10,
  x_label = "P-wave velocity (m/s)",
  y_label = "Depth (m)",
  plot_title = "P-wave Velocity Profile",
  line_color = "blue",
  dot_color = "blue"
)

p_wave_velocity_plot