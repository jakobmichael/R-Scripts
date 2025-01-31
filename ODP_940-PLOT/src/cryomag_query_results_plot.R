source("./src/utils.R")

# Read cryomag data
file_path_cryomag <- "./data/CryomagQueryResults.txt"
cryomag_data <- read.delim(file_path_cryomag, sep = "\t", header = TRUE)

head(cryomag_data)

# Define shared depth range
min_depth <- 0
max_depth <- max(cryomag_data$Depth..mbsf., na.rm = TRUE)

print(max_depth)

# Create the cryomag plot for Decl.
cryomag_decl_plot <- plot_data(
  data = cryomag_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Decl.",
  gap_threshold = 1,
  x_label = "Declination (°)",
  y_label = "Depth (mbsf)",
  plot_title = "Cryomag Declination Profile",
  line_color = "red",
  dot_color = "red"
)

# Define the cryomag plot for Incl.
cryomag_incl_plot <- plot_data(
  data = cryomag_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Incl.",
  gap_threshold = 1,
  x_label = "Inclination (°)",
  y_label = "",
  plot_title = "Cryomag Inclination Profile",
  line_color = "blue",
  dot_color = "blue"
) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Define the cryomag plot for Intensity
cryomag_intensity_plot <- plot_data(
  data = cryomag_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Intensity",
  gap_threshold = 1,
  x_label = "Intensity (nT)",
  y_label = "",
  plot_title = "Cryomag Intensity Profile",
  line_color = "green",
  dot_color = "green"
) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# Combine the three plots into one with shared grid and y-axis
combined_plot <- cryomag_decl_plot + cryomag_incl_plot + cryomag_intensity_plot +
  plot_layout(ncol = 3, widths = c(1, 1, 1)) &
  theme(
    legend.position = "none" # Remove legends
  )

combined_plot
