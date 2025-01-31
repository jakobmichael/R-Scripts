source("./src/utils.R")


# Read bulk density and gamma radiation data
file_path_bulk_density <- "./data/GRABulkDensity.txt"
file_path_gamma_radiation <- "./data/NaturalGammaRadiation.txt"

bulk_density <- read.delim(file_path_bulk_density, sep = "\t", header = TRUE)
gamma_radiation <- read.delim(file_path_gamma_radiation, sep = "\t", header = TRUE)

# Define shared depth range
min_depth <- 0
max_depth <- 270

# Create the bulk density plot
density_plot <- plot_data(
  data = bulk_density,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 2,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Density..g.cc.",
  gap_threshold = 2,
  x_label = "Density (g/cc)",
  y_label = "Depth (mbsf)",
  plot_title = "Bulk Density",
  line_color = "red",
  dot_color = "red"
)

# Create the gamma radiation plot
gamma_plot <- plot_data(
  data = gamma_radiation,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 2,
  points_per_interval = 1,
  y_col = "Depth..mbsf.",
  x_col = "Total.Counts",
  gap_threshold = 2,
  x_label = "Total Counts",
  y_label = NULL,
  plot_title = "Natural Gamma Radiation",
  line_color = "blue",
  dot_color = "blue"
) +
  theme(
    axis.title.y = element_blank(),   # Remove the y-axis title
    axis.text.y = element_blank(),    # Remove the y-axis tick labels
    axis.ticks.y = element_blank()    # Remove the y-axis ticks
  )


# Combine the two plots into one with shared grid and y-axis
combined_plot <- density_plot + gamma_plot +
  plot_layout(ncol = 2, widths = c(1, 1)) &
  theme(
    legend.position = "none" # Remove legends
  )

# Display the combined plot
print(combined_plot)
