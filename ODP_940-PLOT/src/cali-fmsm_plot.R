source("./src/utils.R")

# Load data
file_path <- "./data/csv/155-940A_cali-fmsm.csv"

# Read the entire file as text
file_content <- readLines(file_path)

# Extract metadata (lines starting with "#")
metadata <- file_content[grep("^#", file_content)]

# Parse metadata into a list
metadata_list <- setNames(
  lapply(metadata, function(x) sub("^[^:]+:\\s*", "", x)),  # Extract values
  sub("^#([^:]+):.*$", "\\1", metadata)  # Extract keys
)

# Print the metadata
print(metadata_list)

# Skip metadata lines and read the data including column names and units
raw_data <- read.csv(file_path, skip = length(metadata), header = FALSE)

# Extract column names (first row after metadata)
col_names <- as.character(raw_data[1, ])
raw_data <- raw_data[-1, ]  # Remove the column names row

# Extract units (second row after metadata)
units <- as.character(raw_data[1, ])
raw_data <- raw_data[-1, ]  # Remove the units row

# Assign proper column names
colnames(raw_data) <- col_names

# Convert data to numeric where necessary
raw_data <- as.data.frame(lapply(raw_data, as.numeric))

# Remove last entry in raw data
raw_data <- raw_data[1:(nrow(raw_data) - 1), ]

min_depth <- min(raw_data$DEPTH_WMSF)
max_depth <- max(raw_data$DEPTH_WMSF)

# Create downcore plot
c1_plot <- plot_data(
      data = raw_data,
      min_depth = min_depth,
      max_depth = max_depth,
      y_resolution = 1,
      points_per_interval = 1,
      y_col = "DEPTH_WMSF",
      x_col = "C1",
      gap_threshold = 100,
      x_label = "C1",
      y_label = "Depth (m)",
      plot_title = "",
      line_color = "black",
      dot_color = "red"
) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Create downcore plot
c2_plot <- plot_data(
      data = raw_data,
      min_depth = min_depth,
      max_depth = max_depth,
      y_resolution = 1,
      points_per_interval = 1,
      y_col = "DEPTH_WMSF",
      x_col = "C2",
      gap_threshold = 100,
      x_label = "C2",
      y_label = "Depth (m)",
      plot_title = "",
      line_color = "black",
      dot_color = "red"
) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

mirrored_data_c1 <- transform(raw_data, C1 = -C1)
mirrored_data_c2 <- transform(raw_data, C2 = -C2)

custom_x_labels <- c("12", "10", "8", "6", "4", "2", "0")

c1_plot_mirrored <- plot_data(
  data = mirrored_data_c1,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "DEPTH_WMSF",
  x_col = "C1",
  gap_threshold = 100,
  x_label = "C1",
  y_label = "Depth (m)",
  plot_title = "",
  line_color = "black",
  dot_color = "red"
) + scale_x_continuous(
  position = "top",
  breaks = c(-12, -10, -8, -6, -4),
  labels = c("12", "10", "8", "6", "4")
)


c2_plot_mirrored <- plot_data(
  data = mirrored_data_c2,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "DEPTH_WMSF",
  x_col = "C2",
  gap_threshold = 100,
  x_label = "C2",
  y_label = "Depth (m)",
  plot_title = "",
  line_color = "black",
  dot_color = "red"
) + scale_x_continuous(
  position = "top",
  breaks = c(-12, -10, -8, -6, -4),
  labels = c("12", "10", "8", "6", "4")
)

# Combine the three plots into one with shared grid and y-axis
combined_plot <- c1_plot_mirrored + c1_plot + c2_plot_mirrored + c2_plot +
  plot_layout(ncol = 2, widths = c(1, 1)) + plot_annotation(
    title = "Combined Downcore Plot",
    subtitle = "C1 and C2 data",
    caption = "Source: Your Data"
  )

combined_plot
