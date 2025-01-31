source("./src/utils.R")

# Load data
file_path <- "./data/csv/155-940A_cntg.csv"

# Read the entire file as text
file_content <- readLines(file_path)

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

nphi_plot <- plot_data(
  data = raw_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "DEPTH_WMSF",
  x_col = "NPHI",
  gap_threshold = 100,
  x_label = "NPHI",
  y_label = "Depth (m)",
  plot_title = "Plot",
  line_color = "darkgreen",
  dot_color = "darkgreen"
)

enph_plot <- plot_data(
  data = raw_data,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col = "DEPTH_WMSF",
  x_col = "ENPH",
  gap_threshold = 100,
  x_label = "ENPHI",
  y_label = "Depth (m)",
  plot_title = "Plot",
  line_color = "darkblue",
  dot_color = "darkblue"
)+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

combined_plot <- nphi_plot + enph_plot +
  plot_layout(ncol = 3, widths = c(1, 1, 1)) &
  theme(
    legend.position = "none" # Remove legends
  )

combined_plot