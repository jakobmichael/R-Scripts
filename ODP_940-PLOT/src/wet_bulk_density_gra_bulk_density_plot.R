source("./src/utils.R")


plot_data_from_two_sets_in_one_plot <- function(
    data1,                  # First dataset
    data2 = NULL,           # Optional second dataset
    min_depth = 0,
    max_depth = Inf,
    y_resolution = 1,
    points_per_interval = 2,
    y_col1,                 # Depth column for the first dataset
    x_col1,                 # X-axis variable for the first dataset
    y_col2 = NULL,          # Depth column for the second dataset (optional)
    x_col2 = NULL,          # X-axis variable for the second dataset (optional)
    gap_threshold_1,
    gap_threshold_2,
    x_label1,               # Label for the first x-axis
    x_label2 = NULL,        # Label for the second x-axis (optional)
    y_label,
    plot_title = "Plot",
    line_color1,
    dot_color1,
    line_color2 = NULL,     # Line color for the second dataset (optional)
    dot_color2 = NULL,
    line_width1,
    line_width2,
    dot_size1,
    dot_size2               # Dot color for the second dataset (optional)
) {
  # Check if the specified columns exist in the first dataset
  if (!all(c(y_col1, x_col1) %in% colnames(data1))) {
    stop("Specified columns for the first dataset do not exist.")
  }
  
  
  data1 <- filter_interval_points(data1, y_col1, y_resolution, min_depth, max_depth)
  
  # Stop if no data points remain after filtering
  if (nrow(data1) == 0) {
    stop("No data points in the first dataset within the specified depth range.")
  }
  
  # Handle gaps in the first dataset
  data1 <- data1 %>%
    mutate(group = cumsum(c(0, diff(!!sym(y_col1)) > gap_threshold_1)))
  
  # Initialize the plot with the first dataset
  p <- ggplot(data1, aes(x = !!sym(x_col1), y = !!sym(y_col1), group = group)) +
    geom_path(color = line_color1, linewidth = line_width1) +
    geom_point(color = dot_color1, size = dot_size1)
  
  # If a second dataset is provided, overlay it
  if (!is.null(data2) && !is.null(y_col2) && !is.null(x_col2)) {
    # Check if the specified columns exist in the second dataset
    if (!all(c(y_col2, x_col2) %in% colnames(data2))) {
      stop("Specified columns for the second dataset do not exist.")
    }
    
    # Filter and prepare the second dataset
    data2 <- filter_interval_points(data2, y_col2, y_resolution, min_depth, max_depth)
    
    # Stop if no data points remain after filtering
    if (nrow(data2) == 0) {
      stop("No data points in the second dataset within the specified depth range.")
    }
    
    # Handle gaps in the second dataset
    data2 <- data2 %>%
      mutate(group = cumsum(c(0, diff(!!sym(y_col2)) > gap_threshold_2)))
    
    # Add the second dataset to the plot
    p <- p +
      geom_path(data = data2, aes(x = !!sym(x_col2), y = !!sym(y_col2), group = group), 
                color = line_color2, linewidth = line_width2) +
      geom_point(data = data2, aes(x = !!sym(x_col2), y = !!sym(y_col2)), 
                 color = dot_color2, size = dot_size2)
  }
  
  # Finalize the plot
  p <- p +
    scale_y_reverse(
      limits = c(max_depth, min_depth),
      breaks = seq(min_depth, max_depth, length.out = 5)
    ) +
    scale_x_continuous(
      position = "top",
      name = ifelse(!is.null(x_label2), x_label1, x_label1)
    ) +
    labs(
      title = plot_title,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 14, vjust = 1),
      axis.title.y = element_text(size = 14),
      panel.grid.major = element_line(color = "grey", size = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Read bulk density and gamma radiation data
file_path_bulk_density <- "./data/GRABulkDensity.txt"
file_path_moisture_density <- "./data/MoistureAndDensity.txt"

bulk_density <- read.delim(file_path_bulk_density, sep = "\t", header = TRUE)
moisture_density <- read.delim(file_path_moisture_density, sep = "\t", header = TRUE)

# Define shared depth range
min_depth <- 0
max_depth <- 250




plot_data_from_two_sets_in_one_plot(
  data1 = bulk_density,
  data2 = moisture_density,
  min_depth = min_depth,
  max_depth = max_depth,
  y_resolution = 1,
  points_per_interval = 1,
  y_col1 = "Depth..mbsf.",
  x_col1 = "Density..g.cc.",
  y_col2 = "Depth..mbsf.",
  x_col2 = "BD..g.cc.",
  gap_threshold_1 = 1,
  gap_threshold_2 = 10,
  x_label1 = "Bulk Density (g/cc)",
  x_label2 = "Moisture Density (g/cc)",
  y_label = "Depth (mbsf)",
  plot_title = "Bulk Density vs. Moisture Density",
  line_color1 = "blue",
  dot_color1 = "blue",
  line_color2 = "red",
  dot_color2 = "red",
  line_width1 = 0.8,
  line_width2 = 1.2,
  dot_size1 = 1,
  dot_size2 = 1.1
)