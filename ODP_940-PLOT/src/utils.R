# Define the function to filter first and last rows within intervals
filter_interval_points <- function(data, y_col, y_resolution, min_depth, max_depth) {
  data %>%
    mutate(interval = floor(!!sym(y_col) / y_resolution)) %>%
    group_by(interval) %>%
    filter(row_number() == 1 | row_number() == n()) %>% # Keep only existing first and last rows
    ungroup() %>%
    arrange(!!sym(y_col)) %>%
    filter(!!sym(y_col) >= min_depth & !!sym(y_col) <= max_depth)
}

plot_data <- function(
    data,
    min_depth = 0,
    max_depth = Inf,
    y_resolution = 1,
    points_per_interval = 2,
    y_col,
    x_col,
    gap_threshold,
    x_label,
    y_label,
    plot_title = "Plot",
    line_color,
    dot_color
) {
  # Check if the specified columns exist
  if (!all(c(y_col, x_col) %in% colnames(data))) {
    stop("Specified columns do not exist in the dataset.")
  }
  
  # Filter and prepare data
  data <- filter_interval_points(data, y_col, y_resolution, min_depth, max_depth)
  
  # Stop if no data points remain after filtering
  if (nrow(data) == 0) {
    stop("No data points within the specified depth range.")
  }
  
  # Handle gaps in the data
  data <- data %>%
    mutate(group = cumsum(c(0, diff(!!sym(y_col)) > gap_threshold)))
  
  # Create the plot
  p <- ggplot(data, aes(x = !!sym(x_col), y = !!sym(y_col), group = group)) +
    geom_path(color = line_color, linewidth = 0.8) +
    geom_point(color = dot_color, size = 1) +
    scale_y_reverse(
      limits = c(max_depth, min_depth), # Reverse y-axis (depth increases downward)
      breaks = seq(min_depth, max_depth, length.out = 5)
    ) +
    scale_x_continuous(position = "top") + # Place x-axis on top
    labs(
      title = plot_title,
      x = x_label,
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
