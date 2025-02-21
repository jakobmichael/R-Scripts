# Load necessary libraries
library(ggplot2)
library(scales)  # For rescaling sedimentation rate

# Read the dataset
data <- read.csv("./data/agemodel.csv", header=TRUE, sep=",")

# Define depth limits
depth_min <- 0
depth_max <- 700  # Fixed depth range

# Define fixed sedimentation rate limits (0 to 7)
sed_rate_min <- 0
sed_rate_max <- 7

# Scale sedimentation rate for correct alignment with depth
scale_factor <- depth_max / sed_rate_max  # Ensures sedimentation rate fits within 0-7

# Create the combined plot with shared x-axis
combined_plot <- ggplot() +
  
  # Age-Depth Plot (Left Y-Axis: Reversed Depth)
  geom_line(data=data, aes(x=age_new..a.BP., y=depth_new...cm.), color="black", size=1) +
  geom_line(data=data, aes(x=age_old..a.BP., y=depth_old..cm.), color="darkgrey", linetype="dotted", size=1) +
  geom_point(data=data, aes(x=age_new..a.BP., y=depth_new...cm.), color="black", size=1.5) +
  geom_point(data=data, aes(x=age_old..a.BP., y=depth_old..cm.), color="darkgrey", size=1.5) +
  
  # Sedimentation Rate Plot (Right Y-Axis: Normal Scale)
  geom_line(data=data, aes(x=age_new..a.BP..1, y=depth_max - (sed.rate_new..m.ka. * scale_factor)), color="blue", size=1) +
  geom_line(data=data, aes(x=age_old..a.BP..1, y=depth_max - (sed.rate_old..m.ka. * scale_factor)), color="blue", linetype="dotted", size=1) +
  
  # X-Axis at the Top
  scale_x_continuous(position = "top", breaks = seq(0, 30000, by = 5000)) +
  
  # Primary Y-Axis (Depth) -> Reversed
  scale_y_reverse(
    name="depth [cm]",
    limits=c(depth_max, depth_min - 50 ),
    breaks = seq(0, 800, by = 100),
    minor_breaks = seq(50, 650, by = 100),
    sec.axis = sec_axis(
      ~ (depth_max - .) / scale_factor, 
      name="sedimentation rate [m/ka]", 
      breaks=seq(sed_rate_min - 50, sed_rate_max, by=1), 
      labels=seq(sed_rate_min - 50, sed_rate_max, by=1)
    )
  ) +
 
  # Labels and themes
  labs(x="age [cal yr B.P.]", title="Age Model and Sedimentation Rate") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(size = 12, face = "bold"),  # Bold and size for x-axis text
    axis.text.y = element_text(size = 12, face = "bold"),  # Bold and size for y-axis text
    axis.title.x = element_text(size = 14, face = "bold"), # Bold and size for x-axis label
    axis.title.y = element_text(size = 14, face = "bold"), # Bold and size for y-axis label
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.3)
  )

# Display the combined plot
combined_plot

