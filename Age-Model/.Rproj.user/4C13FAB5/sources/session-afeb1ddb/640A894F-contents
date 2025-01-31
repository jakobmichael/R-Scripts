# Load data and clean column names
age_model_data <- read.delim(
  "./data/GeoB7608-1_radiocarbon_age.tab",
  header = TRUE,
  skip = 24,  # Skip metadata lines
  sep = "\t"
)
names(age_model_data) <- c(
  "Depth_sed_m", "Sample_label", "Age_dated_ka", "Age_dated_ka_BP",
  "Age_e_plus", "Age_e_minus", "Cal_age_ka_BP", "Cal_age_std_dev",
  "Comment", "Dated_material"
)

# Remove rows with missing values and sort by depth
age_model_data_clean <- age_model_data[
  !is.na(age_model_data$Depth_sed_m) & 
    !is.na(age_model_data$Cal_age_ka_BP),
]
age_model_data_clean <- age_model_data_clean[order(age_model_data_clean$Depth_sed_m), ]

# Calculate sedimentation rate (Δdepth/Δage) for each interval
start_age <- age_model_data_clean$Cal_age_ka_BP[-nrow(age_model_data_clean)]  # Start of interval
end_age <- age_model_data_clean$Cal_age_ka_BP[-1]                             # End of interval
depth_diff <- diff(age_model_data_clean$Depth_sed_m)
age_diff <- diff(age_model_data_clean$Cal_age_ka_BP)
sed_rate <- depth_diff / age_diff  # m/ka (positive values)

# Create intervals for plotting (stepwise horizontal lines)
sed_intervals <- data.frame(
  start_age = start_age,
  end_age = end_age,
  sed_rate = sed_rate
)

# Set up two panels
par(mfrow = c(2, 1), mar = c(4, 4, 2, 4))

# --------------------------------------
# Panel 1: Depth vs. Age (Downcore Plot)
# --------------------------------------
plot(
  age_model_data_clean$Cal_age_ka_BP,
  age_model_data_clean$Depth_sed_m,
  xlab = "Calibrated Age (ka BP)",
  ylab = "Depth (m)",
  main = "Downcore Age Profile",
  pch = 19,
  col = "blue",
  type = "b",
  ylim = rev(range(age_model_data_clean$Depth_sed_m))  # Invert y-axis
)

# Add error bars for age uncertainty
arrows(
  x0 = age_model_data_clean$Cal_age_ka_BP - age_model_data_clean$Cal_age_std_dev,
  y0 = age_model_data_clean$Depth_sed_m,
  x1 = age_model_data_clean$Cal_age_ka_BP + age_model_data_clean$Cal_age_std_dev,
  y1 = age_model_data_clean$Depth_sed_m,
  angle = 90,
  code = 3,
  length = 0.05,
  col = "gray"
)

# --------------------------------------
# Panel 2: Stepwise Sedimentation Rate
# --------------------------------------
plot(
  NA,  # Empty plot
  xlim = range(age_model_data_clean$Cal_age_ka_BP),
  ylim = c(0, max(sed_rate) * 1.1),  # Ensure y-axis starts at 0
  xlab = "Calibrated Age (ka BP)",
  ylab = "Sedimentation Rate (m/ka)",
  main = "Stepwise Sedimentation Rate"
)

# Add horizontal segments for each interval
segments(
  x0 = sed_intervals$start_age,
  y0 = sed_intervals$sed_rate,
  x1 = sed_intervals$end_age,
  y1 = sed_intervals$sed_rate,
  col = "red",
  lwd = 2
)

# Add vertical lines to emphasize intervals
segments(
  x0 = sed_intervals$start_age,
  y0 = 0,
  y1 = sed_intervals$sed_rate,
  col = "red",
  lty = "dashed"
)

# Add gridlines
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# Reset plot layout
par(mfrow = c(1, 1))