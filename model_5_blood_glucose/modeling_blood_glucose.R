# R Script: Modeling Blood Glucose Levels After a Meal
# This script generates synthetic data, creates a glucose response model,
# and produces a visualization of blood glucose levels after meal consumption

# # Used Claude 3.7 to generate the synthetic data, model, and code.

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic blood glucose data
# Time points (in minutes) - tracking for 3 hours after meal
time <- seq(0, 180, by = 1)

# Function to generate glucose response curve with individual parameters
generate_glucose_data <- function(
    baseline = 90,         # Baseline glucose level (mg/dL)
    peak_increase = 60,    # Maximum increase over baseline (mg/dL)
    time_to_peak = 45,     # Time to reach peak (minutes)
    decay_rate = 0.025,    # Rate of return to baseline
    noise_sd = 3           # Standard deviation of random noise
) {
  
  # Model components
  # 1. Baseline glucose level
  # 2. Response curve: increase to peak followed by exponential decay
  
  glucose <- baseline + 
    # Rising phase (logistic growth to peak)
    ifelse(time <= time_to_peak, 
           peak_increase * (time / time_to_peak)^2 / (1 + (time / time_to_peak)^2),
           # Falling phase (exponential decay back to baseline)
           peak_increase * exp(-decay_rate * (time - time_to_peak)))
  
  # Add realistic biological variability
  noise <- rnorm(length(time), mean = 0, sd = noise_sd)
  glucose <- glucose + noise
  
  return(glucose)
}

# Generate data for different meal types
# 1. High glycemic meal (fast spike, fast drop)
# 2. Mixed meal (moderate rise and fall)
# 3. Low glycemic meal (gentle rise, slow fall)

glucose_high_gi <- generate_glucose_data(
  baseline = 85, 
  peak_increase = 85, 
  time_to_peak = 35, 
  decay_rate = 0.03
)

glucose_mixed <- generate_glucose_data(
  baseline = 90, 
  peak_increase = 65, 
  time_to_peak = 50, 
  decay_rate = 0.025
)

glucose_low_gi <- generate_glucose_data(
  baseline = 88, 
  peak_increase = 45, 
  time_to_peak = 60, 
  decay_rate = 0.018
)

# Combine data into a dataframe
glucose_data <- data.frame(
  time = rep(time, 3),
  glucose_level = c(glucose_high_gi, glucose_mixed, glucose_low_gi),
  meal_type = factor(rep(c("High Glycemic", "Mixed Meal", "Low Glycemic"), each = length(time)),
                     levels = c("High Glycemic", "Mixed Meal", "Low Glycemic"))
)

# Create smooth model fit for visualizing the trend
glucose_model <- glucose_data %>%
  group_by(meal_type) %>%
  mutate(smooth_glucose = stats::loess(glucose_level ~ time, span = 0.2)$fitted)

# Define glucose range zones for visualization
glucose_zones <- data.frame(
  start = c(70, 100, 140),
  end = c(100, 140, 200),
  zone = c("Normal Fasting", "Elevated", "High")
)

# Create main visualization
p1 <- ggplot(glucose_model, aes(x = time, y = glucose_level, color = meal_type)) +
  # Add glucose range zones
  geom_rect(data = glucose_zones, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = start, ymax = end, fill = zone),
            alpha = 0.2) +
  # Add raw data points with transparency
  geom_point(alpha = 0.1, size = 0.8) +
  # Add smoothed model lines
  geom_line(aes(y = smooth_glucose), size = 1.2) +
  # Event marker - meal consumption
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray") +
  annotate("text", x = 5, y = 200, label = "Meal", hjust = 0, size = 3.5, color = "darkgray") +
  # Labels and titles
  labs(
    title = "Blood Glucose Response After Meal Consumption",
    subtitle = "Comparing responses to different meal types",
    x = "Time After Meal (minutes)",
    y = "Blood Glucose Level (mg/dL)",
    color = "Meal Type",
    fill = "Glucose Zone"
  ) +
  # Set axis limits
  scale_y_continuous(limits = c(70, 200), breaks = seq(70, 200, by = 20)) +
  scale_x_continuous(breaks = seq(0, 180, by = 30)) +
  # Color schemes
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  # Theming
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p1

ggsave("images/modeling_blood_glucose.png", height = 8, width = 12, dpi = 300)

# Create a secondary plot showing area under the curve (glucose exposure)
auc_data <- glucose_model %>%
  group_by(meal_type) %>%
  summarize(total_glucose = sum(smooth_glucose - min(glucose_zones$start)))

p2 <- ggplot(auc_data, aes(x = meal_type, y = total_glucose, fill = meal_type)) +
  geom_col(width = 0.7) +
  labs(
    title = "Total Glucose Exposure",
    subtitle = "Area under the curve relative to baseline",
    x = "",
    y = "Relative Glucose Exposure",
    fill = "Meal Type"
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D", end = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "darkgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )



# Combine plots using patchwork
combined_plot <- p1 + p2 + plot_layout(heights = c(3, 1))

# Display the combined plot
print(combined_plot)

# Calculate and display summary statistics
glucose_stats <- glucose_data %>%
  group_by(meal_type) %>%
  summarize(
    baseline = first(glucose_level),
    peak = max(glucose_level),
    time_to_peak = time[which.max(glucose_level)],
    glucose_increase = peak - baseline,
    recovery_time = max(time[glucose_level > (baseline + 10)])
  )

print(glucose_stats)

# Optional: Save the plot
# ggsave("glucose_response_model.png", plot = combined_plot, width = 10, height = 8, dpi = 300)