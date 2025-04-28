# R Script: Modeling Heart Rate During Exercise

# Used Claude 3.7 to generate the synthetic data, model, and code.

# This script generates synthetic data, creates a heart rate model,
# and produces a visualization of heart rate response during exercise

# Load required libraries
library(ggplot2)
library(dplyr)
library(splines)
library(tidyr)
library(viridis)

# Set seed for reproducibility
set.seed(42)

# Generate synthetic exercise session data
# Exercise session: warm-up (5 min), increasing intensity (15 min), 
# peak exercise (10 min), cool down (10 min)

# Time points (in minutes)
time <- seq(0, 40, by = 0.1)

# Define heart rate model with random elements
# Using a base resting heart rate + exercise response + recovery pattern
generate_hr_data <- function(resting_hr = 70, fitness_level = "medium") {
  
  # Set parameters based on fitness level
  max_increase <- switch(fitness_level,
                         "high" = 100,
                         "medium" = 110,
                         "low" = 125)
  
  recovery_rate <- switch(fitness_level,
                          "high" = 0.20,  # faster recovery
                          "medium" = 0.15,
                          "low" = 0.10)   # slower recovery
  
  # Base heart rate model
  hr <- resting_hr + 
    # Warm-up (0-5 min): gradual increase
    ifelse(time <= 5, time * 8, 0) +
    # Main exercise (5-20 min): increasing intensity
    ifelse(time > 5 & time <= 20, 40 + (time - 5) * 4, 0) +
    # Peak exercise (20-30 min): maximum effort
    ifelse(time > 20 & time <= 30, max_increase, 0) +
    # Cool down (30-40 min): gradual decrease
    ifelse(time > 30, max_increase * exp(-recovery_rate * (time - 30)), 0)
  
  # Add realistic variability
  noise <- rnorm(length(time), mean = 0, sd = 2)
  hr <- hr + noise
  
  return(hr)
}

# Generate data for three different fitness levels
hr_low <- generate_hr_data(resting_hr = 80, fitness_level = "low")
hr_medium <- generate_hr_data(resting_hr = 70, fitness_level = "medium")
hr_high <- generate_hr_data(resting_hr = 60, fitness_level = "high")

# Combine data into a dataframe
exercise_data <- data.frame(
  time = rep(time, 3),
  heart_rate = c(hr_low, hr_medium, hr_high),
  fitness_level = factor(rep(c("Low", "Medium", "High"), each = length(time)),
                         levels = c("Low", "Medium", "High"))
)

# Define exercise phases for visualization
exercise_phases <- data.frame(
  start = c(0, 5, 20, 30),
  end = c(5, 20, 30, 40),
  phase = c("Warm-up", "Increasing Intensity", "Peak Exercise", "Cool Down")
)

# Create a spline smoothed version of the data for model
exercise_model <- exercise_data %>%
  group_by(fitness_level) %>%
  mutate(smooth_hr = predict(smooth.spline(time, heart_rate, spar = 0.6), time)$y)

# Create visualization
p <- ggplot(exercise_model, aes(x = time, y = heart_rate, color = fitness_level)) +
  # Add colored rectangles for exercise phases
  geom_rect(data = exercise_phases, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2) +
  # Add raw data points with transparency
  geom_point(alpha = 0.25, size = 0.8) +
  # Add smoothed model line
  geom_line(aes(y = smooth_hr), linewidth = 1.2) +
  # Labels and titles
  labs(
    title = "Heart Rate Response During Exercise",
    subtitle = "Comparing different fitness levels",
    x = "Time (minutes)",
    y = "Heart Rate (bpm)",
    color = "Fitness Level",
    fill = "Exercise Phase"
  ) +
  # Color schemes
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  # Add heart rate zones
  geom_hline(yintercept = c(100, 130, 160), linetype = "dotted", color = "darkgray") +
  annotate("text", x = 1, y = c(100, 130, 160) + 5, 
           label = c("Light Zone", "Moderate Zone", "Intense Zone"), 
           hjust = 0, size = 3, color = "black") +
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

# Display the plot
print(p)

ggsave("images/modeling_heart_rate_1.png", height = 8, width = 12, dpi = 300)

# Calculate and display summary statistics
hr_stats <- exercise_data %>%
  group_by(fitness_level) %>%
  summarize(
    avg_hr = mean(heart_rate),
    max_hr = max(heart_rate),
    min_hr = min(heart_rate),
    hr_range = max_hr - min_hr
  )

print(hr_stats)

# Optional: Save the plot
# ggsave("heart_rate_model.png", plot = p, width = 10, height = 7, dpi = 300)