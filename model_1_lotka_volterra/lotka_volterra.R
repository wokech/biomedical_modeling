# R Script: Modeling Predator-Prey Dynamics in National Parks

# The AI Chatbots from Qwen, DeepSeek, and Claude were used to help
# generate the code shown below.

# DeepSeek

# Load required libraries
library(deSolve)    # For solving differential equations
library(ggplot2)    # For plotting
library(ggthemes)   # For additional ggplot themes

# Define the Lotka-Volterra equations
lotka_volterra <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- alpha * x - beta * x * y   # Prey population change
    dy <- delta * x * y - gamma * y   # Predator population change
    return(list(c(dx, dy)))
  })
}

# Set parameters (adjust as needed)
parameters <- c(
  alpha = 1.1,    # Prey growth rate
  beta = 0.2,     # Predation rate
  delta = 0.1,    # Predator efficiency
  gamma = 0.4     # Predator death rate
)

# Initial populations (prey, predator)
state <- c(x = 10, y = 2)

# Time steps (years)
times <- seq(0, 50, by = 0.1)

# Solve the ODE system
out <- ode(y = state, times = times, func = lotka_volterra, parms = parameters)

# Convert to data frame for ggplot
df <- as.data.frame(out)

# Plot using ggplot2
ggplot(df, aes(x = time)) +
  geom_line(aes(y = x, color = "Prey (e.g., Deer)"), linewidth = 1) +
  geom_line(aes(y = y, color = "Predator (e.g., Wolves)"), linewidth = 1) +
  labs(
    title = "Predator-Prey Dynamics in a National Park",
    subtitle = "Lotka-Volterra Model",
    x = "Time (years)",
    y = "Population Size",
    color = "Species"
  ) +
  scale_color_manual(values = c("#1f77b4", "#d62728")) +  # Custom colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("model_1_lotka_volterra/lotka_volterra_1.png", height = 6, width = 8, dpi = 300)

# Claude

# Lotka-Volterra Predator-Prey Model with ggplot2 Visualization
library(deSolve)
library(ggplot2)
library(tidyr)
library(viridis)

# Set parameters for Lotka-Volterra model
parameters <- c(
  r = 0.8,    # Prey growth rate
  alpha = 0.04, # Predation rate
  beta = 0.02,  # Conversion efficiency
  m = 0.4     # Predator mortality rate
)

# Initial population sizes
initial_state <- c(
  N = 50,  # Initial prey population
  P = 20   # Initial predator population
)

# Time points to evaluate
times <- seq(0, 100, by = 0.1)

# Lotka-Volterra model equations
lotka_volterra <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Prey population change
    dN <- r * N - alpha * N * P
    
    # Predator population change
    dP <- beta * alpha * N * P - m * P
    
    return(list(c(dN, dP)))
  })
}

# Solve the differential equations
output <- ode(
  y = initial_state,
  times = times,
  func = lotka_volterra,
  parms = parameters
)

# Convert to data frame
results <- as.data.frame(output)
colnames(results) <- c("Time", "Prey", "Predators")

# Create long format for ggplot
results_long <- pivot_longer(
  results, 
  cols = c("Prey", "Predators"),
  names_to = "Population", 
  values_to = "Count"
)

# Create a nice theme for our plots
nice_theme <- theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Plot 1: Population over time
time_plot <- ggplot(results_long, aes(x = Time, y = Count, color = Population)) +
  geom_line(size = 1.2) +
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  labs(
    title = "Predator-Prey Population Dynamics",
    subtitle = "Lotka-Volterra Model Simulation",
    x = "Time",
    y = "Population Size",
    color = "Species"
  ) +
  nice_theme

# Plot 2: Phase plane plot
phase_plot <- ggplot(results, aes(x = Prey, y = Predators)) +
  geom_path(color = "#3366FF", size = 1) +
  geom_point(data = results[1,], aes(x = Prey, y = Predators), 
             color = "#FF6633", size = 3) +
  annotate("text", x = results[1,"Prey"] + 2, y = results[1,"Predators"] + 2, 
           label = "Start", color = "#FF6633") +
  labs(
    title = "Predator-Prey Phase Plane",
    subtitle = "Cyclical Relationship Between Populations",
    x = "Prey Population",
    y = "Predator Population"
  ) +
  nice_theme

# Print plots
print(time_plot)
print(phase_plot)

# Create a function to run simulations with different parameters
simulate_scenarios <- function(param_list, names, duration = 100) {
  scenario_results <- data.frame()
  
  for (i in 1:length(param_list)) {
    output <- ode(
      y = initial_state,
      times = seq(0, duration, by = 0.1),
      func = lotka_volterra,
      parms = param_list[[i]]
    )
    
    results <- as.data.frame(output)
    colnames(results) <- c("Time", "Prey", "Predators")
    results$Scenario <- names[i]
    
    scenario_results <- rbind(scenario_results, results)
  }
  
  return(scenario_results)
}

# Define different ecological scenarios
scenario_params <- list(
  c(r = 0.8, alpha = 0.04, beta = 0.02, m = 0.4),  # Baseline
  c(r = 0.8, alpha = 0.08, beta = 0.02, m = 0.4),  # More efficient predators
  c(r = 0.5, alpha = 0.04, beta = 0.02, m = 0.4)   # Slower prey growth
)

scenario_names <- c("Baseline", "Efficient Predators", "Slow Prey Growth")

# Run scenarios
scenario_results <- simulate_scenarios(scenario_params, scenario_names)

# Convert to long format
scenario_long <- pivot_longer(
  scenario_results,
  cols = c("Prey", "Predators"),
  names_to = "Population",
  values_to = "Count"
)

# Plot comparison of scenarios
scenario_plot <- ggplot(scenario_long, aes(x = Time, y = Count, color = Population, linetype = Scenario)) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  facet_wrap(~Scenario) +
  labs(
    title = "Comparison of Different Ecological Scenarios",
    subtitle = "How parameter changes affect predator-prey dynamics",
    x = "Time",
    y = "Population Size",
    color = "Species",
    linetype = "Scenario"
  ) +
  nice_theme +
  theme(legend.position = "bottom")

# Print comparison plot
print(scenario_plot)

ggsave("model_1_lotka_volterra/lotka_volterra_2.png", height = 6, width = 8, dpi = 300)

# Qwen

# Step 1: Simulate the Predator-Prey Dynamics

# Load necessary libraries
library(deSolve)
library(ggplot2)
library(gganimate)  # For optional animation [[5]]

# Define the Lotka-Volterra model
lotka_volterra <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N - a * N * P  # Prey growth and predation
    dP <- b * a * N * P - m * P  # Predator growth and mortality
    return(list(c(dN, dP)))
  })
}

# Set initial conditions and parameters
state <- c(N = 50, P = 20)  # Initial prey (N) and predator (P) populations
parameters <- c(r = 0.5, a = 0.02, b = 0.6, m = 0.3)  # Model parameters

# Define the time vector
times <- seq(0, 100, by = 0.1)  # Time from 0 to 100 with step size 0.1

# Solve the differential equations
results <- ode(y = state, times = times, func = lotka_volterra, parms = parameters)

# Convert results to a data frame
results_df <- as.data.frame(results)

# Step 2: Visualize the Results with ggplot2

# Create a ggplot
p <- ggplot(results_df, aes(x = time)) +
  geom_line(aes(y = N, color = "Prey"), size = 1) +  # Line for prey population
  geom_line(aes(y = P, color = "Predator"), size = 1) +  # Line for predator population
  scale_color_manual(values = c("Prey" = "#1f78b4", "Predator" = "#e31a1c")) +  # Custom colors
  labs(title = "Lotka-Volterra Predator-Prey Dynamics",
       x = "Time",
       y = "Population Size",
       color = "Species") +
  theme_minimal() +  # A clean and minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

# Display the plot
print(p)

# Optional: Add Animation with gganimate

# Animate the plot
animated_plot <- p +
  transition_reveal(time) +  # Reveal data over time
  ease_aes('linear')  # Smooth animation

# Render the animation
lotka_volterra_gif <- animate(animated_plot, fps = 20, width = 800, height = 600)

# Save the gif

anim_save(filename="model_1_lotka_volterra/lotka_volterra.gif", lotka_volterra_gif)

