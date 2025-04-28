# R Script: Multiscale Model of Bacterial Growth in a Bioreactor

# The AI Chatbots from Google AI Studio, Gemini, and ChatGPT were used to help
# generate the code shown below.

# ChatGPT

# Step 1: Install and Load Required Packages

install.packages(c("ipolygrowth", "growthrates", "dplyr", "ggplot2"))
library(ipolygrowth)
library(growthrates)
library(dplyr)
library(ggplot2)

# Step 2: Load and Prepare the Data
 
# Load the dataset
df <- growthrates::bactgrowth

# Create a unique identifier for each strain-concentration combination
df <- df %>%
  mutate(sample_id = paste(strain, conc, sep = "_"))

# Step 3: Fit Multiscale Growth Models

# Fit polynomial models to each sample
fit_results <- ipg_multisample(
  data = df,
  id = "sample_id",
  time.name = "time",
  y.name = "value",
  epsilon = 0.002  # Adjusted for smoother fits
)

# Step 4: Visualize the Growth Curves

# Plot observed data and fitted curves
ggplot() +
  geom_point(data = df, aes(x = time, y = value, color = factor(replicate)), alpha = 0.6) +
  geom_line(data = fit_results$fitted, aes(x = time, y = fit), color = "black", size = 0.8) +
  facet_wrap(~ sample_id, scales = "free_y") +
  labs(
    title = "Multiscale Bacterial Growth in Bioreactor",
    x = "Time (hours)",
    y = "Optical Density (OD)",
    color = "Replicate"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Step 5: Extract Growth Parameters

# View estimated growth parameters
head(fit_results$estimates)

ggsave("model_2_bacterial_growth/bacterial_growth_1.png", width = 12, height = 8, dpi = 300)

# Gemini

# Load required packages
library(deSolve)
library(ggplot2)
library(tidyr) # For data transformation (pivot_longer)

# --- 1. Define the ODE system function ---
# The function takes time (t), state variables (y), and parameters (parms)
bioreactor_ode <- function(t, y, parms) {
  # State variables: y[1] = Biomass (X), y[2] = Substrate (S)
  X <- y[1]
  S <- y[2]
  
  # Parameters
  mu_max <- parms["mu_max"]
  KS     <- parms["KS"]
  YX_S   <- parms["YX_S"]
  Sin    <- parms["Sin"]
  D      <- parms["D"] # Dilution rate
  
  # Ensure concentrations are non-negative (numerical stability)
  if (X < 0) X <- 0
  if (S < 0) S <- 0
  
  # Population Scale (Mesoscale): Specific Growth Rate (Monod)
  # Only grow if substrate is available
  mu <- mu_max * (S / (KS + S))
  if (S < 1e-6) mu <- 0 # Prevent division by zero if S is effectively zero
  
  # Reactor Scale (Macroscale): ODEs based on Mass Balance
  # dX/dt = mu * X - D * X        (Growth - Outflow)
  # dS/dt = D * (Sin - S) - (1/YX_S) * mu * X (Inflow - Outflow - Consumption)
  dXdt <- mu * X - D * X
  dSdt <- D * (Sin - S) - (1/YX_S) * mu * X
  
  # Return the derivatives as a list
  list(c(dXdt, dSdt))
}

# --- 2. Define parameters and initial conditions ---
parms <- c(
  mu_max = 0.4, # Maximum specific growth rate (h^-1)
  KS     = 0.1, # Monod constant (g/L)
  YX_S   = 0.5, # Yield coefficient (g Biomass / g Substrate)
  Sin    = 10.0, # Influent substrate concentration (g/L)
  D      = 0.2   # Dilution rate (h^-1). Choose D < mu_max for growth
  # D      = 0.5   # Example: Choose D > mu_max for washout
)

y_init <- c(
  X = 0.01, # Initial biomass concentration (g/L) - start with a small inoculum
  S = parms["Sin"] # Initial substrate concentration (g/L) - initially feed is coming in
)

# --- 3. Define time points for the simulation ---
# Simulate for 50 hours, outputting every 0.1 hours
times <- seq(0, 50, by = 0.1)

# --- 4. Solve the ODE system ---
output <- ode(y = y_init, times = times, func = bioreactor_ode, parms = parms)

# The output is a matrix. Convert to a data frame for easier plotting.
output_df <- as.data.frame(output)
colnames(output_df) <- c("time", "Biomass", "Substrate") # Rename columns

# --- 5. Prepare data for ggplot2 (long format) ---
output_long <- output_df %>%
  pivot_longer(
    cols = c("Biomass", "Substrate"), # Columns to pivot
    names_to = "Variable",           # New column for variable name
    values_to = "Concentration"      # New column for concentration value
  )

# --- 6. Generate the plot using ggplot2 ---
p <- ggplot(output_long, aes(x = time, y = Concentration, color = Variable)) +
  geom_line(size = 1.2) + # Add lines
  labs(
    title = "Multiscale Model: Bacterial Growth in a CSTR",
    subtitle = paste0("Parameters: mu_max=", parms['mu_max'], ", KS=", parms['KS'],
                      ", YX_S=", parms['YX_S'], ", Sin=", parms['Sin'], ", D=", parms['D']),
    x = "Time (hours)",
    y = "Concentration (g/L)",
    color = "Species" # Legend title
  ) +
  scale_color_manual(values = c("Biomass" = "darkgreen", "Substrate" = "darkorange")) + # Custom colors
  theme_minimal(base_size = 14) + # Minimal theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
    plot.subtitle = element_text(hjust = 0.5), # Center subtitle
    legend.position = "bottom", # Place legend at the bottom
    panel.grid.major = element_line(color = "grey90", size = 0.5), # Lighter grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# Print the plot
print(p)

ggsave("model_2_bacterial_growth/bacterial_growth_2.png", width = 9, height = 6, dpi = 300)

# --- Optional: Check steady-state values (if D < mu_max) ---
# The model should approach a steady state if D < mu_max
# S_steady = KS * D / (mu_max - D)
# X_steady = YX_S * (Sin - S_steady)
# Check if mu_max > D
if (parms["mu_max"] > parms["D"]) {
  S_steady_expected = parms["KS"] * parms["D"] / (parms["mu_max"] - parms["D"])
  X_steady_expected = parms["YX_S"] * (parms["Sin"] - S_steady_expected)
  cat("\nExpected Steady State (since mu_max > D):\n")
  cat("  Biomass (X):", round(X_steady_expected, 4), "g/L\n")
  cat("  Substrate (S):", round(S_steady_expected, 4), "g/L\n")
  cat("Simulation End Values:\n")
  cat("  Biomass (X):", round(tail(output_df$Biomass, 1), 4), "g/L\n")
  cat("  Substrate (S):", round(tail(output_df$Substrate, 1), 4), "g/L\n")
} else {
  cat("\nExpected Washout (since D >= mu_max).\n")
  cat("  Biomass (X): approx 0 g/L\n")
  cat("  Substrate (S): approx Sin (", parms["Sin"], ") g/L\n")
  cat("Simulation End Values:\n")
  cat("  Biomass (X):", round(tail(output_df$Biomass, 1), 4), "g/L\n")
  cat("  Substrate (S):", round(tail(output_df$Substrate, 1), 4), "g/L\n")
}

# Google AI Studio

# Install necessary packages if you don't have them
# install.packages("deSolve")
# install.packages("ggplot2")
# install.packages("tidyr") # For data reshaping
# install.packages("dplyr") # For data manipulation

# Load packages
library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Define the ODE system function
bioreactor_model <- function(time, state, params) {
  # Unpack state variables
  X <- state["X"] # Biomass concentration (e.g., g/L)
  S <- state["S"] # Substrate concentration (e.g., g/L)
  P <- state["P"] # Product concentration (e.g., g/L)
  
  # Unpack parameters
  mu_max <- params["mu_max"]
  K_s <- params["K_s"]
  Y_xs <- params["Y_xs"]
  K_i <- params["K_i"]
  Y_px <- params["Y_px"]
  k_d <- params["k_d"]
  
  # Calculate specific growth rate (mu) with substrate limitation and product inhibition
  # Add small epsilon to denominators to avoid division by zero if S or K_i+P is exactly zero
  epsilon <- 1e-10
  mu <- mu_max * (max(0, S) / (K_s + max(0, S) + epsilon)) * (K_i / (K_i + max(0, P) + epsilon))
  
  # Calculate the rates of change (ODEs)
  dX_dt <- mu * X - k_d * X
  dS_dt <- - (mu * X) / Y_xs
  dP_dt <- Y_px * mu * X
  
  # Return the rates of change as a list
  return(list(c(dX_dt, dS_dt, dP_dt)))
}

# 2. Define Parameters (Microscale Representatives)
params <- c(
  mu_max = 0.8,   # Max specific growth rate (1/h) - Microscale: Max metabolic rate
  K_s = 0.1,    # Half-saturation constant (g/L) - Microscale: Substrate transporter affinity
  Y_xs = 0.5,   # Biomass yield (g biomass / g substrate) - Microscale: Metabolic efficiency
  K_i = 5.0,    # Product inhibition constant (g/L) - Microscale: Enzyme sensitivity to product
  Y_px = 0.8,   # Product yield (g product / g biomass formed via growth) - Microscale: Pathway yield
  k_d = 0.02    # Decay/maintenance coefficient (1/h) - Microscale: Cell death/endogenous metabolism rate
)

# 3. Define Initial Conditions (Macroscale State)
initial_state <- c(
  X = 0.1,  # Initial biomass concentration (g/L)
  S = 10.0, # Initial substrate concentration (g/L)
  P = 0.0   # Initial product concentration (g/L)
)

# 4. Define Time Span for Simulation (Macroscale)
times <- seq(from = 0, to = 25, by = 0.1) # Simulate for 25 hours

# 5. Solve the ODE system
simulation_out <- ode(
  y = initial_state,
  times = times,
  func = bioreactor_model,
  parms = params
)

# 6. Convert output to a data frame for ggplot
sim_df <- as.data.frame(simulation_out)

# Add meaningful names (although ode() usually does this well)
colnames(sim_df) <- c("Time", "Biomass", "Substrate", "Product")

# 7. Visualize the results using ggplot2

# Reshape data to long format for easier plotting with ggplot
sim_df_long <- sim_df %>%
  pivot_longer(cols = c("Biomass", "Substrate", "Product"),
               names_to = "Variable",
               values_to = "Concentration") %>%
  mutate(Variable = factor(Variable, levels = c("Biomass", "Substrate", "Product"))) # Control plotting order

# Create the plot
growth_plot <- ggplot(sim_df_long, aes(x = Time, y = Concentration, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1.2) + # Use linewidth instead of size for ggplot2 v3.4.0+
  # Aesthetically pleasing color scales
  scale_color_brewer(palette = "Set1", name = "Component") +
  #scale_color_viridis_d(name = "Component") + # Alternative color scale
  scale_linetype_manual(values = c("Biomass" = "solid", "Substrate" = "dashed", "Product" = "dotted"), name = "Component") +
  labs(
    title = "Simulated Bacterial Growth in a Bioreactor",
    subtitle = "Macroscale dynamics based on Monod kinetics with product inhibition",
    x = "Time (hours)",
    y = "Concentration (g/L)",
    caption = "Parameters conceptually linked to microscale cellular properties"
  ) +
  theme_minimal(base_size = 14) + # Clean theme with larger base font size
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(face = "italic", size = 9),
    legend.position = "bottom", # Or "right", "top", "left"
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank() # Cleaner look
  ) +
  ylim(0, NA) # Ensure y-axis starts at 0

# Print the plot
print(growth_plot)

ggsave("model_2_bacterial_growth/bacterial_growth_3.png", width = 9, height = 6, dpi = 300)

# Optional: Save the plot
# ggsave("bioreactor_growth_plot.png", plot = growth_plot, width = 8, height = 6, dpi = 300)