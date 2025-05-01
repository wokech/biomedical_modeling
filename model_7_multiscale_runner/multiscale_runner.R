# R Script: Multiscale Runner Performance Model

# AI Chatbots from various companies were used to help
# generate the code shown below.

# 1. Google AI Studio

# --- 0. Load Libraries ---
library(ggplot2)
library(dplyr)
library(tidyr) # For crossing

# --- 1. Define Runner Parameters (Input Factors) ---

# Physiology
vo2max_ml_kg_min <- 60    # VO2max in ml/kg/min (Good amateur male)
lt1_perc_vo2max  <- 65    # LT1 as % of VO2max (Aerobic Threshold)
lt2_perc_vo2max  <- 85    # LT2 as % of VO2max (Anaerobic Threshold / MLSS approximation)
base_re_ml_kg_km <- 210   # Base Running Economy in ml/kg/km (Average)

# Biomechanics (Micro -> Meso influence on RE)
# Factor < 1.0 implies better than average efficiency
# Factor > 1.0 implies worse than average efficiency
biomechanics_factor <- 0.98 # Slightly efficient biomechanics

# Equipment (Micro -> Meso influence on RE)
# Factor < 1.0 implies efficient equipment (e.g., super shoes)
# Factor > 1.0 implies less efficient equipment (e.g., heavy trainers)
equipment_factor <- 0.97 # Efficient shoes

# Nutrition (Micro -> Macro influence on performance sustainability)
# Factor > 1.0 implies optimal fueling for the effort
# Factor < 1.0 implies suboptimal fueling
nutrition_factor <- 1.01 # Well-fueled

# --- 2. Define Training Zones based on %VO2max ---
# Common 5-zone model (approximations)
zone_defs <- list(
  "Zone 1 (Recovery)" = c(0, 55),
  "Zone 2 (Aerobic)"  = c(55, 75), # Often overlaps LT1
  "Zone 3 (Tempo)"    = c(75, 88), # Often spans LT1 to LT2
  "Zone 4 (Threshold)"= c(88, 95), # Around or just above LT2
  "Zone 5 (VO2max)"   = c(95, 110) # Approaching and at VO2max (includes anaerobic)
)

# --- 3. Model Calculations ---

# Create a sequence of effort levels
effort_perc_vo2max <- seq(40, 105, by = 1) # From 40% to 105% VO2max

# Calculate Adjusted Running Economy
adjusted_re <- base_re_ml_kg_km * biomechanics_factor * equipment_factor

# Calculate performance across effort levels
performance_data <- data.frame(Effort_perc_VO2max = effort_perc_vo2max) %>%
  mutate(
    VO2_ml_kg_min = vo2max_ml_kg_min * (Effort_perc_VO2max / 100),
    # Theoretical aerobic velocity based on O2 consumption and economy
    Velocity_km_min = VO2_ml_kg_min / adjusted_re,
    # Convert to m/s for potential other uses (Velocity_m_s = Velocity_km_min * 1000 / 60)
    Velocity_m_s = Velocity_km_min * 1000 / 60,
    # Calculate Pace in min/km
    Pace_min_km_raw = 1 / Velocity_km_min,
    # Apply Nutrition Factor (simplistic application)
    Pace_min_km = Pace_min_km_raw / nutrition_factor,
    # Assign Training Zone
    Zone = cut(Effort_perc_VO2max,
               breaks = c(zone_defs[[1]][1], zone_defs[[1]][2], zone_defs[[2]][2], zone_defs[[3]][2], zone_defs[[4]][2], zone_defs[[5]][2]),
               labels = names(zone_defs),
               right = FALSE, # Intervals are [min, max)
               include.lowest = TRUE)
  )

# --- 4. Calculate Key Threshold Points ---

# Function to find the pace for a given %VO2max
get_pace_at_perc_vo2max <- function(perc) {
  vo2_at_perc <- vo2max_ml_kg_min * (perc / 100)
  vel_km_min_at_perc <- vo2_at_perc / adjusted_re
  pace_raw <- 1 / vel_km_min_at_perc
  pace_final <- pace_raw / nutrition_factor
  return(pace_final)
}

lt1_pace <- get_pace_at_perc_vo2max(lt1_perc_vo2max)
lt2_pace <- get_pace_at_perc_vo2max(lt2_perc_vo2max)
vo2max_pace <- get_pace_at_perc_vo2max(100) # Theoretical pace at 100% VO2max

threshold_points <- data.frame(
  Threshold = factor(c("LT1", "LT2", "VO2max"), levels = c("LT1", "LT2", "VO2max")),
  Effort_perc_VO2max = c(lt1_perc_vo2max, lt2_perc_vo2max, 100),
  Pace_min_km = c(lt1_pace, lt2_pace, vo2max_pace)
)

# --- 5. Create Zone Rectangles for Plotting ---
zone_rects <- data.frame(
  Zone = factor(names(zone_defs), levels = names(zone_defs)),
  xmin = sapply(zone_defs, function(x) x[1]),
  xmax = sapply(zone_defs, function(x) x[2]),
  ymin = -Inf,
  ymax = Inf
)
# Adjust xmax slightly for better visual separation if needed
zone_rects$xmax[1:4] <- zone_rects$xmax[1:4] - 0.1

# Define nice colors for zones
zone_colors <- c(
  "Zone 1 (Recovery)" = "#add8e6",  # Light Blue
  "Zone 2 (Aerobic)"  = "#90ee90",  # Light Green
  "Zone 3 (Tempo)"    = "#ffffb3",  # Light Yellow
  "Zone 4 (Threshold)"= "#ffcc80",  # Light Orange
  "Zone 5 (VO2max)"   = "#ff9999"   # Light Red
)

# --- 6. Generate the Plot with ggplot2 ---

plot_title <- "Multiscale Runner Model: Pace vs. Effort (%VO2max)"
plot_subtitle <- paste0(
  "VO2max: ", vo2max_ml_kg_min, " ml/kg/min | ",
  "Adj. RE: ", round(adjusted_re, 1), " ml/kg/km | ",
  "LT1: ", lt1_perc_vo2max, "% | ",
  "LT2: ", lt2_perc_vo2max, "%"
)
plot_caption <- paste0(
  "Model incorporates physiology (VO2max, LTs), running economy (adjusted by biomechanics factor: ",
  biomechanics_factor, ", equipment factor: ", equipment_factor,
  "), and nutrition factor: ", nutrition_factor, ".\nPace above LT2 is typically sustainable for limited durations only."
)

# Function to format pace (min:sec)
format_pace <- function(pace_decimal) {
  minutes <- floor(pace_decimal)
  seconds <- round((pace_decimal - minutes) * 60)
  sprintf("%d:%02d", minutes, seconds)
}

# Generate pace labels for y-axis
y_breaks <- pretty(performance_data$Pace_min_km, n = 8)
y_labels <- format_pace(y_breaks)


p <- ggplot(performance_data, aes(x = Effort_perc_VO2max, y = Pace_min_km)) +
  # Zone Rectangles (behind other layers)
  geom_rect(data = zone_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Zone),
            inherit.aes = FALSE, alpha = 0.3) +
  # Main Pace Curve
  geom_line(aes(color = "Modeled Pace"), linewidth = 1.2) +
  # Threshold Vertical Lines
  geom_vline(xintercept = lt1_perc_vo2max, linetype = "dashed", color = "darkblue", linewidth = 0.8) +
  geom_vline(xintercept = lt2_perc_vo2max, linetype = "dashed", color = "darkred", linewidth = 0.8) +
  # Threshold Horizontal Lines
  geom_hline(yintercept = lt1_pace, linetype = "dotted", color = "darkblue", linewidth = 0.8) +
  geom_hline(yintercept = lt2_pace, linetype = "dotted", color = "darkred", linewidth = 0.8) +
  # Points for Thresholds
  geom_point(data = threshold_points, aes(color = Threshold), size = 4, shape = 18) +
  # Threshold Text Labels (slightly offset)
  annotate("text", x = lt1_perc_vo2max + 1, y = max(performance_data$Pace_min_km) * 0.98, # Adjust y position
           label = paste("LT1\n", format_pace(lt1_pace), "/km"), hjust = 0, vjust = 1, size = 3.5, color = "darkblue") +
  annotate("text", x = lt2_perc_vo2max + 1, y = max(performance_data$Pace_min_km) * 0.98, # Adjust y position
           label = paste("LT2\n", format_pace(lt2_pace), "/km"), hjust = 0, vjust = 1, size = 3.5, color = "darkred") +
  annotate("text", x = 100 + 1, y = max(performance_data$Pace_min_km) * 0.98, # Adjust y position
           label = paste("VO2max\n", format_pace(vo2max_pace), "/km"), hjust = 0, vjust = 1, size = 3.5, color = "black") +
  # --- Scales and Labels ---
  scale_y_reverse(name = "Sustainable Pace (min:sec / km)", breaks = y_breaks, labels = y_labels) + # Reverse Y-axis for Pace
  scale_x_continuous(name = "Effort (% VO2max)", breaks = seq(40, 110, 10), limits = c(40, 105)) +
  scale_fill_manual(values = zone_colors, name = "Training Zone") +
  scale_color_manual(name = "Key Points",
                     values = c("Modeled Pace" = "black", "LT1" = "blue", "LT2" = "red", "VO2max" = "purple"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "blank", "blank", "blank"),
                                                              shape = c(NA, 18, 18, 18)))) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  # --- Theme ---
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic"),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey90")
  ) +
  guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))

# --- 7. Print the Plot ---
print(p)

ggsave("model_7_multiscale_runner/runner_1.png", width = 12, height = 8, dpi = 300)

# --- 8. Display Key Values (Optional) ---
cat("\n--- Model Parameters & Key Results ---\n")
cat("VO2max:", vo2max_ml_kg_min, "ml/kg/min\n")
cat("Base RE:", base_re_ml_kg_km, "ml/kg/km\n")
cat("Biomechanics Factor:", biomechanics_factor, "\n")
cat("Equipment Factor:", equipment_factor, "\n")
cat("Nutrition Factor:", nutrition_factor, "\n")
cat("Adjusted RE:", round(adjusted_re, 2), "ml/kg/km\n")
cat("\nThresholds:\n")
cat("LT1:", lt1_perc_vo2max, "% VO2max -> Pace:", format_pace(lt1_pace), "/km\n")
cat("LT2:", lt2_perc_vo2max, "% VO2max -> Pace:", format_pace(lt2_pace), "/km\n")
cat("VO2max (100%): Pace:", format_pace(vo2max_pace), "/km\n")



# 2. Gemini

# Install necessary packages if you don't have them
# install.packages("tidyverse") # Includes ggplot2 and dplyr

library(tidyverse)

# --- Model Parameters (Representing Runner's State) ---

# Base parameters for a hypothetical runner (Scenario 1)
runner_params_1 <- list(
  VO2max = 50, # ml/min/kg (Higher = better aerobic capacity) - Used conceptually, less directly in simple model equations
  Lactate_Threshold_Pace_min_km = 5.5, # min/km (Sustainable pace before rapid fatigue)
  Running_Economy_kJ_per_km_kg = 4, # kJ/km/kg (Lower = more efficient) - Influenced by Biomechanics, Equipment
  Initial_Energy_kJ_per_kg = 25, # kJ/kg (Starting glycogen stores) - Influenced by Nutrition
  Fatigue_Resistance = 1.0, # Unitless factor (Higher = resists fatigue better) - Influenced by Training Zones
  Energy_Storage_Capacity_kJ_per_kg = 30 # kJ/kg (Max energy storage) - Influenced by Training, Genetics
)

# Parameters for a potentially improved runner (Scenario 2)
# Example: Better Nutrition (higher initial energy), Better Equipment (better economy), Better Training (higher LT pace, better resistance)
runner_params_2 <- list(
  VO2max = 55, # Improved VO2max
  Lactate_Threshold_Pace_min_km = 5.0, # Faster LT Pace
  Running_Economy_kJ_per_km_kg = 3.8, # Better Economy
  Initial_Energy_kJ_per_kg = 35, # Higher Initial Energy (Nutrition)
  Fatigue_Resistance = 1.2, # Improved Fatigue Resistance (Training)
  Energy_Storage_Capacity_kJ_per_kg = 35 # Slightly improved capacity
)


# --- Simulation Settings ---
body_weight_kg <- 70 # kg
total_duration_minutes <- 120 # minutes (Simulate a 2-hour run)
time_step_seconds <- 5 # seconds
total_steps <- (total_duration_minutes * 60) / time_step_seconds

# Model Constants (Simplified relationships)
# These translate the theoretical concepts into simulation effects
FATIGUE_ACCUM_RATE_BASE = 0.0005 # Base fatigue increase per second at threshold pace
FATIGUE_EFFORT_POWER = 3 # Power to which (Pace / LT_Pace) is raised for fatigue accumulation (non-linear increase)
FATIGUE_REDUCTION_FACTOR = 0.8 # How much max fatigue can reduce pace (0 = no effect, 1 = pace -> 0)
ENERGY_REDUCTION_FACTOR = 1.0 # How much full depletion can reduce pace (0 = no effect, 1 = pace -> 0)
ENERGY_CONVERSION_FACTOR = 1 # Placeholder - links kJ/kg expenditure to pace/economy


# --- Helper Function to convert pace ---
min_km_to_m_per_s <- function(pace_min_km) {
  return(1000 / (pace_min_km * 60))
}

m_per_s_to_min_km <- function(pace_m_per_s) {
  if (pace_m_per_s <= 0) return(NA) # Avoid division by zero or negative pace
  return((1000 / pace_m_per_s) / 60)
}

# --- Simulation Function ---
run_simulation <- function(params, body_weight_kg, total_steps, time_step_seconds) {
  
  # Convert key paces to internal units (m/s)
  lt_pace_mps <- min_km_to_m_per_s(params$Lactate_Threshold_Pace_min_km)
  # Let's set the base target pace equal to LT pace for simplicity, deviations occur from here
  base_target_pace_mps <- lt_pace_mps
  
  # Initialize state variables
  current_fatigue <- 0 # Scaled 0 to 1, or conceptually 0 to Max_Possible_Fatigue
  # Let's make max fatigue accumulation reach 1 over a very long time at high effort
  MAX_POSSIBLE_FATIGUE = 1 # Represents complete exhaustion
  current_energy_kJ <- params$Initial_Energy_kJ_per_kg * body_weight_kg
  max_energy_kJ <- params$Energy_Storage_Capacity_kJ_per_kg * body_weight_kg
  
  current_distance_m <- 0
  current_time_seconds <- 0
  current_pace_mps <- base_target_pace_mps # Start at target pace
  
  # Store results
  results <- data.frame(
    Time_s = 0,
    Distance_m = 0,
    Pace_min_km = m_per_s_to_min_km(current_pace_mps),
    Fatigue = current_fatigue,
    Energy_kJ = current_energy_kJ
  )
  
  # Simulation loop
  for (i in 1:total_steps) {
    current_time_seconds <- current_time_seconds + time_step_seconds
    
    # --- Update State Variables ---
    
    # 1. Calculate Energy Depletion
    # Energy cost per meter: (kJ/km/kg * kg / 1000 m/km) * ConversionFactor
    energy_cost_per_meter <- (params$Running_Economy_kJ_per_km_kg * body_weight_kg / 1000) * ENERGY_CONVERSION_FACTOR
    energy_expended_in_step_kJ <- current_pace_mps * time_step_seconds * energy_cost_per_meter
    current_energy_kJ <- current_energy_kJ - energy_expended_in_step_kJ
    # Cap energy at 0
    current_energy_kJ <- max(0, current_energy_kJ)
    
    # 2. Calculate Fatigue Accumulation
    # Fatigue increases faster the harder the effort relative to LT pace
    # Effort factor: How much harder than LT pace? (Current_Pace / LT_Pace)
    effort_factor <- max(1, current_pace_mps / lt_pace_mps) # Effort is >= 1 relative to LT pace
    fatigue_increase_rate <- FATIGUE_ACCUM_RATE_BASE * (effort_factor ^ FATIGUE_EFFORT_POWER) / params$Fatigue_Resistance
    fatigue_increase_in_step <- fatigue_increase_rate * time_step_seconds
    current_fatigue <- current_fatigue + fatigue_increase_in_step
    # Cap fatigue at max possible
    current_fatigue <- min(MAX_POSSIBLE_FATIGUE, current_fatigue)
    
    
    # --- Calculate Pace for Next Step (Based on current state) ---
    # Scaled fatigue (0 to 1)
    scaled_fatigue <- current_fatigue / MAX_POSSIBLE_FATIGUE
    # Scaled energy depletion (0 to 1) - 0 means full, 1 means empty
    scaled_energy_depletion <- 1 - (current_energy_kJ / max_energy_kJ) # Use max_energy_kJ here
    
    # Calculate pace reduction factors
    fatigue_reduction <- FATIGUE_REDUCTION_FACTOR * scaled_fatigue
    energy_reduction <- ENERGY_REDUCTION_FACTOR * scaled_energy_depletion
    
    # Ensure reductions don't cause negative pace conceptually, apply multiplicatively
    # Pace = Base * (1 - reduction_fatigue) * (1 - reduction_energy)
    # Add a small minimum multiplier to avoid exactly 0 pace early on
    pace_multiplier <- max(0.1, (1 - fatigue_reduction) * (1 - energy_reduction))
    
    current_pace_mps <- base_target_pace_mps * pace_multiplier
    
    
    # --- Update Distance ---
    # Use the pace from the *previous* step to calculate distance covered *in* the current step
    # This is standard practice in discrete simulations
    distance_covered_in_step_m <- current_pace_mps * time_step_seconds
    current_distance_m <- current_distance_m + distance_covered_in_step_m
    
    # Store results for this step
    results <- bind_rows(results, data.frame(
      Time_s = current_time_seconds,
      Distance_m = current_distance_m,
      Pace_min_km = m_per_s_to_min_km(current_pace_mps),
      Fatigue = current_fatigue,
      Energy_kJ = current_energy_kJ
    ))
  }
  
  return(results)
}

# --- Run Simulations ---

# Run simulation for Scenario 1
sim_results_1 <- run_simulation(runner_params_1, body_weight_kg, total_steps, time_step_seconds) %>%
  mutate(Scenario = "Scenario 1: Baseline Runner")

# Run simulation for Scenario 2
sim_results_2 <- run_simulation(runner_params_2, body_weight_kg, total_steps, time_step_seconds) %>%
  mutate(Scenario = "Scenario 2: Improved Runner (Training, Nutrition, Equipment)")

# Combine results for plotting
all_sim_results <- bind_rows(sim_results_1, sim_results_2)


# --- Plotting with ggplot2 ---

# Convert Distance_m to km for better plot axis
all_sim_results <- all_sim_results %>%
  mutate(Distance_km = Distance_m / 1000)

# Plot Pace vs. Distance
pace_plot <- ggplot(all_sim_results, aes(x = Distance_km, y = Pace_min_km, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Simulated Runner Pace Over Distance",
    subtitle = "Comparing Baseline vs. Improved Runner Scenario",
    x = "Distance (km)",
    y = "Pace (min/km)",
    color = "Runner Type"
  ) +
  scale_y_reverse() + # Pace is lower (faster) at the bottom
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(pace_plot)

ggsave("model_7_multiscale_runner/runner_2a.png", width = 12, height = 8, dpi = 300)

# Optional: Plot Fatigue vs. Distance
fatigue_plot <- ggplot(all_sim_results, aes(x = Distance_km, y = Fatigue, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Simulated Fatigue Level Over Distance",
    subtitle = "Comparing Baseline vs. Improved Runner Scenario",
    x = "Distance (km)",
    y = "Fatigue Level (Scaled 0-1)",
    color = "Runner Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(fatigue_plot)

ggsave("model_7_multiscale_runner/runner_2b.png", width = 12, height = 8, dpi = 300)

# Optional: Plot Energy vs. Distance
energy_plot <- ggplot(all_sim_results, aes(x = Distance_km, y = Energy_kJ, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Simulated Energy (Glycogen) Stores Over Distance",
    subtitle = "Comparing Baseline vs. Improved Runner Scenario",
    x = "Distance (km)",
    y = "Energy Remaining (kJ)",
    color = "Runner Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(energy_plot)

ggsave("model_7_multiscale_runner/runner_2c.png", width = 12, height = 8, dpi = 300)


# 3. DeepSeek

# Load required libraries
library(ggplot2)
library(deSolve)
library(tidyverse)
library(patchwork)
library(viridis)

# Set seed for reproducibility
set.seed(123)

# ---- Multiscale Runner Model ----

# Model parameters
runner_parameters <- list(
  # Physiological parameters
  VO2max = 65,                # ml/kg/min - maximal oxygen uptake
  LT1 = 0.6,                  # Lactate threshold 1 (%VO2max)
  LT2 = 0.85,                 # Lactate threshold 2 (%VO2max)
  running_economy = 200,      # ml/kg/km - lower is better
  body_mass = 68,             # kg
  fat_percentage = 0.15,      # body fat percentage
  muscle_mass = 30,           # kg
  
  # Biomechanical parameters
  stride_length = 1.5,        # m
  cadence = 180,              # steps/min
  ground_contact_time = 220,  # ms
  vertical_oscillation = 8.5, # cm
  
  # Training parameters
  training_load = 60,         # arbitrary units
  recovery_status = 0.9,      # 0-1 scale (1 = fully recovered)
  
  # Equipment parameters
  shoe_efficiency = 0.97,     # energy return (0-1)
  apparel_drag = 0.95,        # drag coefficient (lower is better)
  
  # Nutritional parameters
  glycogen_stores = 0.85,     # 0-1 scale
  hydration_status = 0.92,    # 0-1 scale
  carb_intake = 6             # g/kg/day
)

# Time parameters
simulation_time <- seq(0, 120, by = 1)  # 120 minutes of running

# Differential equation model of runner physiology
runner_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Calculate current intensity (%VO2max)
    if (time < 10) {
      intensity <- 0.5  # warm-up
    } else if (time < 30) {
      intensity <- 0.7  # steady
    } else if (time < 50) {
      intensity <- 0.85 # threshold
    } else if (time < 70) {
      intensity <- 0.95 # VO2max
    } else if (time < 90) {
      intensity <- 0.65 # recovery
    } else {
      intensity <- 0.8  # final push
    }
    
    # Adjust for glycogen depletion
    intensity_adj <- intensity * glycogen_stores * hydration_status * recovery_status
    
    # Energy expenditure calculations
    vo2 <- VO2max * intensity_adj
    energy_expenditure <- vo2 * body_mass * 0.005 * (1 + (1 - shoe_efficiency))
    
    # Fatigue calculations
    fatigue_rate <- ifelse(intensity_adj > LT2, 0.015, 
                           ifelse(intensity_adj > LT1, 0.008, 0.003))
    
    # Glycogen depletion
    glycogen_depletion <- ifelse(intensity_adj > LT1, 
                                 0.002 * intensity_adj^2, 
                                 0.001 * intensity_adj)
    
    # Hydration loss
    hydration_loss <- 0.0015 * intensity_adj * (1 - hydration_status)
    
    # Lactate accumulation
    lactate_prod <- ifelse(intensity_adj > LT2, 0.02,
                           ifelse(intensity_adj > LT1, 0.01, -0.005))
    
    # Biomechanical efficiency
    biomech_eff <- 0.9 + 0.1 * (1 - (ground_contact_time - 200)/100) * 
      (1 - (vertical_oscillation - 6)/10)
    
    # Differential equations
    dglycogen <- -glycogen_depletion
    dhydration <- -hydration_loss
    dfatigue <- fatigue_rate
    dlactate <- lactate_prod - 0.01 * lactate  # lactate clearance
    
    # Performance metric
    performance <- (vo2 / running_economy) * biomech_eff * 
      (1 - 0.5 * fatigue) * shoe_efficiency
    
    # Return the rates of change
    list(c(dglycogen, dhydration, dfatigue, dlactate),
         performance = performance,
         intensity = intensity_adj,
         vo2_utilization = vo2,
         biomech_eff = biomech_eff)
  })
}

# Initial state
initial_state <- c(
  glycogen = runner_parameters$glycogen_stores,
  hydration = runner_parameters$hydration_status,
  fatigue = 0,
  lactate = 0.5
)

# Run simulation
output <- ode(y = initial_state, times = simulation_time, func = runner_model, 
              parms = runner_parameters) %>% 
  as.data.frame()

# Convert to tidy format for plotting
output_long <- output %>%
  as_tibble() %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value") %>%
  mutate(variable_type = case_when(
    variable %in% c("glycogen", "hydration", "fatigue", "lactate") ~ "Physiological State",
    variable %in% c("performance", "biomech_eff") ~ "Performance Metrics",
    variable %in% c("intensity", "vo2_utilization") ~ "Exercise Intensity",
    TRUE ~ "Other"
  ))

# ---- Create Plots ----

# Color palette
perf_palette <- viridis(6)

# 1. Physiological State Plot
physio_plot <- output_long %>%
  filter(variable_type == "Physiological State") %>%
  ggplot(aes(x = time, y = value, color = variable)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = perf_palette[1:4],
                     labels = c("Fatigue", "Glycogen", "Hydration", "Lactate")) +
  labs(title = "Physiological State During Run",
       x = "Time (minutes)",
       y = "Relative Level",
       color = "Variable") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank())

# 2. Performance Metrics Plot
perf_plot <- output_long %>%
  filter(variable_type == "Performance Metrics") %>%
  ggplot(aes(x = time, y = value, color = variable)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = perf_palette[5:6],
                     labels = c("Biomechanical Efficiency", "Performance")) +
  labs(title = "Performance Metrics",
       x = "Time (minutes)",
       y = "Relative Performance",
       color = "Metric") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank())

# 3. Exercise Intensity Plot
intensity_plot <- output_long %>%
  filter(variable_type == "Exercise Intensity") %>%
  ggplot(aes(x = time, y = value, color = variable)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = runner_parameters$LT1 * runner_parameters$VO2max, 
             linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = runner_parameters$LT2 * runner_parameters$VO2max, 
             linetype = "dashed", color = "gray50") +
  annotate("text", x = 10, y = runner_parameters$LT1 * runner_parameters$VO2max + 2, 
           label = "LT1", color = "gray30") +
  annotate("text", x = 10, y = runner_parameters$LT2 * runner_parameters$VO2max + 2, 
           label = "LT2", color = "gray30") +
  scale_color_manual(values = perf_palette[c(2,4)],
                     labels = c("Exercise Intensity", "VO2 Utilization")) +
  labs(title = "Exercise Intensity and VO2 Utilization",
       x = "Time (minutes)",
       y = "VO2 (ml/kg/min)",
       color = "Variable") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank())

# Combine plots
combined_plot <- (physio_plot / perf_plot / intensity_plot) +
  plot_annotation(title = "Multiscale Runner Performance Model",
                  subtitle = "Integration of Biomechanical, Physiological, and Training Factors",
                  theme = theme(plot.title = element_text(size = 16, face = "bold"),
                                plot.subtitle = element_text(size = 12)))

# Display the plot
print(combined_plot)

# Save the plot
ggsave("model_7_multiscale_runner/runner_3.png", width = 12, height = 12, dpi = 300)

# 4. Perplexity

# Load libraries
library(ggplot2)
library(dplyr)

# Define runner parameters
runner <- list(
  VO2max = 70,                # ml/kg/min
  lactate_threshold_pct = 0.85, # Fraction of VO2max at LT
  running_economy = 200,      # ml O2/kg/km
  stride_length = 1.2,        # meters
  ground_contact_time = 0.22, # seconds
  shoe_mass = 250,            # grams
  carb_availability = 1.0     # 0-1 scale
)

# Model equations
# Sustainable speed at LT (km/h)
speed_LT <- function(VO2max, LT_pct, RE) {
  # VO2 at LT (ml/kg/min)
  VO2_LT <- VO2max * LT_pct
  # Speed at LT (km/h)
  speed <- (VO2_LT / RE) * 60
  return(speed)
}

# Equipment effect on economy (simplified): +1% O2 cost per 100g extra shoe mass
adjusted_economy <- function(RE, shoe_mass) {
  RE * (1 + 0.01 * ((shoe_mass - 200) / 100))
}

# Nutrition effect: Reduces sustainable speed if carbs <1
nutrition_factor <- function(speed, carb_avail) {
  speed * carb_avail
}

# Calculate adjusted running economy
RE_adj <- adjusted_economy(runner$running_economy, runner$shoe_mass)

# Calculate sustainable speed
v_LT <- speed_LT(runner$VO2max, runner$lactate_threshold_pct, RE_adj)
v_LT_nutrition <- nutrition_factor(v_LT, runner$carb_availability)

# Predict marathon time (km/h to hours for 42.195 km)
marathon_time_hr <- 42.195 / v_LT_nutrition

# Simulate effect of varying one parameter (e.g., VO2max)
VO2_seq <- seq(55, 85, by=2)
sim_data <- data.frame(
  VO2max = VO2_seq,
  Speed = sapply(VO2_seq, function(vo2) {
    v <- speed_LT(vo2, runner$lactate_threshold_pct, RE_adj)
    nutrition_factor(v, runner$carb_availability)
  })
)
sim_data$MarathonTime <- 42.195 / sim_data$Speed

# Plot
ggplot(sim_data, aes(x=VO2max, y=MarathonTime)) +
  geom_line(size=1.2, color="#2C3E50") +
  geom_point(size=2, color="#E74C3C") +
  labs(
    title="Predicted Marathon Time vs VO2max",
    x="VO2max (ml/kg/min)",
    y="Marathon Time (hours)"
  ) +
  theme_minimal(base_size = 15)

ggsave("model_7_multiscale_runner/runner_4.png", width = 9, height = 6, dpi = 300)


# 5. ChatGPT

# a. Setup

# Install packages if not already installed
#install.packages(c("ggplot2", "dplyr"))

# Load libraries
library(ggplot2)
library(dplyr)

# b. Define the Vₘₐₓ Prediction Function

# Function to predict Vmax
predict_vmax <- function(fat_pct, hr_2vt, tmc) {
  vmax <- 58.632 - 0.183 * fat_pct - 0.507 * hr_2vt + 7.959 * tmc
  return(vmax)
}

# c. Simulate Runner Data

# Sample data for 10 runners
set.seed(123)  # For reproducibility
runner_data <- data.frame(
  Runner = paste0("Runner_", 1:10),
  Fat_Percentage = runif(10, 10, 25),       # Fat percentage between 10% and 25%
  HR_2VT = runif(10, 70, 85),               # HR at 2VT between 70% and 85% of HRmax
  TMC = runif(10, 0.9, 1.2)                 # Theoretical metabolic cost between 0.9 and 1.2
)

# Calculate Vmax for each runner
runner_data <- runner_data %>%
  rowwise() %>%
  mutate(Vmax = predict_vmax(Fat_Percentage, HR_2VT, TMC)) %>%
  ungroup()

# d. Visualize the Results

# Plotting
ggplot(runner_data, aes(x = Fat_Percentage, y = Vmax, color = HR_2VT, size = TMC)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Predicted Vmax Based on Runner Characteristics",
    x = "Fat Percentage (%)",
    y = "Vmax (km/h)",
    color = "HR at 2VT (%)",
    size = "Theoretical Metabolic Cost"
  ) +
  theme_minimal()


ggsave("model_7_multiscale_runner/runner_5.png", width = 9, height = 6, dpi = 300)


# 6. Claude


# Runner Multiscale Model
# Incorporating biomechanics, physiology, training zones, VO2max, lactate thresholds,
# running economy, nutrition, and equipment effects

# Load required libraries
library(tidyverse)
library(patchwork) # For combining plots

# Set random seed for reproducibility
set.seed(42)

#=============================================
# 1. Model Parameters and Simulation Settings
#=============================================

# Simulation duration (in weeks)
weeks <- 24
days <- weeks * 7

# Create date sequence - making sure the length matches days
dates <- seq.Date(from = Sys.Date() - days + 1, to = Sys.Date(), by = "day")

# Verify that the length of dates matches days
stopifnot(length(dates) == days)

# Base runner characteristics
base_params <- list(
  # Physiological parameters
  vo2max_base = 45,           # Starting VO2max (ml/kg/min)
  vo2max_potential = 60,      # Maximum potential VO2max
  lactate_threshold_pct = 0.7, # Initial lactate threshold (% of VO2max)
  running_economy_base = 210, # Initial running economy (ml/kg/km)
  
  # Biomechanical parameters
  stride_length_base = 1.2,   # Initial stride length (m)
  ground_contact_time = 0.25, # Ground contact time (s)
  vertical_oscillation = 8,   # Vertical oscillation (cm)
  
  # Training adaptation parameters
  recovery_rate = 0.92,       # Recovery rate after training
  adaptation_rate = 0.03,     # Rate of positive adaptation
  detraining_rate = 0.005,    # Rate of fitness loss during rest
  
  # Nutrition parameters
  carb_loading = 1,           # Carbohydrate loading effect (multiplier)
  hydration = 1,              # Hydration effect (multiplier)
  
  # Equipment parameters
  shoe_efficiency = 1,        # Shoe efficiency multiplier
  body_weight = 70,           # Runner's body weight (kg)
  
  # Environmental parameters
  temperature = 20,           # Default temperature (Celsius)
  altitude = 0                # Default altitude (meters)
)

#=============================================
# 2. Training Zones Definition
#=============================================

training_zones <- data.frame(
  zone = c(1, 2, 3, 4, 5),
  name = c("Recovery", "Endurance", "Tempo", "Threshold", "VO2max"),
  intensity_low = c(0.60, 0.70, 0.80, 0.87, 0.95),
  intensity_high = c(0.70, 0.80, 0.87, 0.95, 1.0),
  stimulus_factor = c(0.5, 1.0, 1.5, 2.0, 2.5),
  fatigue_factor = c(0.4, 0.8, 1.3, 1.8, 2.3)
)

#=============================================
# 3. Training Plan Generation
#=============================================

# Function to generate a somewhat realistic training plan
generate_training_plan <- function(days, dates) {
  # Create data frame for each day
  training_plan <- data.frame(
    day = 1:days,
    date = dates,
    weekday = weekdays(dates),
    week = ceiling(1:days / 7)
  )
  
  # Base distance distribution
  training_plan$distance_base <- 0
  
  # Generate three mesocycles with different focuses
  for (w in 1:weeks) {
    week_idx <- which(training_plan$week == w)
    
    if (w <= 8) {
      # Base building phase
      phase_multiplier <- 0.8 + (w / 8) * 0.2
    } else if (w <= 16) {
      # Intensity phase
      phase_multiplier <- 1.0 + ((w - 8) / 8) * 0.2
    } else {
      # Taper and peak phase
      phase_multiplier <- 1.2 * (1 - (w - 16) / 8 * 0.3)
    }
    
    # Apply weekly structure
    for (d in week_idx) {
      weekday <- training_plan$weekday[d]
      
      if (weekday == "Monday") {
        # Recovery
        training_plan$distance_base[d] <- 5 * phase_multiplier
        training_plan$zone[d] <- 1
      } else if (weekday == "Tuesday") {
        # Workout
        training_plan$distance_base[d] <- 8 * phase_multiplier
        training_plan$zone[d] <- ifelse(w <= 8, 3, ifelse(w <= 16, 4, 3))
      } else if (weekday == "Wednesday") {
        # Easy
        training_plan$distance_base[d] <- 6 * phase_multiplier
        training_plan$zone[d] <- 2
      } else if (weekday == "Thursday") {
        # Workout
        training_plan$distance_base[d] <- 8 * phase_multiplier
        training_plan$zone[d] <- ifelse(w <= 8, 2, ifelse(w <= 16, 5, 4))
      } else if (weekday == "Friday") {
        # Recovery or rest
        training_plan$distance_base[d] <- 4 * phase_multiplier
        training_plan$zone[d] <- 1
      } else if (weekday == "Saturday") {
        # Long run
        training_plan$distance_base[d] <- 12 * phase_multiplier
        training_plan$zone[d] <- 2
      } else if (weekday == "Sunday") {
        # Rest or easy
        if (w %% 4 == 0) { # Rest every 4th week
          training_plan$distance_base[d] <- 0
          training_plan$zone[d] <- 0
        } else {
          training_plan$distance_base[d] <- 6 * phase_multiplier
          training_plan$zone[d] <- 1
        }
      }
    }
  }
  
  # Add random variations to distances (+-10%)
  training_plan$distance <- pmax(0, training_plan$distance_base * 
                                   (1 + rnorm(days, 0, 0.1)))
  
  # Set zone intensity and names
  training_plan$zone_name <- "Rest"
  training_plan$intensity <- 0
  training_plan$stimulus <- 0
  training_plan$fatigue <- 0
  
  for (i in 1:nrow(training_zones)) {
    zone_idx <- which(training_plan$zone == training_zones$zone[i])
    training_plan$zone_name[zone_idx] <- as.character(training_zones$name[i])
    
    # Randomize intensity within zone boundaries
    for (d in zone_idx) {
      low <- training_zones$intensity_low[i]
      high <- training_zones$intensity_high[i]
      training_plan$intensity[d] <- runif(1, low, high)
      training_plan$stimulus[d] <- training_plan$distance[d] * 
        training_zones$stimulus_factor[i]
      training_plan$fatigue[d] <- training_plan$distance[d] * 
        training_zones$fatigue_factor[i]
    }
  }
  
  # Adding workout descriptions
  training_plan$workout_description <- ""
  
  for (d in 1:nrow(training_plan)) {
    if (training_plan$zone[d] == 0) {
      training_plan$workout_description[d] <- "Rest day"
    } else if (training_plan$zone[d] == 1) {
      training_plan$workout_description[d] <- paste0(round(training_plan$distance[d], 1), 
                                                     "km easy recovery run")
    } else if (training_plan$zone[d] == 2) {
      if (training_plan$weekday[d] == "Saturday") {
        training_plan$workout_description[d] <- paste0(round(training_plan$distance[d], 1), 
                                                       "km long run")
      } else {
        training_plan$workout_description[d] <- paste0(round(training_plan$distance[d], 1), 
                                                       "km endurance run")
      }
    } else if (training_plan$zone[d] == 3) {
      training_plan$workout_description[d] <- paste0(round(training_plan$distance[d] * 0.3, 1), 
                                                     "km warmup + ", 
                                                     round(training_plan$distance[d] * 0.5, 1), 
                                                     "km tempo + ",
                                                     round(training_plan$distance[d] * 0.2, 1), 
                                                     "km cooldown")
    } else if (training_plan$zone[d] == 4) {
      training_plan$workout_description[d] <- paste0(round(training_plan$distance[d] * 0.25, 1), 
                                                     "km warmup + 6x1km @threshold w/2min rest + ",
                                                     round(training_plan$distance[d] * 0.25, 1), 
                                                     "km cooldown")
    } else if (training_plan$zone[d] == 5) {
      training_plan$workout_description[d] <- paste0(round(training_plan$distance[d] * 0.25, 1), 
                                                     "km warmup + 8x400m @VO2max w/90sec rest + ",
                                                     round(training_plan$distance[d] * 0.25, 1), 
                                                     "km cooldown")
    }
  }
  
  return(training_plan)
}

# Generate the training plan
training_plan <- generate_training_plan(days, dates)

#=============================================
# 4. Simulation Functions
#=============================================

# Function to calculate fitness response
calculate_fitness_response <- function(params, plan) {
  # Initialize fitness parameters to track
  plan$vo2max <- params$vo2max_base
  plan$lactate_threshold <- params$vo2max_base * params$lactate_threshold_pct
  plan$running_economy <- params$running_economy_base
  plan$stride_length <- params$stride_length_base
  
  # Initialize fitness and fatigue trackers (fitness-fatigue model)
  plan$fitness <- 0
  plan$fatigue <- 0
  plan$form <- 0 # Form = Fitness - Fatigue
  
  # Simulate day by day
  for(i in 2:nrow(plan)) {
    # Fitness decays slightly each day, but increases with stimulus
    plan$fitness[i] <- plan$fitness[i-1] * (1 - params$detraining_rate) + 
      plan$stimulus[i-1] * params$adaptation_rate
    
    # Fatigue decays each day based on recovery rate
    plan$fatigue[i] <- plan$fatigue[i-1] * params$recovery_rate + 
      plan$fatigue[i-1]
    
    # Form is the difference between fitness and fatigue
    plan$form[i] <- plan$fitness[i] - plan$fatigue[i]
    
    # VO2max adaptation (with ceiling)
    vo2max_gain <- 0.02 * plan$stimulus[i-1] * 
      (1 - (plan$vo2max[i-1] / params$vo2max_potential))
    plan$vo2max[i] <- min(params$vo2max_potential, 
                          plan$vo2max[i-1] + vo2max_gain)
    
    # Lactate threshold adaptation
    lt_pct_increase <- 0.0005 * plan$stimulus[i-1] * (plan$zone[i-1] >= 3)
    new_lt_pct <- params$lactate_threshold_pct + 
      (i/nrow(plan)) * 0.05 + lt_pct_increase
    plan$lactate_threshold[i] <- plan$vo2max[i] * min(0.9, new_lt_pct)
    
    # Running economy improvement (lower is better)
    re_improvement <- 0.05 * plan$stimulus[i-1] * (plan$zone[i-1] >= 2)
    plan$running_economy[i] <- max(180, plan$running_economy[i-1] - re_improvement)
    
    # Stride length adaptation
    sl_improvement <- 0.001 * plan$stimulus[i-1] * (plan$zone[i-1] >= 3)
    plan$stride_length[i] <- min(1.4, plan$stride_length[i-1] + sl_improvement)
  }
  
  # Calculate performance metrics
  plan$predicted_5k_pace <- 60 / (plan$vo2max * 0.8 / plan$running_economy * 60)
  plan$predicted_marathon_pace <- 60 / (plan$lactate_threshold * 0.8 / plan$running_economy * 60)
  
  return(plan)
}

# Simulate the model
simulation_results <- calculate_fitness_response(base_params, training_plan)

#=============================================
# 5. Visualization
#=============================================

# Format dates for better plotting
simulation_results$date_formatted <- format(simulation_results$date, "%b %d")
simulation_results$week_label <- paste("Week", simulation_results$week)

# Create color palette for training zones
zone_colors <- c("Rest" = "gray80", 
                 "Recovery" = "#9ecae1", 
                 "Endurance" = "#6baed6", 
                 "Tempo" = "#4292c6", 
                 "Threshold" = "#2171b5", 
                 "VO2max" = "#084594")

# Fix for newer ggplot2 versions - modify sec_axis to not use call parameter
# Plot 1: Training load and physiological adaptations
p1 <- ggplot(simulation_results, aes(x = date)) +
  geom_col(aes(y = distance, fill = zone_name), alpha = 0.7) +
  geom_line(aes(y = vo2max/2, color = "VO2max"), size = 1) +
  geom_line(aes(y = lactate_threshold/2, color = "Lactate Threshold"), size = 1) +
  geom_line(aes(y = running_economy/10, color = "Running Economy"), size = 1, linetype = "dashed") +
  scale_fill_manual(values = zone_colors, name = "Training Zone") +
  scale_color_manual(values = c("VO2max" = "#d73027", 
                                "Lactate Threshold" = "#fc8d59",
                                "Running Economy" = "#fee090"), 
                     name = "Physiological Metrics") +
  scale_y_continuous(
    name = "Daily Distance (km)",
    #sec.axis = sec_axis(~.*2, name = "Physiological Values")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray80")
  ) +
  labs(
    title = "Multiscale Runner Model: Training Load and Physiological Adaptations",
    x = "Date"
  )

p1

ggsave("model_7_multiscale_runner/runner_6a.png", width = 9, height = 6, dpi = 300)


# Plot 2: Performance predictions and form
p2 <- ggplot(simulation_results, aes(x = date)) +
  geom_ribbon(aes(ymin = 0, ymax = fitness, fill = "Fitness"), alpha = 0.3) +
  geom_ribbon(aes(ymin = 0, ymax = fatigue, fill = "Fatigue"), alpha = 0.3) +
  geom_line(aes(y = form * 2, color = "Form"), size = 1) +
  geom_line(aes(y = predicted_5k_pace, color = "5K Pace (min/km)"), size = 1) +
  geom_line(aes(y = predicted_marathon_pace, color = "Marathon Pace (min/km)"), size = 1) +
  scale_fill_manual(values = c("Fitness" = "#4daf4a", "Fatigue" = "#e41a1c"), 
                    name = "Training Effect") +
  scale_color_manual(values = c("Form" = "#984ea3", 
                                "5K Pace (min/km)" = "#ff7f00", 
                                "Marathon Pace (min/km)" = "#a65628"), 
                     name = "Performance Metrics") +
  scale_y_continuous(name = "Metrics Value") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray80")
  ) +
  labs(
    title = "Performance Predictions and Training Form",
    x = "Date"
  )

p2

ggsave("model_7_multiscale_runner/runner_6b.png", width = 9, height = 6, dpi = 300)


# Plot 3: Weekly distance summary
weekly_summary <- simulation_results %>%
  group_by(week) %>%
  summarize(
    total_distance = sum(distance),
    avg_intensity = mean(intensity[intensity > 0], na.rm = TRUE),
    high_intensity_pct = sum(distance[zone >= 3]) / sum(distance) * 100,
    week_start_date = min(date)
  )

p3 <- ggplot(weekly_summary, aes(x = week_start_date)) +
  geom_col(aes(y = total_distance, fill = high_intensity_pct)) +
  geom_line(aes(y = avg_intensity * 100, color = "Avg Intensity"), size = 1) +
  scale_fill_gradient(low = "#bae4b3", high = "#238b45", 
                      name = "High Intensity %") +
  scale_color_manual(values = c("Avg Intensity" = "#fc8d59"), 
                     name = "Intensity") +
  scale_y_continuous(
    name = "Weekly Distance (km)",
    #sec.axis = sec_axis(~./100, name = "Average Intensity")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray80")
  ) +
  labs(
    title = "Weekly Training Summary",
    x = "Week Starting Date"
  )

p3

ggsave("model_7_multiscale_runner/runner_6c.png", width = 9, height = 6, dpi = 300)

(p1 / p2 / p3)

#ggsave("model_7_multiscale_runner/runner_multiscale_model.png", width = 8, height = 12, dpi = 300)

# Let's try a simpler version of the plots if the above still causes issues

#=============================================
# 6. Model Interpretation
#=============================================

# Extract key insights from the model
final_stats <- simulation_results %>%
  slice(n()) %>%
  select(vo2max, lactate_threshold, running_economy, predicted_5k_pace, predicted_marathon_pace)

# Print key insights
cat("\nFinal Model Statistics:\n")
cat("VO2max at end:", round(final_stats$vo2max, 1), "ml/kg/min\n")
cat("Lactate Threshold at end:", round(final_stats$lactate_threshold, 1), "ml/kg/min\n")
cat("Running Economy at end:", round(final_stats$running_economy, 1), "ml/kg/km\n")
cat("Predicted 5K Pace:", round(final_stats$predicted_5k_pace, 2), "min/km\n")
cat("Predicted Marathon Pace:", round(final_stats$predicted_marathon_pace, 2), "min/km\n")

# Calculate peak form period
peak_form <- simulation_results %>%
  arrange(desc(form)) %>%
  slice(1:7) %>%
  summarize(
    peak_form_value = mean(form),
    peak_form_week = mean(week),
    peak_form_vo2max = mean(vo2max),
    peak_5k_pace = mean(predicted_5k_pace)
  )

cat("\nPeak Form Period:\n")
cat("Around Week:", round(peak_form$peak_form_week, 1), "\n")
cat("Peak Form Value:", round(peak_form$peak_form_value, 2), "\n")
cat("Peak VO2max during peak form:", round(peak_form$peak_form_vo2max, 1), "ml/kg/min\n")
cat("Peak 5K Pace:", round(peak_form$peak_5k_pace, 2), "min/km\n")

# Create a simpler visualization alternative using base R if ggplot2 has issues
# This is just a fallback if the ggplot2 versions cause problems
par(mfrow=c(3,1))

try({
  # Plot 1: Training and physiological metrics
  plot(simulation_results$date, simulation_results$distance, 
       type="h", col=factor(simulation_results$zone), 
       xlab="Date", ylab="Distance (km)",
       main="Training Load and Physiological Adaptations")
  lines(simulation_results$date, simulation_results$vo2max/2, col="red", lwd=2)
  lines(simulation_results$date, simulation_results$lactate_threshold/2, col="orange", lwd=2)
  legend("topleft", legend=c("Distance", "VO2max", "LT"), 
         col=c("black", "red", "orange"), lty=c(1,1,1), cex=0.8)
  
  # Plot 2: Performance
  plot(simulation_results$date, simulation_results$fitness, 
       type="l", col="green", xlab="Date", ylab="Value",
       main="Performance Predictions and Form")
  lines(simulation_results$date, simulation_results$fatigue, col="red", lwd=2)
  lines(simulation_results$date, simulation_results$form, col="purple", lwd=2)
  lines(simulation_results$date, simulation_results$predicted_5k_pace, col="blue", lwd=2)
  legend("topleft", legend=c("Fitness", "Fatigue", "Form", "5K Pace"), 
         col=c("green", "red", "purple", "blue"), lty=c(1,1,1,1), cex=0.8)
  
  # Plot 3: Weekly summary
  plot(weekly_summary$week_start_date, weekly_summary$total_distance, 
       type="h", col="darkgreen", xlab="Week", ylab="Weekly Distance (km)",
       main="Weekly Training Summary")
  lines(weekly_summary$week_start_date, weekly_summary$avg_intensity*100, col="orange", lwd=2)
  legend("topleft", legend=c("Weekly Distance", "Avg Intensity"), 
         col=c("darkgreen", "orange"), lty=c(1,1), cex=0.8)
}, silent = TRUE)
