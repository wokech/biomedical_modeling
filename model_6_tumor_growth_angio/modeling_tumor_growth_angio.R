# Improved Model of Tumor Growth and Angiogenesis (with oxygen and VEGF)
# Pre- and Post-treatment Simulation with More Dynamic VEGF Behavior

# Libraries
library(deSolve)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(patchwork)

# Set random seed for reproducibility
set.seed(123)

# Model parameters with adjusted VEGF dynamics
params <- list(
  # Tumor growth parameters
  r = 0.05,            # Intrinsic tumor growth rate
  K = 1.0,             # Carrying capacity
  
  # Oxygen parameters
  D_o = 0.1,           # Oxygen diffusion rate
  p_o = 0.03,          # Oxygen production rate (from blood vessels)
  c_o = 0.02,          # Oxygen consumption rate by tumor cells
  
  # VEGF parameters - modified for more dynamic behavior
  D_v = 0.08,          # VEGF diffusion rate (increased)
  p_v = 0.2,           # VEGF production rate (increased for more sensitivity)
  c_v = 0.05,          # VEGF degradation rate (increased)
  hypoxia_threshold = 0.7, # Threshold below which tissue is considered hypoxic
  
  # Angiogenesis parameters
  r_v = 0.02,          # Rate of blood vessel formation in response to VEGF
  d_v = 0.01,          # Natural vessel degradation rate
  
  # Treatment parameters
  t_start = 30,        # Treatment start time (day)
  anti_vegf_eff = 0.7, # Anti-VEGF treatment efficacy (reduces VEGF production)
  chemo_eff = 0.4,     # Chemotherapy efficacy (increases tumor cell death)
  radio_eff = 0.3      # Radiotherapy efficacy (direct tumor cell killing)
)

# Initial conditions
init <- c(
  T = 0.01,    # Initial tumor size (normalized)
  O = 1.0,     # Initial oxygen level (normalized)
  V = 0.0,     # Initial VEGF level
  BV = 0.2     # Initial blood vessel density
)

# Time points for simulation
times <- seq(0, 60, by = 0.1)

# Define the ODE system with improved VEGF dynamics
tumor_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Treatment effects (activated after t_start)
    treatment <- ifelse(t >= t_start, 1, 0)
    
    # Adjust parameters based on treatment
    vegf_reduction <- ifelse(treatment == 1, anti_vegf_eff, 0)
    tumor_kill_rate <- ifelse(treatment == 1, chemo_eff + radio_eff, 0)
    
    # Tumor growth equation (logistic growth affected by oxygen and treatment)
    oxygen_factor <- O / (O + 0.1)  # Oxygen-dependent growth factor
    dT <- r * T * (1 - T/K) * oxygen_factor - tumor_kill_rate * T
    
    # Oxygen dynamics - more sensitive to tumor size and vessel density
    dO <- D_o * (1 - O) + p_o * BV * (1 - 0.3*T) - c_o * T * O
    
    # VEGF production - improved hypoxia calculation with threshold
    # More non-linear response to hypoxia using sigmoid-like function
    hypoxia_level <- 1 / (1 + exp(10 * (O - hypoxia_threshold)))
    
    # VEGF increases more with tumor size and responds more strongly to hypoxia
    vegf_production <- p_v * T * hypoxia_level * (1 - vegf_reduction)
    
    # Non-linear VEGF degradation (faster at higher concentrations)
    vegf_degradation <- c_v * V * (1 + 0.5 * V)
    
    # VEGF diffusion and overall dynamics
    dV <- -D_v * V + vegf_production - vegf_degradation
    
    # Blood vessel dynamics - more sensitive to VEGF
    dBV <- r_v * V * (1 - BV) - d_v * BV + 0.005 * (1 - treatment * 0.5)
    
    # Add some feedback from vessels to tumor
    dT <- dT + 0.01 * BV * T * (1 - treatment * 0.2)
    
    return(list(c(dT, dO, dV, dBV)))
  })
}

# Solve the ODE system
out <- ode(y = init, times = times, func = tumor_model, parms = params)

# Convert output to data frame
results <- as.data.frame(out)
colnames(results) <- c("Time", "Tumor", "Oxygen", "VEGF", "BloodVessels")

# Add treatment phase indicator
results$Treatment <- ifelse(results$Time >= params$t_start, "Post-treatment", "Pre-treatment")

# Calculate hypoxia level for understanding VEGF dynamics
results$Hypoxia <- with(results, 1 / (1 + exp(10 * (Oxygen - params$hypoxia_threshold))))

# Add model diagnostics - VEGF production and degradation rates
results$VEGF_Production <- with(results, params$p_v * Tumor * Hypoxia * 
                                  ifelse(Treatment == "Post-treatment", 1 - params$anti_vegf_eff, 1))
results$VEGF_Degradation <- with(results, params$c_v * VEGF * (1 + 0.5 * VEGF))
results$VEGF_NetChange <- with(results, VEGF_Production - VEGF_Degradation - params$D_v * VEGF)

# Create plots
plot_tumor <- ggplot(results, aes(x = Time, y = Tumor, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Tumor Growth Dynamics", 
       x = "Time (days)", 
       y = "Tumor Size (normalized)") +
  theme_minimal()

plot_oxygen <- ggplot(results, aes(x = Time, y = Oxygen, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Oxygen Dynamics", 
       x = "Time (days)", 
       y = "Oxygen Level (normalized)") +
  theme_minimal()

# Improved VEGF plot
plot_vegf <- ggplot(results, aes(x = Time, y = VEGF, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "VEGF Dynamics", 
       x = "Time (days)", 
       y = "VEGF Level (normalized)") +
  theme_minimal()

plot_bv <- ggplot(results, aes(x = Time, y = BloodVessels, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Blood Vessel Density", 
       x = "Time (days)", 
       y = "Blood Vessel Density (normalized)") +
  theme_minimal()

# VEGF component analysis plot
plot_vegf_components <- ggplot(results, aes(x = Time)) +
  geom_line(aes(y = VEGF_Production, color = "Production"), size = 1) +
  geom_line(aes(y = VEGF_Degradation, color = "Degradation"), size = 1) +
  geom_line(aes(y = VEGF_NetChange, color = "Net Change"), size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "VEGF Component Analysis", 
       x = "Time (days)", 
       y = "Rate",
       color = "Component") +
  theme_minimal()

# Plot hypoxia
plot_hypoxia <- ggplot(results, aes(x = Time, y = Hypoxia, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Hypoxia Level", 
       x = "Time (days)", 
       y = "Hypoxia Level (normalized)") +
  theme_minimal()

# Combine main plots
grid.arrange(plot_tumor, plot_oxygen, plot_vegf, plot_bv, ncol = 2)

(plot_tumor + plot_oxygen) / (plot_vegf + plot_bv)

ggsave("images/modeling_tumor_growth_angio_1.png", height = 8, width = 12, dpi = 300)

# Combine diagnostic plots
grid.arrange(plot_vegf_components, plot_hypoxia, ncol = 2)

# Create a phase plot of Tumor vs VEGF
plot_phase <- ggplot(results, aes(x = Tumor, y = VEGF, color = Treatment)) +
  geom_path(size = 1, arrow = arrow(length = unit(0.1, "inches"), ends = "last", type = "closed")) +
  labs(title = "Phase Plot: Tumor vs VEGF", 
       x = "Tumor Size", 
       y = "VEGF Level") +
  theme_minimal()

# Create a phase plot of Oxygen vs VEGF
plot_phase2 <- ggplot(results, aes(x = Oxygen, y = VEGF, color = Treatment)) +
  geom_path(size = 1, arrow = arrow(length = unit(0.1, "inches"), ends = "last", type = "closed")) +
  labs(title = "Phase Plot: Oxygen vs VEGF", 
       x = "Oxygen Level", 
       y = "VEGF Level") +
  theme_minimal()

# Show phase plots
grid.arrange(plot_phase, plot_phase2, ncol = 2)

# Function to run sensitivity analysis
run_sensitivity <- function(parameter_name, values, params) {
  results_list <- list()
  
  for (val in values) {
    # Create modified parameters
    mod_params <- params
    mod_params[[parameter_name]] <- val
    
    # Solve the ODE with modified parameters
    out <- ode(y = init, times = times, func = tumor_model, parms = mod_params)
    df <- as.data.frame(out)
    colnames(df) <- c("Time", "Tumor", "Oxygen", "VEGF", "BloodVessels")
    df$ParamValue <- val
    
    results_list[[length(results_list) + 1]] <- df
  }
  
  # Combine results
  do.call(rbind, results_list)
}

# Run sensitivity analysis on VEGF production rate
vegf_prod_values <- seq(0.05, 0.40, by = 0.05)
sens_vegf_prod <- run_sensitivity("p_v", vegf_prod_values, params)

# Plot sensitivity analysis for VEGF production rate effect on VEGF levels
plot_sens_vegf_prod <- ggplot(sens_vegf_prod, aes(x = Time, y = VEGF, color = as.factor(ParamValue))) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Sensitivity to VEGF Production Rate", 
       x = "Time (days)", 
       y = "VEGF Level",
       color = "Production\nRate") +
  theme_minimal()

# Run sensitivity analysis on anti-VEGF efficacy
anti_vegf_values <- seq(0, 0.9, by = 0.3)
sens_anti_vegf <- run_sensitivity("anti_vegf_eff", anti_vegf_values, params)

# Plot sensitivity analysis for anti-VEGF efficacy
plot_sens_anti_vegf <- ggplot(sens_anti_vegf, aes(x = Time, y = VEGF, color = as.factor(ParamValue))) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Sensitivity to Anti-VEGF Efficacy", 
       x = "Time (days)", 
       y = "VEGF Level",
       color = "Anti-VEGF\nEfficacy") +
  theme_minimal()

# Combine sensitivity plots
grid.arrange(plot_sens_vegf_prod, plot_sens_anti_vegf, ncol = 2)

# Create a prediction of different treatment scenarios
run_treatment_scenario <- function(anti_vegf_eff, chemo_eff, radio_eff, params) {
  # Create modified parameters
  mod_params <- params
  mod_params$anti_vegf_eff <- anti_vegf_eff
  mod_params$chemo_eff <- chemo_eff
  mod_params$radio_eff <- radio_eff
  
  # Solve the ODE with modified parameters
  out <- ode(y = init, times = times, func = tumor_model, parms = mod_params)
  df <- as.data.frame(out)
  colnames(df) <- c("Time", "Tumor", "Oxygen", "VEGF", "BloodVessels")
  
  # Add treatment identifier
  if (anti_vegf_eff > 0 && chemo_eff > 0 && radio_eff > 0) {
    df$Treatment <- "Combined Therapy"
  } else if (anti_vegf_eff > 0 && chemo_eff == 0 && radio_eff == 0) {
    df$Treatment <- "Anti-VEGF Only"
  } else if (anti_vegf_eff == 0 && (chemo_eff > 0 || radio_eff > 0)) {
    df$Treatment <- "Chemo/Radio Only"
  } else {
    df$Treatment <- "No Treatment"
  }
  
  return(df)
}

# Run different treatment scenarios
no_treatment <- run_treatment_scenario(0, 0, 0, params)
anti_vegf_only <- run_treatment_scenario(0.7, 0, 0, params)
chemo_radio_only <- run_treatment_scenario(0, 0.4, 0.3, params)
combined_therapy <- run_treatment_scenario(0.7, 0.4, 0.3, params)

# Combine all scenarios
all_scenarios <- rbind(no_treatment, anti_vegf_only, chemo_radio_only, combined_therapy)

# Plot comparison of tumor growth under different treatment scenarios
plot_treatment_compare <- ggplot(all_scenarios, aes(x = Time, y = Tumor, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "Tumor Growth Under Different Treatment Scenarios", 
       x = "Time (days)", 
       y = "Tumor Size (normalized)") +
  theme_minimal()

# Plot comparison of VEGF levels under different treatment scenarios
plot_vegf_compare <- ggplot(all_scenarios, aes(x = Time, y = VEGF, color = Treatment)) +
  geom_line(size = 1) +
  geom_vline(xintercept = params$t_start, linetype = "dashed", color = "red") +
  labs(title = "VEGF Levels Under Different Treatment Scenarios", 
       x = "Time (days)", 
       y = "VEGF Level (normalized)") +
  theme_minimal()

# Show treatment comparison plots
grid.arrange(plot_treatment_compare, plot_vegf_compare, ncol = 1)

plot_treatment_compare / plot_vegf_compare

ggsave("images/modeling_tumor_growth_angio_2.png", height = 8, width = 12, dpi = 300)

# Print summary of treatment effects from the original simulation
pre_treatment <- subset(results, Time < params$t_start)
post_treatment <- subset(results, Time >= params$t_start)

# Create a data frame for final values
final_pre <- tail(pre_treatment, 1)
final_post <- tail(post_treatment, 1)

cat("Analysis of VEGF Dynamics in the Model:\n")
cat("---------------------------------------\n")
cat("Initial VEGF level:", init["V"], "\n")
cat("Maximum VEGF level reached:", max(results$VEGF), "\n")
cat("VEGF level before treatment (day", params$t_start, "):", pre_treatment[nrow(pre_treatment), "VEGF"], "\n")
cat("Final VEGF level after treatment:", post_treatment[nrow(post_treatment), "VEGF"], "\n")
cat("VEGF reduction due to treatment:", 
    (1 - post_treatment[nrow(post_treatment), "VEGF"]/pre_treatment[nrow(pre_treatment), "VEGF"]) * 100, "%\n\n")

cat("Key factors affecting VEGF dynamics:\n")
cat("1. VEGF production rate (p_v):", params$p_v, "\n")
cat("2. VEGF degradation rate (c_v):", params$c_v, "\n") 
cat("3. VEGF diffusion rate (D_v):", params$D_v, "\n")
cat("4. Hypoxia threshold:", params$hypoxia_threshold, "\n")
cat("5. Anti-VEGF treatment efficacy:", params$anti_vegf_eff, "\n\n")

cat("Maximum VEGF production rate:", max(results$VEGF_Production), "\n")
cat("Maximum VEGF degradation rate:", max(results$VEGF_Degradation), "\n")

