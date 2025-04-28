# R Script: PBPK Model for Antihypertensive Therapy in Pregnant Women

# The AI Chatbots from Deepseek, Claude, and ChatGPT were used to help
# generate the code shown below.

# DeepSeek

# Load required libraries
library(deSolve)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)
library(tidyverse)

# Define PBPK model function
pbpk_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Drug administration (oral dose as a bolus input)
    if (time <= dosing_interval) {
      Gut <- Gut + (dose * (1 - bioavailability) / dosing_interval)
    }
    
    # Absorption from gut to liver
    dGut <- -ka * Gut
    dLiver <- ka * Gut - (Ql * (Liver / Vl - Plasma / Vp)) - (Clh * Liver / Vl)
    
    # Distribution between plasma and tissues
    dPlasma <- (Ql * (Liver / Vl - Plasma / Vp)) + 
      (Qf * (Fat / Vf - Plasma / Vp)) + 
      (Qm * (Muscle / Vm - Plasma / Vp)) + 
      (Qp * (Placenta / Vplac - Plasma / Vp)) - 
      (Cl * Plasma / Vp)
    
    dFat <- Qf * (Plasma / Vp - Fat / Vf) * (1 - fup)
    dMuscle <- Qm * (Plasma / Vp - Muscle / Vm) * (1 - fup)
    
    # Pregnancy-specific compartments
    dPlacenta <- Qp * (Plasma / Vp - Placenta / Vplac) - Clp * Placenta / Vplac
    dFetus <- Qfetus * (Placenta / Vplac - Fetus / Vfetus)
    
    # Metabolism and excretion
    dMetabolized <- Cl * Plasma / Vp + Clh * Liver / Vl + Clp * Placenta / Vplac
    
    # Return rates of change
    list(c(dGut, dLiver, dPlasma, dFat, dMuscle, dPlacenta, dFetus, dMetabolized))
  })
}

# Parameters for a pregnant woman (hypothetical antihypertensive drug)
parameters <- c(
  # Physiological parameters (pregnancy-adjusted)
  Vp = 6.5,        # Plasma volume (L) - increased in pregnancy
  Ql = 90,         # Liver blood flow (L/h)
  Qf = 10,         # Fat blood flow (L/h)
  Qm = 40,         # Muscle blood flow (L/h)
  Qp = 15,         # Placental blood flow (L/h) - pregnancy specific
  Qfetus = 1,      # Fetal blood flow (L/h)
  
  # Tissue volumes (pregnancy-adjusted)
  Vl = 2.5,        # Liver volume (L)
  Vf = 15,         # Fat volume (L) - increased in pregnancy
  Vm = 25,         # Muscle volume (L)
  Vplac = 0.5,     # Placenta volume (L)
  Vfetus = 0.2,    # Fetus volume (L)
  
  # Drug-specific parameters
  ka = 0.5,        # Absorption rate (1/h)
  bioavailability = 0.8,  # Bioavailability
  fup = 0.2,       # Fraction unbound in plasma
  Cl = 5,          # Systemic clearance (L/h)
  Clh = 10,        # Hepatic clearance (L/h)
  Clp = 2,         # Placental clearance (L/h)
  
  # Dosing parameters
  dose = 50,       # Dose (mg)
  dosing_interval = 0.5 # Dosing interval (h)
)

# Initial conditions (all compartments start at 0 except gut)
state <- c(
  Gut = 0,         # Gut compartment
  Liver = 0,       # Liver compartment
  Plasma = 0,      # Plasma compartment
  Fat = 0,         # Fat tissue
  Muscle = 0,      # Muscle tissue
  Placenta = 0,    # Placenta
  Fetus = 0,       # Fetus
  Metabolized = 0  # Metabolized drug
)

# Time points to simulate (6 hours)
times <- seq(0, 6, by = 0.1)

# Simulate the model
out <- ode(y = state, times = times, func = pbpk_model, parms = parameters)

# Convert to data frame
out_df <- as.data.frame(out)
names(out_df) <- c("time", "Gut", "Liver", "Plasma", "Fat", "Muscle", 
                   "Placenta", "Fetus", "Metabolized")

# Convert to concentrations (mg/L)
out_df$Plasma_conc <- out_df$Plasma / parameters["Vp"]
out_df$Liver_conc <- out_df$Liver / parameters["Vl"]
out_df$Fat_conc <- out_df$Fat / parameters["Vf"]
out_df$Muscle_conc <- out_df$Muscle / parameters["Vm"]
out_df$Placenta_conc <- out_df$Placenta / parameters["Vplac"]
out_df$Fetus_conc <- out_df$Fetus / parameters["Vfetus"]

# Create a theme for consistent aesthetics
pbpk_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Plot plasma concentration (main plot)
main_plot <- ggplot(out_df, aes(x = time)) +
  geom_line(aes(y = Plasma_conc, color = "Plasma"), linewidth = 1.2) +
  geom_line(aes(y = Placenta_conc, color = "Placenta"), linewidth = 1.2) +
  geom_line(aes(y = Fetus_conc, color = "Fetus"), linewidth = 1.2) +
  scale_color_manual(
    name = "Compartment",
    values = c("Plasma" = "#440154", "Placenta" = "#21908C", "Fetus" = "#FDE725"),
    breaks = c("Plasma", "Placenta", "Fetus")
  ) +
  labs(
    title = "PBPK Model: Antihypertensive Drug in Pregnancy",
    subtitle = "Drug Concentration in Key Compartments Over Time",
    x = "Time (hours)",
    y = "Concentration (mg/L)",
    color = "Compartment"
  ) +
  pbpk_theme +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Plot tissue concentrations (small multiples)
tissue_plot <- ggplot(out_df %>% 
                        select(time, Liver_conc, Fat_conc, Muscle_conc) %>%
                        pivot_longer(-time, names_to = "Tissue", values_to = "Concentration"), 
                      aes(x = time, y = Concentration, color = Tissue)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Tissue, scales = "free_y", ncol = 1,
             labeller = labeller(Tissue = c(
               "Liver_conc" = "Liver Concentration",
               "Fat_conc" = "Fat Concentration",
               "Muscle_conc" = "Muscle Concentration"
             ))) +
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  labs(x = "Time (hours)", y = "Concentration (mg/L)") +
  pbpk_theme +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# Combine plots
combined_plot <- ggarrange(
  main_plot, tissue_plot,
  ncol = 2, widths = c(2, 1),
  labels = c("A", "B")
)

# Annotate the combined plot
annotate_figure(
  combined_plot,
  top = text_grob("Physiologically-Based Pharmacokinetic Model for Pregnancy", 
                  color = "#2F4F4F", face = "bold", size = 18),
  bottom = text_grob("Simulation of antihypertensive drug distribution in pregnant woman\nAssumes 50mg oral dose with 80% bioavailability", 
                     color = "gray30", hjust = 0.5, size = 10)
)

ggsave("model_3_pharmacokinetics/pbpk_1.png", width = 12, height = 8, dpi = 300)



# Claude


# PBPK Model for Antihypertensive Medication in Pregnant Women
# This model simulates the distribution of labetalol (a common antihypertensive in pregnancy)
# across maternal and fetal compartments 

library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

# Parameters for the PBPK model
# Values based on literature for labetalol and physiological changes in pregnancy

# Physiological parameters for pregnant woman (third trimester)
BW <- 80           # Body weight (kg)
CO <- 7.0          # Cardiac output (L/min)
CO_L_hr <- CO * 60 # Cardiac output (L/hr)

# Tissue volumes (L) - adjusted for pregnancy
V_lungs <- 1.4     # Lungs
V_liver <- 1.8     # Liver (slightly enlarged in pregnancy)
V_kidneys <- 0.35  # Kidneys
V_placenta <- 0.65 # Placenta
V_fetus <- 3.5     # Fetus and amniotic fluid
V_uterus <- 1.2    # Uterus (enlarged)
V_plasma <- 3.9    # Plasma volume (increased in pregnancy)
V_rest <- BW*0.82 - (V_lungs + V_liver + V_kidneys + V_placenta + V_fetus + V_uterus + V_plasma) # Rest of body

# Regional blood flows (fraction of cardiac output)
Q_lungs <- 1.0     # Lungs receive total cardiac output
Q_liver <- 0.24    # Liver (slightly decreased in pregnancy)
Q_kidneys <- 0.19  # Kidneys (increased in pregnancy)
Q_placenta <- 0.11 # Placenta
Q_uterus <- 0.09   # Uterus (increased in pregnancy)
Q_rest <- 1 - (Q_liver + Q_kidneys + Q_placenta + Q_uterus) # Rest of body

# Convert fractional flows to absolute flows (L/hr)
F_lungs <- Q_lungs * CO_L_hr
F_liver <- Q_liver * CO_L_hr
F_kidneys <- Q_kidneys * CO_L_hr
F_placenta <- Q_placenta * CO_L_hr
F_uterus <- Q_uterus * CO_L_hr
F_rest <- Q_rest * CO_L_hr

# Drug-specific parameters for labetalol
# Partition coefficients (tissue:plasma)
PC_lungs <- 2.5
PC_liver <- 9.2
PC_kidneys <- 6.8
PC_placenta <- 3.1
PC_fetus <- 0.8   # Lower due to placental barrier
PC_uterus <- 2.0
PC_rest <- 1.5

# Clearance parameters
CL_hepatic <- 45   # Hepatic clearance (L/hr)
CL_renal <- 12     # Renal clearance (L/hr)
CL_placental <- 2  # Placental clearance (L/hr)

# Drug absorption and protein binding
F_oral <- 0.3      # Oral bioavailability (30%)
fu <- 0.45         # Fraction unbound in plasma (increased in pregnancy)
ka <- 0.8          # Absorption rate constant (1/hr)
Dose <- 200        # Oral dose (mg)

# PBPK model function
pbpk_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Drug amount in each compartment
    # A_gi: GI tract, A_plasma: plasma, A_lungs: lungs, A_liver: liver, 
    # A_kidneys: kidneys, A_placenta: placenta, A_fetus: fetus, A_uterus: uterus, A_rest: rest of body
    
    # Concentrations
    C_plasma <- A_plasma / V_plasma
    C_lungs <- A_lungs / V_lungs
    C_liver <- A_liver / V_liver
    C_kidneys <- A_kidneys / V_kidneys
    C_placenta <- A_placenta / V_placenta
    C_fetus <- A_fetus / V_fetus
    C_uterus <- A_uterus / V_uterus
    C_rest <- A_rest / V_rest
    
    # Free plasma concentration
    C_plasma_free <- C_plasma * fu
    
    # Differential equations
    dA_gi <- -ka * A_gi
    
    dA_plasma <- ka * A_gi + 
      F_lungs * (C_lungs/PC_lungs - C_plasma_free) + 
      F_liver * (C_liver/PC_liver - C_plasma_free) + 
      F_kidneys * (C_kidneys/PC_kidneys - C_plasma_free) + 
      F_placenta * (C_placenta/PC_placenta - C_plasma_free) + 
      F_uterus * (C_uterus/PC_uterus - C_plasma_free) + 
      F_rest * (C_rest/PC_rest - C_plasma_free) - 
      CL_renal * C_plasma_free
    
    dA_lungs <- F_lungs * (C_plasma_free - C_lungs/PC_lungs)
    
    dA_liver <- F_liver * (C_plasma_free - C_liver/PC_liver) - 
      CL_hepatic * C_liver/PC_liver
    
    dA_kidneys <- F_kidneys * (C_plasma_free - C_kidneys/PC_kidneys) - 
      CL_renal * C_plasma_free
    
    dA_placenta <- F_placenta * (C_plasma_free - C_placenta/PC_placenta) - 
      CL_placental * (C_placenta/PC_placenta - C_fetus/PC_fetus)
    
    dA_fetus <- CL_placental * (C_placenta/PC_placenta - C_fetus/PC_fetus)
    
    dA_uterus <- F_uterus * (C_plasma_free - C_uterus/PC_uterus)
    
    dA_rest <- F_rest * (C_plasma_free - C_rest/PC_rest)
    
    # Return the rate of change
    return(list(c(dA_gi, dA_plasma, dA_lungs, dA_liver, dA_kidneys, dA_placenta, dA_fetus, dA_uterus, dA_rest)))
  })
}

# Initial conditions: Drug only in GI tract initially
state <- c(A_gi = Dose * F_oral, 
           A_plasma = 0, 
           A_lungs = 0, 
           A_liver = 0, 
           A_kidneys = 0, 
           A_placenta = 0, 
           A_fetus = 0, 
           A_uterus = 0, 
           A_rest = 0)

# Parameters to pass to the ODE solver
parameters <- c(V_plasma = V_plasma, V_lungs = V_lungs, V_liver = V_liver, 
                V_kidneys = V_kidneys, V_placenta = V_placenta, V_fetus = V_fetus, 
                V_uterus = V_uterus, V_rest = V_rest,
                F_lungs = F_lungs, F_liver = F_liver, F_kidneys = F_kidneys, 
                F_placenta = F_placenta, F_uterus = F_uterus, F_rest = F_rest,
                PC_lungs = PC_lungs, PC_liver = PC_liver, PC_kidneys = PC_kidneys, 
                PC_placenta = PC_placenta, PC_fetus = PC_fetus, PC_uterus = PC_uterus, 
                PC_rest = PC_rest, 
                CL_hepatic = CL_hepatic, CL_renal = CL_renal, CL_placental = CL_placental, 
                fu = fu, ka = ka)

# Time points for simulation (0 to 24 hours)
times <- seq(0, 24, by = 0.1)

# Solve the ODE system
out <- ode(y = state, times = times, func = pbpk_model, parms = parameters)

# Convert to data frame for plotting
results <- as.data.frame(out)
colnames(results) <- c("time", "A_gi", "A_plasma", "A_lungs", "A_liver", "A_kidneys", 
                       "A_placenta", "A_fetus", "A_uterus", "A_rest")

# Calculate concentrations
results$C_plasma <- results$A_plasma / V_plasma
results$C_fetus <- results$A_fetus / V_fetus
results$C_placenta <- results$A_placenta / V_placenta
results$C_uterus <- results$A_uterus / V_uterus

# Reshape data for plotting
plot_data <- results %>%
  select(time, C_plasma, C_fetus, C_placenta, C_uterus) %>%
  pivot_longer(cols = -time, names_to = "compartment", values_to = "concentration") %>%
  mutate(compartment = factor(compartment, 
                              levels = c("C_plasma", "C_placenta", "C_fetus", "C_uterus"),
                              labels = c("Maternal Plasma", "Placenta", "Fetus", "Uterus")))

# Define a custom color palette
custom_colors <- c("Maternal Plasma" = "#E64B35", 
                   "Placenta" = "#4DBBD5", 
                   "Fetus" = "#00A087", 
                   "Uterus" = "#F39B7F")

# Create a plot
p <- ggplot(plot_data, aes(x = time, y = concentration, color = compartment, linetype = compartment)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
  labs(title = "PBPK Model: Antihypertensive Drug Distribution",
       subtitle = "Labetalol (200 mg oral dose) in Pregnant Woman - Third Trimester",
       x = "Time (hours)",
       y = "Concentration (mg/L)",
       color = "Compartment",
       linetype = "Compartment") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  annotate("text", x = 20, y = max(plot_data$concentration) * 0.95, 
           label = "Maternal exposure higher than fetal", 
           fontface = "italic", size = 4, hjust = 1)

# Print the plot
print(p)

# Optional: Generate a second plot showing the ratio of fetal to maternal concentration over time
ratio_data <- results %>%
  mutate(fetal_maternal_ratio = C_fetus / C_plasma) %>%
  select(time, fetal_maternal_ratio)

p2 <- ggplot(ratio_data, aes(x = time, y = fetal_maternal_ratio)) +
  geom_line(size = 1.2, color = "#3B9AB2") +
  labs(title = "Fetal-to-Maternal Concentration Ratio",
       subtitle = "Labetalol Transfer Across Placenta",
       x = "Time (hours)",
       y = "Fetal/Maternal Concentration Ratio") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 20, y = 0.52, 
           label = "Safety threshold for many drugs", 
           fontface = "italic", size = 4, hjust = 1)

# Print the second plot
print(p2)

# combi Plot

p / p2

ggsave("model_3_pharmacokinetics/pbpk_2.png", width = 8, height = 12, dpi = 300)



#  ChatGPT

# Step 1: Install and Load Required Packages

# Install packages if not already installed
#install.packages("rPBK")
#install.packages("ggplot2")

# Load packages
library(rPBK)
library(ggplot2)

# Step 2: Define Model Parameters

# Define base parameters
params <- list(
  MW = 300,        # Molecular weight
  logP = 2.5,      # Lipophilicity
  fu = 0.1,        # Fraction unbound
  CL = 15,         # Clearance (L/h)
  Vd = 50          # Volume of distribution (L)
)

# Adjust parameters for pregnancy
params_pregnant <- params
params_pregnant$CL <- params$CL * 1.5    # Increase clearance by 50%
params_pregnant$Vd <- params$Vd * 1.2    # Increase Vd by 20%

# Step 3: Simulate Concentration-Time Profiles

# Define time points
time <- seq(0, 24, by = 0.1)  # 0 to 24 hours in 0.1-hour increments

# Function to simulate concentration-time profile
simulate_concentration <- function(CL, Vd, dose, time) {
  k_elim <- CL / Vd
  conc <- (dose / Vd) * exp(-k_elim * time)
  return(conc)
}

# Simulate for non-pregnant
conc_non_pregnant <- simulate_concentration(
  CL = params$CL,
  Vd = params$Vd,
  dose = 100,
  time = time
)

# Simulate for pregnant
conc_pregnant <- simulate_concentration(
  CL = params_pregnant$CL,
  Vd = params_pregnant$Vd,
  dose = 100,
  time = time
)

# Combine data into a data frame
data <- data.frame(
  Time = rep(time, 2),
  Concentration = c(conc_non_pregnant, conc_pregnant),
  State = rep(c("Non-Pregnant", "Pregnant"), each = length(time))
)

# Step 4: Visualize with ggplot2

# Create the plot
ggplot(data, aes(x = Time, y = Concentration, color = State)) +
  geom_line(size = 1.2) +
  labs(
    title = "Plasma Concentration-Time Profile of Antihypertensive Drug",
    subtitle = "Comparison between Non-Pregnant and Pregnant States",
    x = "Time (hours)",
    y = "Plasma Concentration (mg/L)",
    color = "Physiological State"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Non-Pregnant" = "#1f77b4", "Pregnant" = "#ff7f0e")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("model_3_pharmacokinetics/pbpk_3.png", width = 8, height = 6, dpi = 300)

