library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
timesteps <- 100
dt <- 1
N0 <- 1e6           # Initial tumor cells
K <- 1e9            # Carrying capacity
growth_rate <- 0.03 # Tumor cell proliferation rate
natural_death <- 0.005 # Baseline cell death

# Chemotherapy parameters for two drugs
chemo <- data.frame(
  name = c("DrugA", "DrugB"),
  efficacy = c(0.04, 0.02),   # Fractional kill per unit concentration
  clearance = c(0.1, 0.05),   # Drug clearance rate
  dose = c(1, 2),             # Dose per administration
  admin_interval = c(14, 7)   # Days between doses
)

# Initialize storage
results <- data.frame(
  time = 0:timesteps,
  TumorCells = NA,
  DrugA = NA,
  DrugB = NA
)
results$TumorCells[1] <- N0
results$DrugA[1] <- 0
results$DrugB[1] <- 0

# Simulation loop
for (t in 1:timesteps) {
  N_prev <- results$TumorCells[t]
  # Drug concentrations
  DrugA_prev <- results$DrugA[t]
  DrugB_prev <- results$DrugB[t]
  
  # Administer drugs at intervals
  DrugA_new <- DrugA_prev * exp(-chemo$clearance[1] * dt)
  DrugB_new <- DrugB_prev * exp(-chemo$clearance[2] * dt)
  if (t %% chemo$admin_interval[1] == 0) DrugA_new <- DrugA_new + chemo$dose[1]
  if (t %% chemo$admin_interval[2] == 0) DrugB_new <- DrugB_new + chemo$dose[2]
  
  # Effective kill rate
  kill_A <- chemo$efficacy[1] * DrugA_new
  kill_B <- chemo$efficacy[2] * DrugB_new
  total_kill <- kill_A + kill_B
  
  # Logistic growth with treatment and death
  dN <- (growth_rate * N_prev * (1 - N_prev / K) - (natural_death + total_kill) * N_prev) * dt
  N_new <- max(N_prev + dN, 0)
  
  # Store
  results$TumorCells[t+1] <- N_new
  results$DrugA[t+1] <- DrugA_new
  results$DrugB[t+1] <- DrugB_new
}

# Prepare for plotting
plot_data <- results %>%
  select(time, TumorCells, DrugA, DrugB) %>%
  pivot_longer(-time, names_to = "Variable", values_to = "Value")

# Plot
ggplot(plot_data, aes(x = time, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  scale_y_log10() +
  labs(title = "Multiscale Tumor Growth and Chemotherapy Response",
       x = "Time (days)",
       y = "Tumor Cells / Drug Concentration (log scale)",
       color = "Variable") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


ggsave("images/modeling_tumor_growth_chemo_1.png", height = 6, width = 6, dpi = 300)
