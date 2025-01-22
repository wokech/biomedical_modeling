# Predator-Prey (Perplexity)

# Load necessary library
library(ggplot2)

# Parameters
alpha <- 0.1  # Prey growth rate
beta <- 0.02  # Predation rate
gamma <- 0.1  # Predator death rate
delta <- 0.01 # Predator growth rate per prey eaten

# Time parameters
time_steps <- 200
dt <- 1

# Initialize populations
prey <- numeric(time_steps)
predators <- numeric(time_steps)
prey[1] <- 40   # Initial number of prey
predators[1] <- 9 # Initial number of predators

# Simulation loop
for (t in 2:time_steps) {
  prey[t] <- prey[t - 1] + (alpha * prey[t - 1] - beta * prey[t - 1] * predators[t - 1]) * dt
  predators[t] <- predators[t - 1] + (delta * prey[t - 1] * predators[t - 1] - gamma * predators[t - 1]) * dt
  
  # Ensure populations do not go negative
  if (prey[t] < 0) prey[t] <- 0
  if (predators[t] < 0) predators[t] <- 0
}

# Create a data frame for plotting
data <- data.frame(Time = seq(1, time_steps), Prey = prey, Predators = predators)

# Plot results
ggplot(data, aes(x = Time)) +
  geom_line(aes(y = Prey, color = "Prey")) +
  geom_line(aes(y = Predators, color = "Predators")) +
  labs(title = "Predator-Prey Dynamics", x = "Time", y = "Population") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()
