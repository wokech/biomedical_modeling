# Bacterial Growth Model

# Claude

# R Implementation
install.packages("deSolve")
library(deSolve)

# Define the growth model
bacterial_growth <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N  # dN/dt = r*N
    list(dN)
  })
}

# Set parameters
parameters <- c(r = 0.5)  # growth rate per hour
state <- c(N = 1000)      # initial population
times <- seq(0, 10, by = 0.1)

# Solve ODE
out <- ode(y = state, 
           times = times, 
           func = bacterial_growth, 
           parms = parameters)

# Convert to data frame
result_df <- as.data.frame(out)

# Create plot
plot(result_df$time, result_df$N,
     type = "l",
     col = "blue",
     xlab = "Time (hours)",
     ylab = "Number of Bacteria",
     main = "Bacterial Growth Model")
grid()

# Calculate doubling time
doubling_time <- log(2) / parameters["r"]
print(paste("Population doubling time:", round(doubling_time, 2), "hours"))

# ChatGPT

# Simulating Exponential Growth (Bacterial Growth Model)

# Load necessary library
library(ggplot2)

# Function to simulate exponential growth
simulate_growth <- function(N0, r, t_max) {
  # Time sequence
  time <- seq(0, t_max, by = 1)
  
  # Calculate population at each time point
  population <- N0 * exp(r * time)
  
  # Create a data frame for plotting
  data <- data.frame(Time = time, Population = population)
  
  # Plot the results
  ggplot(data, aes(x = Time, y = Population)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Exponential Growth of Bacteria",
         x = "Time (hours)",
         y = "Population") +
    theme_minimal()
}

# Example usage
simulate_growth(N0 = 100, r = 0.5, t_max = 24)

# Gemini

