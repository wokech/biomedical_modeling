# Pharmacokinetics (One-Compartment Model, Intravenous Bolus) 

# Gemini

# Parameters
C0 <- 100 # Initial concentration (mg/L)
k <- 0.2  # Elimination rate constant (1/hour)
time <- seq(0, 24, by = 0.5) # Time vector from 0 to 24 hours in 0.5-hour intervals

# Calculate concentration over time
C <- C0 * exp(-k * time)

# Plot the results
plot(time, C, type = "l", xlab = "Time (hours)", ylab = "Concentration (mg/L)",
     main = "One-Compartment PK Model (IV Bolus)", col = "blue", lwd = 2)
grid() # Add a grid for better readability

# Print some key values
cat("Concentration at t=0:", C[1], "mg/L\n")
cat("Concentration at t=4:", C[9], "mg/L\n") #index 9 corresponds to time = 4

# Google AI Studio

# Define model parameters
D <- 100    # Dose (mg)
Vd <- 10    # Volume of distribution (L)
kel <- 0.2  # Elimination rate constant (1/hour)
t_max <- 20  # Maximum simulation time (hours)

# Create a time vector
time <- seq(from = 0, to = t_max, length.out = 100)

# Calculate concentration over time using the model equation
concentration <- (D / Vd) * exp(-kel * time)

# Plot the results
plot(time, concentration, type = "l",
     xlab = "Time (hours)",
     ylab = "Concentration (mg/L)",
     main = "One-Compartment PK Model",
     grid = TRUE)

