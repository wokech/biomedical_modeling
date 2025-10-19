# SIR models in R

# https://rpubs.com/choisy/sir

# A) Solving differential equations in R

install.packages("deSolve")

library(deSolve) # using the "ode" function

# Step 1: writing the differential equations in R

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Step 2: defining some values for the parameters

parameters_values <- c(
  beta  = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5    # recovery rate (/day)
)

# Step 3: defining initial values for the variables

initial_values <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

# Step 4: the points in time where to calculate variables values

time_values <- seq(0, 10) # days

# Step 5: numerically solving the SIR model

ls()

sir_equations

parameters_values

initial_values

time_values

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1

sir_values_1 <- as.data.frame(sir_values_1)
sir_values_1

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

# Value of R0

(999 + 1) * parameters_values["beta"] / parameters_values["gamma"]

# Exercises

# Writing a simulator

sir_1 <- function(beta, gamma, S0, I0, R0, times) {
  require(deSolve) # for the "ode" function
  
  # the differential equations:
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  # the parameters values:
  parameters_values <- c(beta  = beta, gamma = gamma)
  
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values)
  
  # returning the output:
  as.data.frame(out)
}

sir_1(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10))

# Comparing a model’s predictions with data

flu <- read.table("https://bit.ly/2vDqAYN", header = TRUE)

flu

with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
predictions <- sir_1(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
with(predictions, lines(time, I, col = "red"))


model_fit <- function(beta, gamma, data, N = 763, ...) {
  I0 <- data$cases[1] # initial number of infected (from data)
  times <- data$day   # time points (from data)
  # model's predictions:
  predictions <- sir_1(beta = beta, gamma = gamma,   # parameters
                       S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                       times = times)                # time points
  # plotting the observed prevalences:
  with(data, plot(day, cases, ...))
  # adding the model-predicted prevalence:
  with(predictions, lines(time, I, col = "red"))
}


model_fit(beta = 0.004, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))


# Estimating model’s parameters

# Sums of squares

predictions <- sir_1(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
predictions

flu

sum((predictions$I - flu$cases)^2)

# the observed prevalences:
with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
# the model-predicted prevalences:
with(predictions, lines(time, I, col = "red", type = "o"))
# the "errors":
segments(flu$day, flu$cases, predictions$time, predictions$I)


# Exercises

# Sum of squares profile

ss <- function(beta, gamma, data = flu, N = 763) {
  I0 <- data$cases[1]
  times <- data$day
  predictions <- sir_1(beta = beta, gamma = gamma,   # parameters
                       S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                       times = times)                # time points
  sum((predictions$I[-1] - data$cases[-1])^2)
}

ss(beta = 0.004, gamma = 0.5)

beta_val <- seq(from = 0.0016, to = 0.004, le = 100)
ss_val <- sapply(beta_val, ss, gamma = 0.5)

min_ss_val <- min(ss_val)
min_ss_val

beta_hat <- beta_val[ss_val == min_ss_val]
beta_hat

plot(beta_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("infectious contact rate ", beta)),
     ylab = "sum of squares")
# adding the minimal value of the sum of squares:
abline(h = min_ss_val, lty = 2, col = "grey")
# adding the estimate of beta:
abline(v = beta_hat, lty = 2, col = "grey")

gamma_val <- seq(from = 0.4, to = 0.575, le = 100)
ss_val <- sapply(gamma_val, function(x) ss(beta_hat, x))
(min_ss_val <- min(ss_val))


(gamma_hat <- gamma_val[ss_val == min_ss_val])



plot(gamma_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("recovery rate ", gamma)),
     ylab = "sum of squares")
abline(h = min_ss_val, lty = 2, col = "grey")
abline(v = gamma_hat, lty = 2, col = "grey")


n <- 10 # number of parameter values to try
beta_val <- seq(from = 0.002, to = 0.0035, le = n)
gamma_val <- seq(from = 0.3, to = 0.65, le = n)
param_val <- expand.grid(beta_val, gamma_val)
ss_val <- with(param_val, Map(ss, Var1, Var2))
ss_val <- matrix(unlist(ss_val), n)
persp(beta_val, gamma_val, -ss_val, theta = 40, phi = 30,
      xlab = "beta", ylab = "gamma", zlab = "-sum of squares")



n <- 30 # number of parameters values
beta_val <- seq(from = 0.002, to = 0.0035, le = n)
gamma_val <- seq(from = 0.3, to = 0.65, le = n)
# calculating the sum of squares:
param_val <- expand.grid(beta_val, gamma_val)
ss_val <- with(param_val, Map(ss, Var1, Var2))
ss_val <- unlist(ss_val)

# minimum sum of squares and parameters values:
(ss_min <- min(ss_val))


ind <- ss_val == ss_min
(beta_hat <- param_val$Var1[ind])


(gamma_hat <- param_val$Var2[ind])


# visualizing the sum of squares profile:
ss_val <- matrix(ss_val, n)
image(beta_val, gamma_val, ss_val,
      xlab = expression(paste("infectious contact rate ", beta, " (/person/day)")),
      ylab = expression(paste("recovery rate ", gamma, " (/day)")))
contour(beta_val, gamma_val,ss_val, add = TRUE)
points(beta_hat, gamma_hat, pch = 3)
box(bty = "o")


# Optimisation

# The aim here is to estimate the parameters values more efficiently, 
# with an “intelligent” algorithm instead of exploring many possible 
# values (at random or “exhaustively”). This can be done with the function optim().

ss(beta = 0.004, gamma = 0.5)

ss2 <- function(x) {
  ss(beta = x[1], gamma = x[2])
}

starting_param_val <- c(0.004, 0.5)
ss_optim <- optim(starting_param_val, ss2)

ss_optim

ss_optim$value

ss_optim$par

# Maximum likelihood estimation with the bbmle package

mLL <- function(beta, gamma, sigma, day, cases, N = 763) {
  beta <- exp(beta) # to make sure that the parameters are positive
  gamma <- exp(gamma)
  sigma <- exp(sigma)
  I0 <- cases[1] # initial number of infectious
  observations <- cases[-1] # the fit is done on the other data points
  predictions <- sir_1(beta = beta, gamma = gamma,
                       S0 = N - I0, I0 = I0, R0 = 0, times = day)
  predictions <- predictions$I[-1] # removing the first point too
  # returning minus log-likelihood:
  -sum(dnorm(x = observations, mean = predictions, sd = sigma, log = TRUE))
}

library(bbmle) # for "mle2", "coef", "confint", "vcov", "logLik", "profile", "summary", "plot.profile.mle2"

starting_param_val <- list(beta = 0.004, gamma = 0.5, sigma = 1)
estimates <- mle2(minuslogl = mLL, start = lapply(starting_param_val, log),
                  method = "Nelder-Mead", data = c(flu, N = 763))

summary(estimates)

exp(coef(estimates))

exp(confint(estimates))

vcov(estimates)

-logLik(estimates)

prof <- profile(estimates)
plot(prof, main = NA)



N <- 763 # total population size
time_points <- seq(min(flu$day), max(flu$day), le = 100) # vector of time points
I0 <- flu$cases[1] # initial number of infected
param_hat <- exp(coef(estimates)) # parameters estimates
# model's best predictions:
best_predictions <- sir_1(beta = param_hat["beta"], gamma = param_hat["gamma"],
                          S0 = N - I0, I0 = I0, R0 = 0, time_points)$I
# confidence interval of the best predictions:
cl <- 0.95 # confidence level
cl <- (1 - cl) / 2
lwr <- qnorm(p = cl, mean = best_predictions, sd = param_hat["sigma"])
upr <- qnorm(p = 1 - cl, mean = best_predictions, sd = param_hat["sigma"])
# layout of the plot:
plot(time_points, time_points, ylim = c(0, max(upr)), type = "n",
     xlab = "time (days)", ylab = "prevalence")
# adding the predictions' confidence interval:
sel <- time_points >= 1 # predictions start from the second data point
polygon(c(time_points[sel], rev(time_points[sel])), c(upr[sel], rev(lwr[sel])),
        border = NA, col = adjustcolor("red", alpha.f = 0.1))
# adding the model's best predictions:
lines(time_points, best_predictions, col = "red")
# adding the observed data:
with(flu, points(day, cases, pch = 19, col = "red"))


# Poisson distribution of errors

#mLL <- function(beta, gamma, sigma, day, cases, N = 763) {
mLL_pois <- function(beta, gamma, day, cases, N = 763) {
  beta <- exp(beta) # to make sure that the parameters are positive
  gamma <- exp(gamma)
  #  sigma <- exp(sigma)
  I0 <- cases[1] # initial number of infectious
  observations <- cases[-1] # the fit is done on the other data points
  predictions <- sir_1(beta = beta, gamma = gamma,
                       S0 = N - I0, I0 = I0, R0 = 0, times = day)
  predictions <- predictions$I[-1] # removing the first point too
  if (any(predictions < 0)) return(NA) # safety
  # returning minus log-likelihood:
  #  -sum(dnorm(x = observations, mean = predictions, sd = sigma, log = TRUE))
  -sum(dpois(x = observations, lambda = predictions, log = TRUE))
}


starting_param_val <- list(beta = 0.004, gamma = 0.5)
estimates_pois <- mle2(minuslogl = mLL_pois,
                       start = lapply(starting_param_val, log),
                       data = c(flu, N = 763))


exp(coef(estimates))


exp(coef(estimates_pois))


exp(confint(estimates))


exp(confint(estimates_pois))


vcov(estimates_pois)



# points estimates:
param_hat <- exp(coef(estimates_pois))
# model's best predictions:
best_predictions <- sir_1(beta = param_hat["beta"], gamma = param_hat["gamma"],
                          S0 = N - I0, I0 = I0, R0 = 0, time_points)$I
# confidence interval of the best predictions:
cl <- 0.95 # confidence level
cl <- (1 - cl) / 2
lwr <- qpois(p = cl, lambda = best_predictions)
upr <- qpois(p = 1 - cl, lambda = best_predictions)
# layout of the plot:
plot(time_points, time_points, ylim = c(0, max(upr)), type = "n",
     xlab = "time (days)", ylab = "prevalence")
# adding the predictions' confidence interval:
sel <- time_points >= 1 # predictions start from the second data point
polygon(c(time_points[sel], rev(time_points[sel])), c(upr[sel], rev(lwr[sel])),
        border = NA, col = adjustcolor("red", alpha.f = 0.1))
# adding the model's best predictions:
lines(time_points, best_predictions, col = "red")
# adding the observed data:
with(flu, points(day, cases, pch = 19, col = "red"))



-logLik(estimates)



-logLik(estimates_pois)
