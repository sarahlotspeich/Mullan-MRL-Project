# //////////////////////////////////////////////////////////////////////
# Simulation setting from Table 1 //////////////////////////////////////
# Censoring in a Weibull covariate X ///////////////////////////////////
# Imputed using "gold standard" with true survival function S(X|Z) /////
# //////////////////////////////////////////////////////////////////////

# For reproducibility 
set.seed(114) 

# Specify simulation setting 
censoring = "light" # censoring rate, other options included "moderate" or "heavy
n = 100 # sample size, other options included 500, 1000, or 2000

# Generate data 
z = rbinom(n = n, size = 1, prob = 0.5) # Uncensored covariate
x = rweibull(n = n, shape = 0.75, scale = 0.25) # To-be-censored covariate
e = rnorm(n = n, mean = 0, sd = 1) # Random errors
y = 1 + 0.5 * x + 0.25 * z + e # Continuous outcome
q = ifelse(censoring == "light", 2, 
           ifelse(censoring == "moderate", 0.35, 0.05)) # Rate parameter for censoring
c = rweibull(n = n, shape = 1, scale = q) # Random censoring mechanism
w <- pmin(x, c) # Observed covariate value
d <- as.numeric(x <= c) # "Event" indicator
dat = data.frame(z, w, y, d) # Construct dataset

# Check % censored
1 - mean(dat$d) # 12% 

# Write a function for the true survival function used to generate Weibull X 
trueSURV = function(q, z) {
  pweibull(q = q, shape = 0.75, scale = 0.25, lower.tail = FALSE)
}

# Install/load packages 
## RUN ONCE: install.packages("devtools")
## RUN ONCE: devtools::install_github("sarahlotspeich/imputeCensRd", ref = "main")
library(imputeCensRd)

# Imputation approach 1: Calculate conditional means with trapezoidal rule
trap_rule_imp = cmi_custom(W = "w", Delta = "d", Z = "z", data = dat, useSURV = trueSURV, trapezoidal_rule = TRUE)
trap_rule_imp$code # If TRUE, imputation was successful 
trap_rule_fit <- lm(y ~ imp + z, data = trap_rule_imp$imputed_data) # Fit analysis model to imputed data

# Imputation approach 2: Calculate conditional means with adaptive quadrature
adapt_quad_imp = cmi_custom(W = "w", Delta = "d", Z = "z", data = dat, useSURV = trueSURV, trapezoidal_rule = FALSE)
adapt_quad_imp$code # If TRUE, imputation was successful 
adapt_quad_fit <- lm(y ~ imp + z, data = adapt_quad_imp$imputed_data) # Fit analysis model to imputed data

# Compare models 
trap_rule_fit
adapt_quad_fit
