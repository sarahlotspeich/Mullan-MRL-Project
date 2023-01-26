# //////////////////////////////////////////////////////////////////////
# Simulation setting from Table 2 //////////////////////////////////////
# Censoring in a Weibull covariate X ///////////////////////////////////
# Imputed using estimated survival function S(X|Z) /////////////////////
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
q = ifelse(censoring == "light", 0.4, 
           ifelse(censoring == "moderate", 2.9, 20)) # Rate parameter for censoring
c = rexp(n = n, rate = q) # Random censoring mechanism
w <- pmin(x, c) # Observed covariate value
d <- as.numeric(x <= c) # "Event" indicator
dat = data.frame(z, w, y, d) # Construct dataset

# Check % censored
1 - mean(dat$d) # 12% 

# Install/load packages 
## RUN ONCE: install.packages("devtools")
## RUN ONCE: devtools::install_github("sarahlotspeich/imputeCensRd", ref = "main")
library(imputeCensRd)

# Imputation approach 1: Calculate conditional means with trapezoidal rule
## Breslow's estimator is carry-forward interpolated (surv_between = "cf)
## and extrapolated using the Weibull extension (surv_beyond = "w")
trap_rule_imp = cmi_sp(W = "w", Delta = "d", Z = "z", data = dat, trapezoidal_rule = TRUE,
                       Xmax = Inf, surv_between = "cf", surv_beyond = "w")
trap_rule_imp$code # If TRUE, imputation was successful 
trap_rule_fit <- lm(y ~ imp + z, data = trap_rule_imp$imputed_data) # Fit analysis model to imputed data

# Imputation approach 2: Calculate conditional means with adaptive quadrature
## Breslow's estimator is carry-forward interpolated (surv_between = "cf)
## and extrapolated using the Weibull extension (surv_beyond = "w")
adapt_quad_imp = cmi_sp(W = "w", Delta = "d", Z = "z", data = dat, trapezoidal_rule = FALSE, 
                        Xmax = Inf, surv_between = "cf", surv_beyond = "w")
adapt_quad_imp$code # If TRUE, imputation was successful 
adapt_quad_fit <- lm(y ~ imp + z, data = adapt_quad_imp$imputed_data) # Fit analysis model to imputed data

# Compare models 
trap_rule_fit
adapt_quad_fit
