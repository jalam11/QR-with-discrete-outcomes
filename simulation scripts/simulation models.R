
# For creating 90% Confidence intervals. This can be adjusted if you want to create 95% confidence intervals
z <- qnorm(0.95); z_lower <- 0.05; z_upper <- 0.95; level = 0.9 

## x2
prob_x2 <- 0.2 # probability that the confounder x2 equals 1

## x1
### binary
gamma <- 0.1 # effect of x2 on x1
prob_x1_binary <- 0.5 - (gamma * prob_x2) # probability that binary x1=1 should be 50% after accounting for confounders

### Continuous
mu_x1 <- 0 # mean of continuous x1
sigma_x1 <- 1 # variance of continuous x1

### Categorical
prob_x1_cat <- c(0.25, 0.25, 0.25, 0.25) # Simulates people having equall probability of falling into each category

# But people with lower SES (which is simulated with x2) should have higher probability of having higher exposure
prob_x1_cat_lowSES <- prob_x1_cat + c(-gamma, 
                                      -gamma * 0.5, 
                                      gamma * 0.5, 
                                      gamma) 

# And people with higher SES (which is simulated with x2) should have higher probability of having lower exposure
## If prob_x2 = 0.5, then these two sets of probabilities will be the same. 
## But because this is not the case, we need to decrease the effect of being low SES on x1 catergory to allow the
## Probability of falling into each of the four categories to be 0.25 overall
prob_x1_cat_highSES <- prob_x1_cat + c(gamma * prob_x2, 
                                       gamma * 0.5 * prob_x2, 
                                       -gamma * 0.5 * prob_x2, 
                                       -gamma* prob_x2)

# Make function for this
make_cat_x1 <- function(n, x2 = x2, 
                        prob_lowSES = prob_x1_cat_lowSES, 
                        prob_highSES = prob_x1_cat_highSES) {
  prob = ifelse(x2 == 1, # If x2 equals 1
                prob_x1_cat_lowSES, # use x1 probabilities for low SES people
                prob_x1_cat_highSES) # otherwise, use x1 probabilities for high SES people
  
  # make categorical x1
  x1 <- sample(c(0, 1, 2, 3), size = n, replace = T)
  return(x1)
}


## y parameters
beta0 <- 44 # true y value when x1 and x2 equall 0
beta1 <- 0.5 # true effect of x1 on y
beta2 <- 2.5 # true effect of x2 on y

mu_u <- 0 # mean model error value
sigma_u <- 1 # variance of model errors
tdist_df <- 4 # Degrees of freedom for the t distribution
alpha_y <- 5 # model error value is multiplied by this

## misc parameters
boot <- 1000 # Number of xy bootstraps selected
ndraw <- 10000 # Number of MCMC draws for Bayesian QR
burnin <- 2000 # Initial number of MCMC draw that are 'burned' or discarded

# Parameters that were adjusted in the simulation experiment
n <- 100 # Sample sizes considered in this paper include 100, 250, 500, 750. You can edit this to whatever you like

alpha_x1 <- 0.5 # Setting this to 0.5 results in heteroscedastic errors. Set to 0 for homoscedastic errors (associations are constant across all taus).Set to >0.5 to model even stronger heteroscedasticity. 

# Covariates
x2 <- rbinom(n, 1, prob_x2) # create a confounding variable that is associated with x and y

x1_binary <- rbinom(n, 1, prob_x1_binary + gamma*x2) # binary x1 for models 1, 2, 3, that depends on x2

x1_cnts <- rnorm(n, mu_x1 + gamma*x2, sigma_x1) # Continuous x1 for models 4 and 5, that depends on x2

x1_discrete <- make_cat_x1(n = n, x2 = x2) # create a discrete variable with values ranging from 0:3, that depends on x2

# Model errors
e_normal <- rnorm(n, mu_u, sigma_u) # Normally distributed errors 
e_heavy_tailed <- rt(n, df = tdist_df, mu_u) # heavy-tailed (t distribution with df=4) model errors for models 3 and 5

# Discrete outcome variable
## For this version I used a a binary x1 and a normally distributed error distribution (e_normal). Feel free to explore with different values. 
x1 <- x1_binary
e <- e_normal
y <- round(beta0 + beta1*x1 + beta2*x2 + e*(alpha_y + alpha_x1*x1), digits=0)      


