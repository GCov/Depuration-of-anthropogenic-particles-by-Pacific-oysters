M1 <- 
  glmmTMB(sum ~ sampleday*dry.weight - 1 + (1 | Table/Tank),
          family = poisson(link = 'log'),
          data = totalsums2)

library(R2jags)
library(coda)
library(lattice)

totalsums2$tank <- as.character(with(totalsums2, Table:Tank))
totalsums2$tank <- as.factor(totalsums2$tank)

model1 <-
  function() {
    for (i in 1:N) {
      y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- 
        alpha[sampleday[i]] + 
        beta[sampleday[i]] * dry.weight[i] +
        gamma_table[table[i]] +
        gamma_tank[tank[i]]
    }
    for (j in 1:Ntable) {
      gamma_table[j] ~ dnorm(mu_table, tau_table)
    }
    for (k in 1:Ntank) {
      gamma_tank[k] ~ dnorm(mu_tank, tau_tank)
    }
    for (l in 1:Nsampleday) {
      alpha[l] ~ dnorm(0, 1)
      beta[l] ~ dnorm(0, 1)
    }
    mu_table ~ dnorm(0, 1)
    sigma_table ~ dexp(1)
    tau_table <- 1/(sigma_table*sigma_table)
    mu_tank ~ dnorm(0, 1)
    sigma_tank ~ dexp(1)
    tau_tank <- 1/(sigma_tank * sigma_tank)
  }

init1 <- function() {
  list("alpha" = rnorm(length(unique(totalsums2$sampleday))),
       "beta" = rnorm(length(unique(totalsums2$sampleday))),
       "mu_table" = rnorm(1),
       "sigma_table" = 1,
       "mu_tank" = rnorm(1),
       "sigma_tank" = 1)
  }

params1 <- c("alpha", "beta")

data1 <- list(y = totalsums2$sum,
              N = nrow(totalsums2),
              Nsampleday = length(unique(totalsums2$sampleday)),
              Ntable = length(unique(totalsums2$Table)),
              Ntank = length(unique(totalsums2$tank)),
              sampleday = as.integer(totalsums2$sampleday),
              dry.weight = as.numeric(scale(totalsums2$dry.weight,
                                            center = TRUE)),
              table = as.integer(totalsums2$Table),
              tank = as.integer(totalsums2$tank))

run1 <- jags(
  data = data1,
  inits = init1,
  parameters.to.save = params1,
  n.chains = 3,
  n.iter = 10000,
  n.burnin = 1000,
  n.thin = 1,
  jags.seed = 5196,
  model = model1
)

run1
run1mcmc <- as.mcmc(run1)
xyplot(run1mcmc)
plot(run1mcmc)
