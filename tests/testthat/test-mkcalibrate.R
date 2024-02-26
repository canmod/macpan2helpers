## no explicit testthat tests yet ...

# library(macpan2helpers)
# library(testthat)
# library(dplyr)
# setup_sim <- function() {
# m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
# sim <- m$simulators$tmb(
#     time_steps = 100,
#     state = c(S = 99, I = 1, R = 0),
#     flow = c(foi = NA, gamma = 0.1),
#     beta = 0.2,
#     N = empty_matrix
#     )
# }
# 
# if (require(outbreaks)) {
#      I_obs <- influenza_england_1978_school[["in_bed"]]
# } else {
#   set.seed(101)
#    I_obs <- (sim$report(.phases = "during")
#       |> filter(row == "I")
#      |> mutate(obs = rnbinom(100, mu = value, size = 2))
#      |> pull(obs)
#    )
# }
# sim <- setup_sim()
# m1 <- mk_calibrate(sim,
#              data = data.frame(I_obs),
#              params = list(beta = 0.2, I_sd = 1),
#              transforms = list(beta = "log", I_sd = "log"),
#              exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
#              )
# cat(m1, sep = "\n")
# 
# 
# ## time explicitly included
# sim <- setup_sim()
# dd <- data.frame(time = 1:length(I_obs), I_obs)
# mk_calibrate(sim,
#              data = dd,
#              params = list(beta = 0.2, I_sd = 1),
#              transforms = list(beta = "log", I_sd = "log"),
#              exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
#              )
# 
# sim <- setup_sim()
# dd <- data.frame(time = 1:length(I_obs), I_obs)
# dd <- dd[-2,]
# m1 <- mk_calibrate(sim,
#              data = dd,
#              params = list(beta = 0.2, I_sd = 1),
#              transforms = list(beta = "log", I_sd = "log"),
#              exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
#              )
# 
# sim$report()
# sim$optimize$nlminb()
