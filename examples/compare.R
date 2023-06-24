
## from Denis Rapoport

## Bjornstad

library(deSolve)
library(tidyverse)

sirmod = function(t, y, parameters) {
  # Pull state variables from y vector
  S = y[1]
  I = y[2]
  R = y[3]
  # Pull parameter values from the input vector
  beta = parameters["beta"]
  mu = parameters["mu"]
  gamma = parameters["gamma"]
  N = parameters["N"]
  # Define equations
  dS = - beta * S * I/N
  dI = beta * S * I/N - gamma * I
  dR = gamma * I
  res = c(dS, dI, dR)
  # Return list of gradients
  list(res)
}

times = seq(0, 101, by = 1)
paras = c(mu = 0, N = 100, gamma = 0.1)
paras["beta"] = 0.2
start = c(S = 0.999, I = 0.001, R = 0) * paras["N"]

deSolve_results = ode(y = start, times = times, func = sirmod, parms = paras,
                      method = "euler")
## out = as_tibble(out)
## bjornstad_results = out |>
##   pivot_longer(c(S, I, R), names_to = "row", values_to = "value")


# macpan2

library(macpan2)

sir_dir = file.path("examples", "sir_model_1")

sir <- Compartmental(sir_dir)  

sir_simulator = sir$simulators$tmb(
  time_steps = 100,
  state = c(S = 99.9, I = 0.1, R = 0),
  flow = c(foi = NA_real_, gamma = 0.1),
  beta = 0.2,
  N = empty_matrix # explained below
)

macpan2_results = sir_simulator$report() 

## pieces are there, but needs cleanup
##' @importFrom dplyr select mutate as_tibble across
##' @importFrom tidyr pivot_longer
compare_fits <- function(macpan2_results, deSolve_results) {
    ## drop unused columns, convert time to float
    x1 <- (macpan2_results
        |> as_tibble()
        |> select(-c(matrix, col))
        |> mutate(across(time, as.numeric))
    )
    ## pivot, drop deSolve attributes
    x2 <- (deSolve_results
        |> as_tibble()
        |> pivot_longer(-time, names_to = "row")
        |> mutate(across(c(time, value), c))
    )
    ## combine
    x12 <- (list(macpan = x1, deSolve = x2)
        |> dplyr::bind_rows(.id = "pkg")
    )
    ## find differences
    xdiff <- (x12
        |> group_by(time, row)
        |> summarise(value = diff(value), .groups = "drop")
    )
    ## plot together
    ggplot(x12, aes(time, value, colour = row)) +
        geom_line(aes(linetype = pkg))
    ## plot diffs
    ggplot(xdiff, aes(time, value, colour = row)) +
        geom_line()
    ## compare
    all.equal(x1, x2)
    ## TODO: understand/drop last point?
}

    
