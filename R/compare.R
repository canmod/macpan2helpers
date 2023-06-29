## how should this function work? return everything, or pass an option for which plots/comparisons to make?
##'
##' Compare fits between `deSolve::ode()` and `macpan2`
##' @importFrom dplyr select mutate as_tibble across filter summarise group_by
##' @importFrom tidyr pivot_longer
##' @importFrom ggplot2 ggplot aes geom_line geom_point %+%
##' @param macpan2_results results from running \code{report()} on a macpan2 simulator
##' @param deSolve_results results from \code{ode(...)} (typically want to use \code{method = "euler"} to match)
##' @param drop_last drop last point? (for reasons I don't yet understand, macpan2 diverges by a little bit
##' at last time point)
##' @param tolerance  tolerance for numerical comparisons (set to 0 for exact comparison)
##' @return a list containing elements
##' * `comb_plot`: combination plot (all variables)
##' * `diff_plot`: difference plot
##' * `all_equal`: results of `all.equal()`
##' * `waldo_compare`: results of `waldo::compare` (if available)
##' @examples
##' if (require("deSolve")) {
##' sirmod <- function(t, y, parameters) {
##'     grad <- with(as.list(c(y, parameters)),
##'                   c(- beta * S * I/N,
##'                     beta * S * I/N - gamma * I,
##'                      gamma * I))
##'     list(grad)
##' }
##' 
##' times <- 0:101
##' paras <- c(mu = 0, N = 100, gamma = 0.1, beta = 0.2)
##' start <- c(S = 0.999, I = 0.001, R = 0) * paras["N"]
##' deSolve_results = ode(y = start, times = times, func = sirmod, parms = paras,
##'                       method = "euler")
##' 
##' # macpan2
##' library(macpan2)
##' 
##' sir <- Compartmental(system.file("models", "sir_model_1", package = "macpan2helpers"))
##' 
##' sir_simulator = sir$simulators$tmb(
##'   time_steps = 100,
##'   state = start,
##'   flow = c(foi = NA_real_, gamma = paras[["gamma"]]),
##'   beta = paras[["beta"]],
##'   N = empty_matrix
##' )
##' macpan2_results = sir_simulator$report() 
##' comp <- compare_fits(macpan2_results, deSolve_results)
##' print(comp$comb_plot)
##' print(comp$diff_plot)
##' comp$all_equal
##' comp$waldo_compare
##' }
##' @export
compare_fits <- function(macpan2_results, deSolve_results,
                         drop_last = TRUE, tolerance = sqrt(.Machine$double.eps)) {
    time <- pkg <- value <- NULL ## NSE visible bindings warnings
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
    if (drop_last) {
        x1 <- dplyr::filter(x1, time != max(time))
        x2 <- dplyr::filter(x2, time != max(time))
    }
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
    gg0 <- ggplot(x12, aes(time, value, colour = row)) 
    gg_comb <- gg0 + geom_line(aes(linetype = pkg))
    ## plot diffs
    gg_diff <- gg0 %+% xdiff + geom_line()
    ## compare (do we need both?
    ae <- all.equal(x1, x2, tolerance = tolerance)
    wc <- NULL
    if (requireNamespace("waldo")) {
        wc <- waldo::compare(x1, x2, tolerance = tolerance)
    }
    list(comb_plot = gg_comb, diff_plot = gg_diff, all_equal = ae,
         waldo_compare = wc)
}


    
