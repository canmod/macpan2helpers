## how should this function work? return everything, or pass an option for which plots/comparisons to make?
##'
##' Compare fits between `deSolve::ode()` and `macpan2`
##' @importFrom dplyr select mutate as_tibble across filter summarise group_by
##' @importFrom tidyr pivot_longer
##' @importFrom ggplot2 ggplot aes geom_line geom_point %+% labs
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
##' @export
compare_fits <- function(macpan2_results, deSolve_results,
                         drop_last = TRUE, tolerance = sqrt(.Machine$double.eps)) {
    time <- pkg <- value <- NULL ## NSE visible bindings warnings

    if (inherits(macpan2_results, "ode") || !inherits(deSolve_results, "deSolve")) {
        stop("macpan2_results should be a (long-format) data frame and ",
             "deSolve_results should be a (wide-format) matrix/class 'deSolve'; ",
             "(did you switch the input order?)")
    }
    
    ## drop unused columns, convert time to float
    x1 <- (macpan2_results
        |> as_tibble()
        |> select(!any_of(c("matrix", "col")))
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
    ## find differences:
    ##  assume (?) macpan-first order remains, so diff is ode-macpan
    xdiff <- (x12
        |> group_by(time, row)
        |> summarise(value = diff(value), .groups = "drop")
    )
    ## plot together
    gg0 <- ggplot(x12, aes(time, value, colour = row)) 
    gg_comb <- gg0 + geom_line(aes(linetype = pkg))
    ## plot diffs
    gg_diff <- gg0 %+% xdiff + geom_line() +
        labs(y = "deSolve - macpan")
    ## compare (do we need both?
    ae <- all.equal(x1, x2, tolerance = tolerance)
    wc <- NULL
    if (requireNamespace("waldo")) {
        wc <- waldo::compare(x1, x2, tolerance = tolerance)
    }
    list(comb_plot = gg_comb, diff_plot = gg_diff, all_equal = ae,
         waldo_compare = wc)
}


    
