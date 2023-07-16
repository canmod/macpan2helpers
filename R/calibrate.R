add_slot <- function(x, save_x = FALSE, return_x = FALSE) {
    args <- list()
    if (save_x) args <- c(args, list(.mats_to_save = x))
    if (return_x) args <- c(args, list(.mats_to_return = x))
    args <- c(list(empty_matrix), args)
    names(args)[1] <- x
    do.call(sim$add$matrices, args)
}


##' add calibration information to a simulatore
##' @param sim a \code{macpan2} simulator (i.e., a \code{TMBSimulator} object)
##' @param data a list-like object (list or data frame) containing data to add
##' @param exprs a list of expressions to add
## wishlist
## \itemize
#' @examples
#' m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
#' sim <- m$simulators$tmb(
#'  time_steps = 100,
#'  state = c(S = 99, I = 1, R = 0),
#'  flow = c(foi = NA, gamma = 0.1),
#'  beta = 0.2,
#'  N = empty_matrix
#'  )
#' ## mk_calibrate(sim, list(I_obs = rep(0, 100)))
mk_calibrate <- function(sim,
                         params = list(),
                         transforms = NULL,
                         data = list(),
                         exprs = list(log_lik ~ dnorm(I_obs, I, I_d))) {
    ## for testing!
    data = list(I_obs = rnorm(100))
    exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
    ## is there a better/easier way to get state names??
    ##  these are present in 'Compartmental' objects;
    ## easy to get with sim$labels$state().  Should they be carried along
    ## somehow?
    state_vars <- rownames(sim$print$model$data_arg()$mats[[1]])
    add_slot("log_lik")
    added_vars <- character(0)
    for (nm in seq(names(data))) {
        do.call(sir_simulator$add$matrices, data[nm])
    }
    for (i in seq_along(exprs)) {
        ee <- exprs[[i]]
        all_vars <- all.vars(ee)
        ## create a placeholder 
        for (v in intersect(all_vars, c(added_vars, state_vars))) {
            ph <- paste0(v, "_sim")
            add_slot(ph)
            sim$insert$expressions(
                           reformulate(v, response = ph),
                           .phase = "during",
                           .at = Inf)
            bind_var <- sprintf("rbind_time(%s)", ph)
            exprs[[i]] <- do.call(substitute,
                                  list(ee, setNames(list(as.name(bind_var)), v)))
                                 
        }
        ## add remaining required variables
        for (v in setdiff(all_vars, c(added_vars, state_vars))) {
            add_slot(ph)
        }
    }
    sim$replace$obj_fn(~ -sum(log_lik))
    ## add transformations
    ## add parameters
    ## for now, assume all parameters are scalar?

}

    
    
