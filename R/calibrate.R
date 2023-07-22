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
##' @param params a list of parameters with default/starting values
##' @export
#' @examples
#' m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
#' sim <- m$simulators$tmb(
#'  time_steps = 100,
#'  state = c(S = 99, I = 1, R = 0),
#'  flow = c(foi = NA, gamma = 0.1),
#'  beta = 0.2,
#'  N = empty_matrix
#'  )
#' mk_calibrate(sim,
#'     data = list(I_obs = rep(0, 100)),
#'     params = list(beta = 0.2, I_sd = 1),
#'     transforms = list(beta = "Log", I_sd = "Log"),
#'     exprs = list(log_lik ~ dnorm(I_obs, I, I_sd))
#' )
mk_calibrate <- function(sim,
                         params = list(),
                         transforms = list(),
                         data = list(),
                         exprs = list()) {
    ## how do I get these programmatically from sim?
    ## is there a better/easier way to get state names??
    ##  these are present in 'Compartmental' objects;
    ## easy to get with sim$labels$state().  Should they be carried along
    ## somehow?

    sim_params <- c("beta", "gamma")
    state_vars <- rownames(sim$print$model$data_arg()$mats[[1]])

    logit <- plogis  ## ugh; better way to handle transformations?
    
    ## for testing!


    cap <- function(s) paste(toupper(substring(s, 1, 1)), substring(s, 2))
    
    ## add log-likelihood slot
    add_slot("log_lik")
    ## added_vars <- character(0)

    ## add data
    for (nm in names(data)) {
        do.call(sim$add$matrices, data[nm])
    }

    ## ?? needs to go before expressions get added?
    for (p in setdiff(names(params), sim_params)) {
        ## add params if not already in model
        add_slot(p)
    }

    ## add _sim analogues for state variables referred to in expressions;
    ## substitute rbind_time(*_sim) in expressions
    for (i in seq_along(exprs)) {
        ee <- exprs[[i]]
        all_vars <- all.vars(ee)
        ## create a placeholder 
        for (v in intersect(all_vars, state_vars)) {
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
        sim$insert$expressions(ee, .phase = "after")
    }

    ## modify names for transforms, apply transform to specified values
    trpars <- transforms != ""
    trp <- params[trpars]
    names(trp) <- paste(transforms[trpars], names(trp), sep = "_")
    trp <- Map(function(x, tr) get(tolower(tr))(x), trp, transforms[trpars])
    pframe <- data.frame(mat = names(trp), row = 0, col = 0, default = trp)
    sim$replace$params_frame(pframe)

    ## add transformations
    Map(function(tr, nm) sim$add$transformations(get(cap(tr))(nm)),
        transforms[trpars], names(params)[trpars])

    
    sim$replace$obj_fn(~ -sum(log_lik))
    ## add transformations
    ## add parameters
    ## for now, assume all parameters are scalar?

}

    
    
1
