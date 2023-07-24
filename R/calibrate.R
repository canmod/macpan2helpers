add_slot <- function(x, value = empty_matrix, save_x = FALSE, return_x = FALSE) {
    args <- list()
    if (save_x) args <- c(args, list(.mats_to_save = x))
    if (return_x) args <- c(args, list(.mats_to_return = x))
    args <- c(list(value), args)
    names(args)[1] <- x
    do.call(sim$add$matrices, args)
}

## Note: example is failing with bf1de7f99
## with: Error in valid$consistency_params_mats$check(self$model) :
## optimization parameters are not consistent with matrices
## but validity could not be checked because:
## Error in if (any(!valid_pars)) { : missing value where TRUE/FALSE needed

##' add calibration information to a simulatore
##' @param sim a \code{macpan2} simulator (i.e., a \code{TMBSimulator} object)
##' @param data a list-like object (list or data frame) containing data to add
##' @param exprs a list of expressions to add
##' @param params a list of parameters with default/starting values
##' @export
#' @examples
#' ## it's convenient to have a function that sets up a fresh simulation
#' ## (since adding already-existing components to a simulation object throws an error)
#' library(dplyr)
#' setup_sim <- function() {
#'   m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
#'   sim <- m$simulators$tmb(
#'    time_steps = 100,
#'    state = c(S = 99, I = 1, R = 0),
#'    flow = c(foi = NA, gamma = 0.1),
#'    beta = 0.2,
#'    N = empty_matrix
#'  )
#' }
#' sim <- setup_sim()
#' ## if (require(outbreaks)) {
#' ##     I_obs <- influenza_england_1978_school[["in_bed"]]
#' ##     sim$ad_fun()$env$data$time_steps <- length(I_obs)
#' ## } else {
#'   set.seed(101)
#'   I_obs <- (sim$report(.phases = "during")
#'      |> filter(row == "I")
#'     |> mutate(obs = rnbinom(100, mu = value, size = 2))
#'     |> pull(obs)
#'   )
#' ## }
#' mk_calibrate(sim,
#'     data = list(I_obs = I_obs),
#'     params = list(beta = 0.2, I_sd = 1),
#'     transforms = list(beta = "log", I_sd = "log"),
#'     exprs = list(log_lik ~ dnorm(I_obs, I, I_sd)),
#'     debug = TRUE
#' )
#' sim$optimize$nlminb()
#' sim <- setup_sim()
#' mk_calibrate(sim,
#'     data = list(I_obs = rep(0, 100)),
#'     params = list(beta = 0.2, gamma = 0.05),
#'     transforms = list(beta = "log", gamma = "log"),
#'     exprs = list(log_lik ~ dpois(I_obs, I)),
#' )
#' ## not yet: gives NaN function eval warnings, singular convergence
#' # sim$optimize$nlminb()
mk_calibrate <- function(sim,
                         params = list(),
                         transforms = list(),
                         data = list(),
                         exprs = list(),
                         debug = FALSE) {
    ## how do I get these programmatically from sim?
    ## is there a better/easier way to get state names??
    ##  these are present in 'Compartmental' objects;
    ## easy to get with sim$labels$state().  Should they be carried along
    ## somehow?

    sim_params <- c("beta", "gamma")
    state_vars <- rownames(sim$print$model$data_arg()$mats[[1]])

    logit <- plogis  ## ugh; better way to handle transformations?

    ## for testing!

    cap <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

    ## add log-likelihood slot
    if (debug) cat("add log_lik matrix (empty)\n")
    add_slot("log_lik")
    ## added_vars <- character(0)

    ## add data
    for (nm in names(data)) {
        if (debug) cat(sprintf("add data matrix: %s\n", nm))
        do.call(sim$add$matrices, data[nm])
    }

    ## ?? needs to go before expressions get added?
    for (p in setdiff(names(params), sim_params)) {
        ## add params if not already in model
        if (debug) cat("add param (scalar placeholder value): ", p, "\n")
        add_slot(p, 1.0)
    }

    ## add _sim analogues for state variables referred to in expressions;
    ## substitute rbind_time(*_sim) in expressions
    for (i in seq_along(exprs)) {
        ee <- exprs[[i]]
        if (debug) cat("process expression: ", deparse(ee), "\n")
        all_vars <- all.vars(ee)
        ## create a placeholder
        for (v in intersect(all_vars, state_vars)) {
            ph <- paste0(v, "_sim")
            if (debug) cat("add (empty matrix): ", ph, "\n")
            add_slot(ph, save_x = TRUE)
            newexpr <- reformulate(v, response = ph)
            if (debug) cat("add ", deparse(newexpr), "\n")
            sim$insert$expressions(
                           newexpr,
                           .phase = "during",
                           .at = Inf)
            bind_var <- sprintf("rbind_time(%s)", ph)
            ## convert to parsed expression, then get rid of expression()
            newsym <- parse(text=bind_var)[[1]]
            exprs[[i]] <- do.call(substitute,
                                  list(ee, setNames(list(newsym), v)))
        }
        if (debug) cat("add ", deparse(exprs[[i]]), "\n")
        sim$insert$expressions(exprs[[i]], .phase = "after")
    }

    ## modify names for transforms, apply transform to specified values
    trpars <- transforms != ""
    trp <- params[trpars]
    names(trp) <- paste(transforms[trpars], names(trp), sep = "_")
    ## trp <- Map(function(x, tr) get(tolower(tr))(x), trp, transforms[trpars])
    pframe <- data.frame(mat = names(trp), row = 0, col = 0, default = unlist(trp))
    rownames(pframe) <- NULL ## cosmetic

    if (debug) {
        cat("param_frame:\n")
        print(pframe)
    }

    if (debug) cat("adding transformations\n")
    add_trans <- function(tr, nm) {
        if (debug) cat("add transformation: ", cap(tr)," ", nm, "\n")
        sim$add$transformations(get(cap(tr))(nm))
    }

    ## add transformations
    Map(add_trans, transforms[trpars], names(params)[trpars])

    ## now add param frame (does order matter??)
    sim$replace$params_frame(pframe)

    if (debug) cat("set obj_fn to -sum(log_lik)\n")
    sim$replace$obj_fn(~ -sum(log_lik))
    ## add transformations
    ## add parameters
    ## for now, assume all parameters are scalar?

    ## everything is done as a side effect (mutating state of sim)
    return(NULL)

}
