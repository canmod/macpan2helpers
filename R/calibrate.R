## taken from glmmTMB
replaceForm <- function(term,target,repl) {
    if (identical(term,target)) return(repl)
    if (!inForm(term,target)) return(term)
    if (length(term) == 2) {
        return(substitute(OP(x),list(OP=replaceForm(term[[1]],target,repl),
                                     x=replaceForm(term[[2]],target,repl))))
    }
    return(substitute(OP(x,y),list(OP=replaceForm(term[[1]],target,repl),
                                   x=replaceForm(term[[2]],target,repl),
                                   y=replaceForm(term[[3]],target,repl))))
}

## better way to do this?
mk_symb <- function(txt) {
    parse(text=txt)[[1]]
}

add_slot <- function(x) {
    args <- list(empty_matrix, .mats_to_save = x, .mats_to_return = x)
    names(args)[1] <- x
    do.call(sim$add$matrices, args)
}


#' @examples
#' m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
#' sim <- sir$simulators$tmb(
#'  time_steps = 100,
#'  state = c(S = 99, I = 1, R = 0),
#'  flow = c(foi = NA, gamma = 0.1),
#'  beta = 0.2,
#'  N = empty_matrix
#'  )
#' ## mk_calibrate(sim, list(I_obs = rep(0, 100)))
mk_calibrate <- function(sim,
                         data = list(),
                         exprs = list(log_lik ~ dnorm(I_obs, I, I_d))) {
    exprs = list(log_lik ~ dnorm(I_obs, I, I_d))
    ## how do I get state names?? these are present in 'Compartmental' objects;
    ## easy to get with sim$labels$state().  Should they be carried along
    ## somehow?
    state_vars <- rownames(sim$print$model$data_arg()$mats[[1]])
    add_slot("log_lik")
    added_vars <- character(0)
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
                                  list(ee, setNames(list(mk_symb(bind_var)), v)))
                                 
        }
        for (v in setdiff(all_vars, c(added_vars, state_vars))) {
            add_slot(ph)
        }
    }
}
    
    
        


    
}
    
    
