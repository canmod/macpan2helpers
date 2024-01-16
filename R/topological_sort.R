has_inflow = function(focal_state, to_states, state_nms) {
  stopifnot(focal_state %in% state_nms)
  focal_state %in% to_states
}

#' Topological Sort
#'
#' @param model \code{\link{Compartmental}} model.
#' @param warn_not_dag Throw a warning if the model flows do not constitute
#' a directed acyclic graph (DAG)?
#'
#' @export
#' @returns Character vector giving the names of the state variables in
#' topologically sorted order.
topological_sort = function(model, warn_not_dag = TRUE) {
  topological_sort_engine(
    flows = model$flows_expanded(),
    model$labels$state(),
    warn_not_dag = warn_not_dag
  )
}

topological_sort_engine = function(flows, state_nms, warn_not_dag = TRUE) {
  if (missing(state_nms)) state_nms = unique(c(flows$from, flows$to))
  state_order = c()
  n = length(state_nms)
  for (i in 1:n) {
    if (length(state_nms) == 0L) break
    remaining_inflow = sapply(
      state_nms,
      has_inflow,
      flows$to,
      state_nms
    )
    state_order = c(state_order, state_nms[!remaining_inflow])
    flows = flows[!flows$to %in% state_order, , drop = FALSE]
    flows = flows[!flows$from %in% state_order, , drop = FALSE]
    state_nms = state_nms[remaining_inflow]
    is_acyclic = (
        isTRUE(all(remaining_inflow))
      & length(state_nms) != 0L
      & length(remaining_inflow) != 0L
    )
    if (is_acyclic) {
      if (warn_not_dag) {
        warning(
         "\nState network is not acyclic (i think),",
         "\nand therefore cannot be topologically sorted.",
         "\nDefaulting to original order where necessary."
        )
      }
      state_order = c(state_order, state_nms) |> unique()
      state_nms = character(0L)
    }
  }
  state_order
}

layered_sort_engine = function(flows) {
  state = topological_sort_engine(flows)
  
}

# ## in macpan2
# @param x,y data frames
# @param by 
# connection_merge = function(x, y, by, output_cols) {
#   merge(x, y, by = by, sort = FALSE)[, output_cols]
# }



if (FALSE) {
  library(macpan2helpers)
  library(macpan2)
  library(oor)
  
  epi = atomic_partition(c("S", "I", "R"), c("lambda", "gamma"), "Epi")
  age = atomic_partition(c("young", "old"), c("aging"), "Age")
  
  
  
  cartesian(epi$state(), age$state())
  cartesian(epi$flow(), age$state())
  cartesian(epi$state(), age$flow())
  
  
  epi_state = epi$filter("state", .wrt = "Vec")$select_out("Vec")
  epi_state$filter("S")
  epi_state$
  join_partitions(state, state, "A", "From", "To", "A.BFrom")
  join_partitions(state, state, "A", "From", "To", "A.BTo")
  
  macpan2helpers:::topological_sort_engine(dd)
  
  #oor_debug$flag("to_conn_merge")
  #sir_symp = Compartmental("../macpan2/inst/starter_models/sir_symp/")
  sir_symp$flows_expanded()
  sir_symp$labels$state()
  sir_symp$labels$flow()
  topological_sort(sir_symp)
  sir_symp$trans()
  sir_symp$trans()
  
  sir_symp$trans_info$frame()
  
  
  
}
