#' Visualize a Compartmental model
#'
#' @param model Model object created using [macpan2::Compartmental()]
#' @param label_flows Logical. Should edges be labelled with flow rates?
#'
#' @return A `visNetwork` object
#'
#' @example
#' sir = Compartmental(system.file('starter_models', 'sir', package = 'macpan2'
#' visCompartmental(sir)))
visCompartmental <- function(model, label_flows = FALSE){
  nodes = data.frame(id = sir$labels$state(),
                     label = sir$labels$state(),
                     shape = "square")
  edges = sir$flows_expanded()[c("from", "to")]
  if(label_flows) edges = cbind(edges,
                                label = sir$flows_expanded()[["flow"]])

  vn = visNetwork(nodes, edges) |>
    visEdges(arrows = "to") |>
    visHierarchicalLayout(direction = "LR")

  return(vn)
}
