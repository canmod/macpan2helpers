#' Visualize a Compartmental model
#'
#' @param model Model object created using [macpan2::Compartmental()]
#' @param label_flows Logical. Should edges be labelled with flow rates?
#' @param ... Additional arguments to pass to \code{\link{visNetwork}}
#'
#' @return A `visNetwork` object
#'
#' @examples
#' # example code
#' # sir <- macpan2::Compartmental(system.file('starter_models', 'sir', package = 'macpan2'))
#' # visCompartmental(sir)
#' @export
visCompartmental <- function(model, label_flows = FALSE, ...){
  nodes = node_data(model)
  edges = edge_data(model, label_flows = label_flows)
  vn = vis_obj(nodes, edges, label_flows = label_flows, ...) |>
    visNetwork::visHierarchicalLayout(direction = "LR")
  return(vn)
}

node_data = function(model) {
  data.frame(
    id = model$labels$state(),
    label = model$labels$state(),
    shape = "square"
  )
}

edge_data = function(model, label_flows = FALSE) {
  edges = model$flows_expanded()[c("from", "to")]
  if(label_flows) {
    edges = cbind(
      edges,
      label = model$flows_expanded()[["flow"]]
    )
  }
  edges
}

vis_obj = function(nodes, edges, ...) {
  visNetwork::visNetwork(nodes, edges, ...) |>
    visNetwork::visEdges(arrows = "to")
}
