#' Visualize a Compartmental model
#'
#' @param model Model object created using [macpan2::Compartmental()]
#' @param label_flows Logical. Should edges be labelled with flow rates?
#'
#' @return A `visNetwork` object
#'
#' @export
#' @examples
#' sir = macpan2::Compartmental(system.file('starter_models', 'sir', package = 'macpan2'))
#' visCompartmental(sir)
visCompartmental <- function(model, label_flows = FALSE){
  nodes = data.frame(id = model$labels$state(),
                     label = model$labels$state(),
                     shape = "square")
  edges = model$flows_expanded()[c("from", "to")]
  if(label_flows) edges = cbind(edges,
                                label = model$flows_expanded()[["flow"]])

  vn = visNetwork::visNetwork(nodes, edges) |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visHierarchicalLayout(direction = "LR")

  return(vn)
}
