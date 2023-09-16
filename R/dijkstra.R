#'Dijkstra Algorithm for the shortest path.
#'
#'@param graph It must be data frame. The graph is a data frame including three variables v1,v2,w and contains the edges of the graph from v1 to v2 and the weight of the edge w.
#'
#'@param init_node It must be numeric scalar. The initial node that the algorithm will start calculating the shortest path.
#'
#'@return It will be a vector with the shortest paths from the initial node.























dijkstra <- function(graph, init_node) {
  all_nodes <- unique(c(graph$v1, graph$v2))
  if (!is.numeric(init_node) || length(init_node) != 1) {
    stop("the initial node should be a numerical scalar")
  }
  if (!(init_node %in% all_nodes)) {
    stop("the initial node must be included in the list")
  }

  length_nodes <- length(all_nodes)
  distance_nodes <- rep(Inf, length_nodes)
  distance_nodes[init_node] <- 0
  visited_nodes <- numeric(0)

  while (length(visited_nodes) < length_nodes) {
    unvisited_nodes <- setdiff(all_nodes, visited_nodes)

    min_distance_nodes <- Inf
    min_dist <- NULL

    for (node in unvisited_nodes) {
      if (distance_nodes[node] < min_distance_nodes) {
        min_distance_nodes <- distance_nodes[node]
        min_dist <- node
      }
    }

    visited_nodes <- c(visited_nodes, min_dist)

    connected_nodes <- unique(c(graph$v1[graph$v2 == min_dist], graph$v2[graph$v1 == min_dist]))

    for (neighbor in connected_nodes) {
      new_dist <- distance_nodes[min_dist] + graph$w[which(graph$v1 == min_dist & graph$v2 == neighbor)]
      if (new_dist < distance_nodes[neighbor]) {
        distance_nodes[neighbor] <- new_dist
      }
    }
  }

  return(distance_nodes)
}


