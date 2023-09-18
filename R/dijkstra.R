#'Dijkstra Algorithm for the shortest path.
#'
#'@param graph The graph must be data frame. It includes three variables v1,v2,w and contains the edges of the graph from v1 to v2 and the weight of the edge w.
#'
#'@param init_node Inıt_node must be numeric scalar. The initial node that the algorithm will start calculating the shortest path.
#'
#'
#'@description
#'The function takes a data frame. v1 and v2 vertices of the graph and w weight of the nodes.
#'
#'all_nodes<-unique(c(graph$v1, graph$v2)) this line will extract all unique nodes from the graph data frame.
#'
#'The function checks if the init_node is a numerical scalar. It checks if init_node is included in the node list.
#'It ,also, checks if the names of the columns of the data frame are v1, v2 and w.
#'
#'If the init_node is not a numeric scalar, it will stop with a message "The initial node should be a numeric scalar"
#'initializing length_nodes, distance_nodes, visited_nodes variables.
#'
#'The length_nodes variable, stores the total number of the nodes in the graph.
#'The distance_nodes variable, is a numeric vector initialized with infinity for each node's distance. The distance to the initial node is set to zero.
#'The visited_nodes variable, is an empty numeric vector. Visited nodes of the graph are temporarily stored here.
#'
#'The function has a loop for going through nodes. This loop stops when all nodes in the graph have been visited.
#'
#'unvisited_nodes<- setdiff(all,nodes,visited_nodes) finding the nodes that are not visited.
#'
#'To find the minimum distanced node from the initial node, the code repeats and goes through all not visited nodes.
#'The minimum distanced node is added to the 'visited_nodes' vector.
#'For each node which has connection to the minimum distanced node, the function calculates a new distance value and if it is shorter than the current one it will update the old value.

#'@return It will be a numeric vector with the shortest paths from the initial node.
#'
#'@examples
#'wiki_graph <-
#'data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'dijkstra(wiki_graph, 1)
#'
#'@seealso [Dijkstra's Algorithm Wikipedia Page] (https://en.wikipedia.org/wiki/Dijkstra%27s algorithm)
#'
#'@export
dijkstra <- function(graph, init_node) {
  all_nodes <- unique(c(graph$v1, graph$v2))
  if (!is.numeric(init_node) || length(init_node) != 1) {
    stop("The initial node should be a numerical scalar")
  }
  if (!(init_node %in% all_nodes)) {
    stop("The initial node must be included in the list")
  }
  if(!((names(graph)[1]=="v1")&& (names(graph)[2]=="v2")&& (names(graph)[3]=="w"))){
    stop("Dataset should have v1,v2 and w columns. Check it again!!!")
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


