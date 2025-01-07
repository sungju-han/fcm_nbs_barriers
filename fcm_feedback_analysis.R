#' FCM Analysis for NBS Implementation Barriers
#' 
#' Functions for analyzing FCM including centrality measures, network metrics,
#' and feedback loops
#' Used in "Beyond Individual Barriers" paper (Han et al., 2025)
#'
#' @author Sungju Han

# Load required packages
library(igraph)
library(here)

#' Calculate Network Metrics
#' @param graph_model igraph object
#' @return list of network metrics
calculate_network_metrics <- function(graph_model) {
  # Density
  density <- graph.density(graph_model)
  
  # Calculate in-degree and out-degree
  in_degree <- degree(graph_model, mode = "in")
  out_degree <- degree(graph_model, mode = "out")
  
  # Calculate receiver (more in than out) and driver (more out than in) variables
  receivers <- sum(in_degree > out_degree)
  drivers <- sum(out_degree > in_degree)
  
  # Complexity score
  complexity <- ifelse(drivers > 0, receivers/drivers, NA)
  
  return(list(
    density = density,
    complexity = complexity,
    in_degree = in_degree,
    out_degree = out_degree
  ))
}

#' Calculate Centrality Metrics
#' @param graph_model igraph object
#' @return data frame of centrality metrics
calculate_centrality <- function(graph_model) {
  # Get adjacency matrix
  adj_matrix <- as_adjacency_matrix(graph_model, attr = "weight")
  
  # Calculate indegree and outdegree
  indegree <- colSums(abs(adj_matrix))
  outdegree <- rowSums(abs(adj_matrix))
  
  # Calculate centrality
  centrality <- indegree + outdegree
  
  # Calculate transformative potential
  transformative_potential <- outdegree - indegree
  
  return(data.frame(
    node = V(graph_model)$name,
    indegree = indegree,
    outdegree = outdegree,
    centrality = centrality,
    transformative_potential = transformative_potential
  ))
}

#' Normalize Centrality Scores Globally
#' @param centrality_list list of centrality dataframes from different sites
#' @return list of normalized centrality dataframes
normalize_centrality_global <- function(centrality_list) {
  # Get all centrality scores
  all_centrality <- unlist(lapply(centrality_list, function(x) x$centrality))
  
  # Calculate global min and max
  global_min <- min(all_centrality)
  global_max <- max(all_centrality)
  
  # Normalize each site's centrality scores
  normalized_list <- lapply(centrality_list, function(df) {
    df$normalized_centrality <- (df$centrality - global_min) / (global_max - global_min)
    return(df)
  })
  
  return(normalized_list)
}

#' Find Feedback Loops
#' @param graph_model igraph object
#' @return list of feedback loops
find_feedback_loops <- function(graph_model) {
  # Remove zero-weight edges
  zero_weights <- E(graph_model)[weight == 0]
  graph_model <- delete_edges(graph_model, zero_weights)
  
  Cycles <- list()
  for (v1 in V(graph_model)) {
    for (v2 in neighbors(graph_model, v1, mode = "out")) {
      cycles <- all_simple_paths(graph_model, v2, v1, mode = "out")
      if (length(cycles) > 0) {
        cycles_char <- lapply(cycles, function(x) sort(as.character(x)))
        for (cycle in cycles_char) {
          cycle_str <- paste(cycle, collapse = " ")
          if (!(cycle_str %in% Cycles)) {
            Cycles <- c(Cycles, cycle_str)
          }
        }
      }
    }
  }
  return(Cycles)
}

#' Analyze Feedback Loops
#' @param graph_model igraph object
#' @param fcm_name name of the FCM for reporting
#' @return list of feedback loop information
analyze_feedback_loops <- function(graph_model, fcm_name) {
  node_names <- V(graph_model)$name
  cycles <- find_feedback_loops(graph_model)
  
  results <- list()
  for (i in seq_along(cycles)) {
    cycle_nodes <- node_names[as.numeric(unlist(strsplit(cycles[[i]], " ")))]
    cycle_info <- list(
      nodes = cycle_nodes,
      relationships = data.frame(
        from = character(),
        to = character(),
        weight = numeric(),
        stringsAsFactors = FALSE
      )
    )
    
    for (j in 1:(length(cycle_nodes))) {
      from_node <- cycle_nodes[j]
      to_node <- if(j == length(cycle_nodes)) cycle_nodes[1] else cycle_nodes[j + 1]
      weight <- graph_model[from_node, to_node]
      cycle_info$relationships <- rbind(
        cycle_info$relationships,
        data.frame(from = from_node, to = to_node, weight = weight)
      )
    }
    
    results[[i]] <- cycle_info
  }
  
  return(results)
}

# Example usage:
#' @example
#' # Read FCM data
#' fcm_data <- read.csv("path_to_fcm_data.csv", row.names = 1)
#' 
#' # Create graph
#' g_fcm <- graph_from_adjacency_matrix(as.matrix(fcm_data), weighted = TRUE)
#' 
#' # Calculate network metrics
#' network_metrics <- calculate_network_metrics(g_fcm)
#' 
#' # Calculate centrality measures
#' centrality_metrics <- calculate_centrality(g_fcm)
#' 
#' # Analyze feedback loops
#' feedback_loops <- analyze_feedback_loops(g_fcm, "Example FCM")
