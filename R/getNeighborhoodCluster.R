# getNeighborhoodCluster
#'
#' For each node, gets the most common cluster assignment in its
#' neighboorhood.
#'
#' @param adjMat An adjacency matrix.
#' @param clusterAssignments A vector of cluster assignments of each node.
#' 
#' @export
#' @return A vector with the most common cluster assignments in the
#' neighborhood of each node.
#'
getNeighborhoodCluster <- function(adjMat, clusterAssignments) {

    nNodes = length(clusterAssignments)

    # change to sparse 3 column representation
    # IMPORTANT: internal indices in Matrix start with 0 not 1
    adjMat = as(adjMat, "dgTMatrix")

    clusterCountMat = matrix(0, nrow = nNodes,
        ncol = length(unique(clusterAssignments)))
    for(i in 1:length(adjMat@i)) {
        node = adjMat@i[i+1]
        cluster = clusterAssignments[adjMat@j[i+1]]
        clusterCountMat[node, cluster] = clusterCountMat[node, cluster] + 1  
    }

    neighborhoodCluster = vector(length = nNodes)
    for(i in 1:nNodes) {
        neighborhoodCluster[i] = which.max(clusterCountMat[i, ])
    }

    return( neighborhoodCluster )
}
