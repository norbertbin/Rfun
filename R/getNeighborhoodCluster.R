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

    # ensure that the adjacency matrix is sparse
    adjMat = Matrix(adjMat)

    # change to 3 column representation
    # IMPORTANT: internal indices in Matrix start with 0 not 1
    nC3col = as(adjMat, "dgTMatrix")
    nC3col = matrix(c(nC3col@i+1, nC3col@j+1), ncol = 2)

    neighborhoodCluster = vector(length = nNodes)
    for(i in 1:nNodes) {
        tmpTable = table(clusterAssignments[nC3col[nC3col[,1] == i, 2]])
        neighborhoodCluster[i] = as.numeric(names(
                               tmpTable[which.max(tmpTable)]))
    }

    return( neighborhoodCluster )
}
