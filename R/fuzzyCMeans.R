#' Fuzzy C-Means
#'
#' @description This function used to perform Fuzzy C-Means of X dataset.
#'
#' @param X data frame n x p
#' @param K specific number of cluster (must be >1)
#' @param m fuzzifier / degree of fuzziness
#' @param max.iteration maximum iteration to convergence
#' @param threshold threshold of convergence
#' @param RandomNumber specific seed
#'
#' @return func.obj objective function that calculated.
#' @return U matrix n x K consist fuzzy membership matrix
#' @return V matrix K x p consist fuzzy centroid
#' @return D matrix n x K consist distance of data to centroid that calculated
#' @return Clust.desc cluster description (dataset with additional column of cluster label)
#'
#' @details This function perform Fuzzy C-Means algorithm by Bezdek (1981).
#' Fuzzy C-Means is one of fuzzy clustering methods to clustering dataset
#' become K cluster. Number of cluster (K) must be greater than 1. To control the overlaping
#' or fuzziness of clustering, parameter m must be specified.
#' Maximum iteration and threshold is specific number for convergencing the cluster.
#' Random Number is number that will be used for seeding to firstly generate fuzzy membership matrix.
#' @details Clustering will produce fuzzy membership matrix (U) and fuzzy cluster centroid (V).
#' The greatest value of membership on data point will determine cluster label.
#' Centroid or cluster center can be use to interpret the cluster. Both membership and centroid produced by
#' calculating mathematical distance. Fuzzy C-Means calculate distance with Euclideans norm. So it can be said that cluster
#' will have sperichal shape of geometry.
#'
#' @export
fuzzy.CM <- function(X,K,m,max.iteration,threshold,RandomNumber) {
  ## Set data
  library(MASS)
  data.X <- as.matrix(X)
  n <- nrow(data.X)
  p <- ncol(data.X)
  cat("Initiation Parameter...\n")
  ## Initiation Parameter##
  if (missing(K) ||
      (K <= 1) || !(is.numeric(K)) || (K %% ceiling(K) > 0))
    K = 2
  if (missing(m) || (m <= 1) || !(is.numeric(m)))
    m = 2
  if (missing(max.iteration))
    max.iteration = 1000
  if (missing(threshold))
    threshold <- 10 ^ (-9)
  ## Initiation Matrix
  if (RandomNumber > 0)
    set.seed(RandomNumber)
  ## Membership Matrix U (n x K)
  U <- matrix(runif(n * K,0,1),n,K)

  ## Prerequirement of U:
  ## Sum of membership on datum is 1
  U <- U / rowSums(U)

  ## Centroid Matrix V (K x p)
  V <- matrix(0,K,p)

  ## Distance Matrix D (n x K)
  D <- matrix(0,n,K)

  U.old <- U + 1
  V.old <- V
  D.old <- D
  iteration = 0
  flag = 0

  cat("Processing:\n")
  while ((sum(abs(U.old - U)) > threshold) &&
         (iteration < max.iteration))
  {
    cat("=")
    U.old <- U
    D.old <- D
    V.old <- V

    ## Calculate Centroid
    V <- t(U ^ m) %*% data.X / colSums(U ^ m)

    ## Calculate Distance
    for (k in 1:K)
    {
      for (i in 1:n)
      {
        D[i,k] = t(data.X[i,] - V[k,]) %*% (data.X[i,] - V[k,])
      }
    }

    ## Calculate Membership/Fuzzy Partition
    if ((all(is.finite(D)) == T))
    {
      for (i in 1:n)
      {
        if (min(D[i,]) == 0)
        {
          c <- which.min(D[i,])
          U[i,] = 0
          U[i,c] = 1
        }
        else
          U[i,] <- 1 / ((D[i,] ^ (1 / (m - 1))) * sum((1 / D[i,]) ^ (1 /
                                                                       (m - 1))))
      }
      if (all(is.finite(U) == FALSE))
      {
        U = U.old
        D = D.old
        V = V.old

      }
    }
    else{
      V <- V.old
      U <- U.old
      D <- D.old

    }
    for (i in 1:n)
      for (k in 1:K) {
        if (U[i,k] < 0)
          U[i,k] = 0
        else if (U[i,k] > 1)
          U[i,k] = 1
      }
    func.obj = sum(U ^ m * D)
    if (func.obj == 0)
      func.obj = Inf
    iteration = iteration + 1
  }

  func.obj -> func.Obj.opt
  U -> U.opt
  V -> V.opt
  D -> D.opt
  ###Labelling###
  colnames(U.opt) = paste("Clust",1:K,sep = " ")
  Clust.desc <- matrix(0,n,p + 1)
  rownames(Clust.desc) <- rownames(X)
  colnames(Clust.desc) <- c(colnames(X),"cluster")
  Clust.desc[,1:p] <- data.X
  for (i in 1:n)
    Clust.desc[i,p + 1] <- which.max(U.opt[i,])
  result <- list()
  result$func.obj <- func.Obj.opt
  result$U <- U.opt
  result$V <- V.opt
  result$D <- D.opt
  result$m <- m
  result$Clust.desc <- Clust.desc
  cat("\nFinish :)")
  return(result)
}
