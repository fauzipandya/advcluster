#' Fuzzy C-Means
#'
#' @description  Fuzzy C-Means clustering Algorithm (Bezdek, 1984)
#' @param X dataset (matrix/data frame)
#' @param K number of cluster
#' @param m fuzzyfier
#' @param max.iteration maximum iteration for convergence
#' @param threshold convergence criteria
#' @param member.init membership object or matrix that will be used for initialized
#' @param RandomNumber random number for start initializing
#' @param print.result print result (9/1)
#' @details This function perform Fuzzy C-Means algorithm by Bezdek (1984).
#' Fuzzy C-Means is one of fuzzy clustering methods to clustering dataset
#' become K cluster. Number of cluster (K) must be greater than 1. To control the overlaping
#' or fuzziness of clustering, parameter m must be specified.
#' Maximum iteration and threshold is specific number for convergencing the cluster.
#' Random Number is number that will be used for seeding to firstly generate fuzzy membership matrix.
#' @details Clustering will produce fuzzy membership matrix (U) and fuzzy cluster centroid (V).
#' The greatest value of membership on data point will determine cluster label.
#' Centroid or cluster center can be use to interpret the cluster. Both membership and centroid produced by
#' calculating mathematical distance. Fuzzy C-Means calculate distance with Euclideans norm.
#' @references Balasko, B., Abonyi, J., & Feil, B. (2002). Fuzzy Clustering and Data Analysis Toolbox: For Use with Matlab. Veszprem, Hungary.
#' @references Bezdek, J. C., Ehrlich, R., & Full, W. (1984). FCM: The Fuzzy C-Means Clustering Algorithm. Computers and Geosciences Vol 10, 191-203
#' @export
fuzzy.CM<- function(X,
                    K,
                    m,
                    max.iteration,
                    threshold,
                    member.init,
                    RandomNumber=0,
                    print.result=0)
  {
  #Parameter Checker
  if(missing(X))
    return("Data Unknown\n")
  if(missing(K)||K<2||!is.numeric(K))
  {
    K<-2
    cat("Default K=2\n")
  }
  if(missing(m)||m<1||!is.numeric(m))
  {
    m<-1.5
    cat("Fuzzyfier unidentified/error/less than 1. Default Fuzzyfier (1.5) will be used.\n")
  }
  if(missing(max.iteration)||!is.numeric(max.iteration))
  {
    max.iteration<-1000
    cat("Maximum Iteration 1000 will be used.\n")
  }
  if(missing(threshold)||!is.numeric(threshold))
  {
    threshold<-1e-9
    cat("Default threshold 1e-9 will be used.\n")
  }

  #Dataset
  data.X <- as.matrix(X)
  n <- nrow(data.X)
  p <- ncol(data.X)

  #Initialized Membership Matrix
  if(missing(member.init))
  {
    if (RandomNumber <= 0 || !is.numeric(RandomNumber))
    {
      member.init<-membership(K=K,n=n)
    } else
      member.init<-membership(K=K,n=n,RandomNumber = RandomNumber)
  } else if(!is.membership(member.init))
  {
    member.init<-as.matrix(member.init)
    if(ncol(member.init)!=K ||
       nrow(member.init)!=n)
    {
      if (RandomNumber <= 0 || !is.numeric(RandomNumber))
      {
        member.init<-membership(K=K,n=n)
      } else
        member.init<-membership(K=K,n=n,RandomNumber = RandomNumber)
    } else member.init<-membership(member.init,K=K,n=n)
  } else
    member.init<-as.membership(member.init)
  U<-member(member.init)

  #Initialize Centroid Matrix V (K x p)
  V <- matrix(0,K,p)

  #Distance Matrix
  D <- matrix(0,n,K)
  iteration<-1
  cat("\n")
  repeat{
    cat("\r",paste("iteration:\t",iteration))
    U.old <- U
    D.old <-D
    V.old<-V
    V <- t(U ^ m) %*% data.X / colSums(U ^ m)
    for (k in 1:K)
    {
      #Distance calculation
      for (i in 1:n)
      {
        D[i,k] = t(data.X[i,] - V[k,]) %*%
          (data.X[i,] -V[k,])
      }
    }

    #Update Fuzzy Partition Matrix
    for (i in 1:n)
    {
      U[i,] <- 1 /
        (((D[i,]) ^ (1 / (m - 1))) *
           sum((1 / (D[i,])) ^ (1 /(m - 1))))
    }
    if(any(is.na(U))==T||any(is.infinite(U))==T)
    {
      U<-U.old
      V<-V.old
      D<-D.old
    }
    for (i in 1:n)
      for (k in 1:K) {
        if (U[i,k] < 0)
          U[i,k] = 0
        else if (U[i,k] > 1)
          U[i,k] = 1
      }

    func.obj = sum(U ^ m * D)
    iteration = iteration + 1
    if((max(abs(U.old - U)) <= threshold) ||
       (iteration > max.iteration))
      break
  }

  rownames(U)<-rownames(X)
  label<-apply(U, 1,
               function(x) which.max(x))
  result<-new("fuzzycluster",
              partition=U,
              centroid=V,
              func.obj=func.obj,
              distance=D,
              label=label,
              call.func=as.character(deparse(match.call())),
              fuzzyfier=m,
              method.fuzzy="Fuzzy C-Means"
  )
  cat("\nFinish :)\n")
  if(print.result==1)
  show(result)
  return(result)
}
