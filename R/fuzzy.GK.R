#' Gustafson Kessel Clustering with Babuska Improvisation
#'
#' @description  Gustafson Kessel clustering Algorithm that improved by Babuska for estimating covariance cluster (Babuska, 2002)
#' @param X dataset (matrix/data frame)
#' @param K number of cluster
#' @param m fuzzyfier
#' @param max.iteration maximum iteration for convergence
#' @param threshold convergence criteria
#' @param member.init membership object or matrix that will be used for initialized
#' @param RandomNumber random number for start initializing
#' @param gamma tuning parameter
#' @param rho volume cluster parameter
#' @param print.result print result (0/1)
#' @details This function perform Fuzzy C-Means algorithm by Gustafson Kessel (1968) that improved by Babuska et al (2002).
#' Gustafson Kessel (GK) is one of fuzzy clustering methods to clustering dataset
#' become K cluster. Number of cluster (K) must be greater than 1. To control the overlaping
#' or fuzziness of clustering, parameter m must be specified.
#' Maximum iteration and threshold is specific number for convergencing the cluster.
#' Random Number is number that will be used for seeding to firstly generate fuzzy membership matrix.
#' @details Clustering will produce fuzzy membership matrix (U) and fuzzy cluster centroid (V).
#' The greatest value of membership on data point will determine cluster label.
#' Centroid or cluster center can be use to interpret the cluster. Both membership and centroid produced by
#' calculating mathematical distance. Fuzzy C-Means calculate distance with Covariance Cluster norm distance. So it can be said that cluster
#' will have both sperichal and elipsodial shape of geometry.
#' @details Babuska improve the covariance estimation via tuning covariance cluster
#' with covariance of data. Tuning parameter determine proportion of covariance data and covariance cluster
#' that will be used to estimate new covariance cluster. Beside improving via tuning, Basbuka improve
#' the algorithm with decomposition of covariance so it will become non singular matrix.
#'
#' @references Babuska, R., Veen, P. v., & Kaymak, U. (2002). Improved Covarians Estimation for Gustafson Kessel Clustering. IEEE, 1081-1084.
#' @references Balasko, B., Abonyi, J., & Feil, B. (2002). Fuzzy Clustering and Data Analysis Toolbox: For Use with Matlab. Veszprem, Hungary.
#' @references Gustafson, D. E., & Kessel, W. C. (1978). Fuzzy Clustering With A Fuzzy Covariance Matrix. 761-766.
#' @export
#' @import MASS
fuzzy.GK<- function(X,
                    K,
                    m,
                    gamma,
                    rho,
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
  if(missing(gamma)){
    gamma<-0
    cat("Default Gamma (0) will be used\n")
  }
  if(missing(rho)){
    rho<-rep(1,K)
    cat("Default rho will be used\n")
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

  #Covariance Cluster
  F <- array(0,c(p,p,K))

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
      F[,,k] = as.matrix(0,p,p)
      F.bantu <- F[,,k]
      for (i in 1:n)
      {
        F.bantu = (U[i,k] ^ m) * (data.X[i,] - V[k,]) %*% t((data.X[i,] - V[k,]))+F.bantu
      }
      F.bantu = F.bantu / sum(U[,k] ^ m)
      #Improved estimation covariance by Babuska: Tuning Covariance
      F.bantu = (1 - gamma) * F.bantu + (gamma * (det(cov(data.X))) ^ (1 / p)) * diag(p)
      #Checking wheter Covariance is singular or not
      if (kappa(F.bantu) > 10 ^ 15)
      {
        #Improved estimation covariance by Babuska: Reconstruction
        eig <- eigen(F.bantu)
        eig.values <- eig$values
        eig.vec <- eig$vectors
        eig.val.max <- max(eig.values)
        eig.values[eig.values*( 10 ^ 15 ) < eig.val.max]=eig.val.max/ ( 10 ^ 15)
        F.bantu = eig.vec %*% diag(eig.values) %*% solve(eig.vec)
      }
      detMat= det(F.bantu)
      #Distance calculation
      for (i in 1:n)
      {
        D[i,k] = t(data.X[i,] - V[k,]) %*% (
          (rho[k] * (detMat ^ (1 / p)))*ginv(F.bantu,tol=0)) %*% (data.X[i,] -V[k,])
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
              method.fuzzy="Gustafson Kessel Clustering"
  )
  cat("\nFinish :)\n")
  if(print.result==1)
    show(result)
  return(result)
}
