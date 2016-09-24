fuzzy.CM<- function(X,
                    K,
                    m,
                    max.iteration,
                    threshold,
                    member.init,
                    RandomNumber=0,
                    print.result=0,
                    diss.method="Cosine")
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
      cat("Membership not applicable with other parameter")
      if (RandomNumber <= 0 || !is.numeric(RandomNumber))
      {
        member.init<-membership(K=K,n=n)
      } else
        member.init<-membership(K=K,n=n,RandomNumber = RandomNumber)
    } else member.init<-membership(member.init,K=K,n=n)
  } else
  {
    if(ncol(member(member.init))!=K||nrow(member(member.init))!=n)
    {
      cat("Membership not applicable with other parameter")
      if (RandomNumber <= 0 || !is.numeric(RandomNumber))
      {
        member.init<-membership(K=K,n=n)
      } else
        member.init<-membership(K=K,n=n,RandomNumber = RandomNumber)
    } else
      member.init<-as.membership(member.init)
  }
  U<-member(member.init)

  #Initialize Centroid Matrix V (K x p)
  V<-matrix(ncol=p,nrow=K)
  index.picked<-sample(nrow(X),1)
  V<-as.matrix(X[index.picked,])
  ind.k=1
  while(ind.k<=K)
  {
    if(ind.k==1)
      {
      apply(X,1,function(x) dissimilarity(x,V,diss.method))
      }
  }

















  V<-as.matrix(X[sample(nrow(X),K),])

  #Distance Matrix
  D <- matrix(0,n,K)
  iteration<-1
  cat("\n")
  repeat{
    cat("\r",paste("iteration:\t",iteration))
    U.old <- U
    D.old <-D
    V.old<-V
    vector.V<-vector(mode="numeric",length = K)


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
              member=U,
              centroid=V,
              func.obj=func.obj,
              distance=D,
              hard.label=label,
              call.func=as.character(deparse(match.call())),
              fuzzyfier=m,
              method.fuzzy="Fuzzy C-Means"
  )
  cat("\nFinish :)\n")
  if(print.result==1)
    show(result)
  return(result)
}
