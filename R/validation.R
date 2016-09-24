#' Validation Index
#' @description Validation index for validating fuzzy clustering result
#' @param object fuzzy clustering object
#' @details This function provide several validation indexs that calculated from fuzzy clustering result. Validation index can be used for choose best optimum parameter.
#' @details There are PC, MPC, CE, S, Xie Beni, Kwon, and Tang index. PC (Partition Coefficient), MPC (Modified Partition Coefficient), and CE (Classification Entropy) are calculated from membership matrix. S (Separation Index), Xie Beni, Kwon, and Tang use both distance and membership matrix.
#' @details The best cluster result can be decided with minimum value of index, except MPC and PC use maximum value.
#' @references Wang, W., & Zhang, Y. (2007). On Fuzzy Cluster Validity Indices. Fuzzy Sets and System, 2095-2117.
#' @author Achmad Fauzi Bagus F
#' @return validation index object.
#' @slot XB Xie Beni Index
#' @slot PC Partition Coef.
#' @slot MPC Modifief Partition Coef.
#' @slot Kwon Kwon Index
#' @slot Tang Tang Index
#' @slot S Separation Index
#' @slot CE Classification Entropy
#' @export
#'
#' @examples
#' fuzzy.CM(iris[,1:4],K=3,m=2,max.iteration=100,threshold=1e-5,RandomNumber=1234)->cl
#' validation.index(cl)->valid
#' #example for Xie Beni index
#' XB(valid)
validation.index<-function(object){
  if(!is(object,"fuzzycluster"))
    stop("This function just for Fuzzy Clustering Result")
  #Parameter Initialized
  U<-member(object)
  K<-ncol(U)
  n<-nrow(U)
  D<-distance(object)
  V<-centroid(object)
  m<-fuzzyfier(object)
  p<-ncol(V)

  PC<-sum(U^2)/n

  MPC<-(1-(K/(K-1))*(1-PC))

  CE<-10^10
  try(CE<-(-1)*sum(U*log(U,base=exp(1)))/n,silent = T)

  XB<-10^10
  try({
    XB.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        XB.temp1[i,k]<-D[i,k]*(U[i,k]^m)

      XB.temp2<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          XB.temp2[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
      XB.min<-min(XB.temp2[lower.tri(XB.temp2)])
      XB<-sum(XB.temp1)/(XB.min*n)
  },silent=T)

  S<-10^10
  try({
    S.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        S.temp1[i,k]<-D[i,k]*(U[i,k]^2)
      S.temp2<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          S.temp2[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
      S.min<-min(S.temp2[lower.tri(S.temp2)])
      S<-sum(S.temp1)/(S.min*n)
  },silent=T)

  V.bar<-colMeans(V)
  Kwon<-10^10
  try({
    Kwon.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        Kwon.temp1[i,k]<-D[i,k]*(U[i,k]^m)
      Kwon.temp2<-matrix(0,1,K)
      for(k in 1:K)
        Kwon.temp2[1,k]<-t(V[k,]-V.bar)%*%(V[k,]-V.bar)
      Kwon.temp3<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          Kwon.temp3[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
      Kwon.min<-min(Kwon.temp3[lower.tri(Kwon.temp3)])
      Kwon<-(sum(Kwon.temp1)+sum(Kwon.temp3)/K)/(Kwon.min)
  },silent=T)

  Tang<-10^10
  try({
    Tang.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        Tang.temp1[i,k]<-D[i,k]*(U[i,k]^m)
    Tang.temp2<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          Tang.temp2[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
    Tang.min<-min(Tang.temp2[lower.tri(Tang.temp2)])
    Tang<-(sum(Tang.temp1)+(sum(Tang.temp2)/(K*(K-1))))/(Tang.min)
  },silent=T)

  result<-new("validation",S=S,XB=XB,Tang=Tang,PC=PC,MPC=MPC,CE=CE,Kwon=Kwon)
  result
}
