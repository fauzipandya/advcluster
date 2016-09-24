init.V.med<-function(V,X,K){
V<-matrix(ncol=K,nrow=p)
ind.pick<-sample(nrow(X),1)
ind.k=1
V[ind.k,]<-as.matrix(X[ind.pick,])
while(ind.k<K)
{
  if(ind.k==1)
  {
    diss.check.V<-apply(X,
                        1,
                        function(x) dissimilarity(V[ind.k,],x,diss.method))
    diss.check.V[ind.pick]<- Inf
    ind<-which.min(diss.check.V)
    ind.pick<-c(ind.pick,ind)
    ind.k<-ind.k+1
    V[ind.k,]<-as.matrix(X[ind,])
  } else {
    b=1
    diss.check.V<-apply(X,
                        1,
                        function(x)
                          dissimilarity(V[b,],x,diss.method))
    while(b<ind.k)
    {
      b=b+1
      diss.check.V.temp<-apply(X,
                               1,
                               function(x)
                                 dissimilarity(V[b,],x,diss.method))
      diss.check.V<-cbind(diss.check.V,diss.check.V.temp)
    }
    diss.check.V.min<-apply(diss.check.V,1,min)
    diss.check.V.min[ind.pick]<- -Inf
    ind<-which.max(diss.check.V.min)
    ind.pick<-c(ind.pick,ind)
    ind.k<-ind.k+1
    V[ind.k,]<-as.matrix(X[ind,])
  }
}
return(V)
}