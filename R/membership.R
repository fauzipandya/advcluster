#' Make an Partition Matrix
#' @param member membership matrix
#' @param K number of cluster
#' @param n number of observation
#' @param RandomNumber random number/seed used
#' @export
"membership"<-function(member,K,n,RandomNumber=0){
  if(missing(K))
  {
    cat("\nNumber Of Cluster undefined.Default number of cluster(2) will be used\n")
    K<-2
  }

  if(missing(n))
    stop("Specify number of data first")

  if(missing(member)||
     ncol(member)!=K||
     nrow(member)!=n)
  {
    cat("\nMembership initialiazed randomly\n")
    if(RandomNumber<0 || !is.numeric(RandomNumber))
      RandomNumber<-0
    if (RandomNumber > 0)
      set.seed(RandomNumber)
    member <- matrix(runif(n * K,0,1),n,K)
    while(any(rowSums(member)!=1))
      member <- member / rowSums(member)
  }
  hard.label<-apply(member,
               1,
               function(x){which.max(x)})

  result<-new("membership",
              member=(member),
              hard.label=(hard.label))
  return(result)
}
