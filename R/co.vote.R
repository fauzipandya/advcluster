#' Consensus / Ensemble Fuzzy Clustering: Voting
#' @description Provide consensus / ensemble fuzzy clustering with voting method. Several option for voting step provided
#' @details Consensus clustering is method for combine several result of clustering into one robust result. This method used to overcome unstability of cluster result.
#' @details This function perform consensus clustering with voting approach. Voting approach look the domination of membership with several algorithm like sum rule, product rule and borda rule.
#' @details The differences of that method are how to combine several membership. Sum rule use Sum operation. Product rule use Product operation, and Borda use Borda count algorithm.
#' @param object a fuzzycluster_list object
#' @param method voting step that used to combine the partition ("sum","borda","product")
#' @references Sevillano, X., Alias, F., & Socoro, J. C. (2013). Posisional and Confidence voting-based Consensus Function For Fuzzy Cluster Ensemble.
#' @return Fuzzy Consensus Object
#' @slot member membership matrix
#' @slot hard.label hard.label
#' @slot method.consensus method of consensus
#' @examples
#' fuzzy.CM(iris[,1:4],K=2,m=2,max.iteration=20,threshold=1e-3,RandomNumber=1234)->cl1
#' fuzzy.GK(iris[,1:4],K=2,m=2,max.iteration=20,threshold=1e-3,RandomNumber=1234)->cl2
#' fuzzy.CM(iris[,1:4],K=2,m=2,max.iteration=20,threshold=1e-3,RandomNumber=1234)->cl3
#' c_fuzzycluster(cl1,cl2,cl3)->CL
#' co.vote(CL,"borda")
#' @export
co.vote<-function(object, method){
  if(!is(object,"fuzzycluster_list"))
    stop("Object must be fuzzy cluster list")
  if(!(method== "sum"||method== "product"||method=="borda"))
    stop("No specific algorithm inputted.")
  n<-length(pair(object))
  standard<-pair(object)[[1]]
  n.col<-ncol(member(standard))
  n.row<-nrow(member(standard))
  i<-2
  while(i<=n){
    clusterA<-pair(object)[[i]]
    matching<-minWeightBipartiteMatching(clusterA,standard)
    #changing the label
    #tmp-> vector list label
    label.temp<-hard.label(clusterA)
    label.temp2<-label.temp
    tmp <- sapply(1:length(matching), function(y) {
      label.temp[which(label.temp2==y)]<<-matching[y]
    })

    partition.temp2<-member(pair(object)[[i]])
    partition.temp<-partition.temp2

    #labeling the partition label
    for(j in 1:length(tmp)){
      partition.temp[,j]=partition.temp2[,tmp[j]]
    }

    object@pair[[i]]@member<-partition.temp
    i<-i+1
  }
  i<-2
  #Consensus step
  if(method=="product" || method=="sum"){
    partition.ensemble<-member(pair(object)[[1]])
    if(method=="sum")
    {for(i in 2:n)
      partition.ensemble<-member(pair(object)[[i]])+partition.ensemble
    } else if(method=="product")
    {for(i in 2:n)
      partition.ensemble<-member(pair(object)[[i]])*partition.ensemble
    }
    #edit U to satisfy U condition
    partition.ensemble<-partition.ensemble/rowSums(partition.ensemble)
  }
  else if(method=="borda"){
    borda.ensemble<-matrix(0,nrow =n.row,ncol=n.col )
    for(i in 1:n){
      rank.partition<-t(apply(member(pair(object)[[i]]),
                              1,
                              rank))
      borda.ensemble<-borda.ensemble+rank.partition
    }
    partition.ensemble<-borda.ensemble/rowSums(borda.ensemble)
  }
  label.ensemble<-apply(partition.ensemble,1,which.max)
  method.consensus<-paste("Vote-",method,sep="")
  result<-new("co_fuzzycluster",
              member=partition.ensemble,
              hard.label=label.ensemble,
              method.consensus=method.consensus)
  return(result)
}