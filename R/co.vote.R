#' Consensus / Ensemble Fuzzy Clustering: Voting
#' @description Provide consensus / ensemble fuzzy clustering with voting method. Several option for voting step provided
#' @param object a fuzzycluster_list object
#' @param method voting step that used to combine the partition
#' @export
co.vote<-function(object, method){
  if(!is(object,"fuzzycluster_list"))
    stop("Object must be fuzzy cluster list")
  n<-length(pair(object))
  standard<-pair(object)[[1]]
  n.col<-ncol(partition(standard))
  n.row<-nrow(partition(standard))
  i<-2
  while(i<=n){
    clusterA<-pair(object)[[i]]
    matching<-minWeightBipartiteMatching(clusterA,standard)
    #changing the label
    #tmp-> vector list label
    label.temp<-label(clusterA)
    label.temp2<-label.temp
    tmp <- sapply(1:length(matching), function(y) {
      label.temp[which(label.temp2==y)]<<-matching[y]
    })

    partition.temp2<-partition(pair(object)[[i]])
    partition.temp<-partition.temp2

    #labeling the partition label
    for(j in 1:length(tmp)){
      partition.temp[,j]=partition.temp2[,tmp[j]]
    }

    object@pair[[i]]@partition<-partition.temp
    i<-i+1
  }
  i<-2
  #Consensus step
  if(method=="product" || method=="sum"){
    partition.ensemble<-partition(pair(object)[[1]])
    if(method=="sum")
    {for(i in 2:n)
      partition.ensemble<-partition(pair(object)[[i]])+partition.ensemble
    } else if(method=="product")
    {for(i in 2:n)
      partition.ensemble<-partition(pair(object)[[i]])*partition.ensemble
    }
    #edit U to satisfy U condition
    partition.ensemble<-partition.ensemble/rowSums(partition.ensemble)
  }
  else if(method=="borda"){
    borda.ensemble<-matrix(0,nrow =n.row,ncol=n.col )
    for(i in 1:n){
      rank.partition<-t(apply(partition(pair(object)[[i]]),
                              1,
                              rank))
      borda.ensemble<-borda.ensemble+rank.partition
    }
    partition.ensemble<-borda.ensemble/rowSums(borda.ensemble)
  }
  label.ensemble<-apply(partition.ensemble,1,which.max)
  method.consensus<-paste("Vote-",method,sep="")
  result<-new("co_fuzzycluster",
              partition.ensemble=partition.ensemble,
              label.ensemble=label.ensemble,
              method.consensus=method.consensus)
  return(result)
}