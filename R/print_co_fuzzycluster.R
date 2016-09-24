#' Print Consensus Fuzzy Clustering Result
#' @description Print Consensus Fuzzy Clustering
#' @param x consensus fuzzy clustering object
#' @param ... another paramater
#' @import knitr
#' @export
print.co_fuzzycluster<-function(x,...){
  cat(paste("Method of Consensus used:\t"),
      method.consensus(x),
      sep="")

  cat("\nEnsemble/Consensus Membership:")
  member.matrix<-member(x)
  colnames(member.matrix)<-paste("Cluster ",
                                 c(1:ncol(member.matrix)),
                                 sep="")
  rownames(member.matrix)<-paste("Obs ",
                                 c(1:nrow(member.matrix)),
                                 sep="")
  print(kable(member.matrix,digits = 3))

  cat("\nEnsemble/Consensus Label:")
  label<-hard.label(x)
  names(label)<-rownames(member.matrix)
  print(kable(as.data.frame(label)))

}

