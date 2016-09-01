#' Print Fuzzy Clustering Result
#' @description Print Fuzzy Clustering
#' @param x fuzzy clustering object
#' @param ... another paramater
#' @import knitr
#' @export
print.fuzzycluster<-function(x,...){
  cat(paste("Function call:",call.func(x),sep=" "))
  cat(paste("\n"),method.fuzzy(x),sep="")
  cat(paste("\nFunction objective:",func.obj(x),sep=" "))

  cat("\nMembership & Label Matrix:")
  member.matrix<-partition(x)
  colnames(member.matrix)<-paste("Cluster ",
                                 c(1:ncol(member.matrix)),
                                 sep="")
  Label<-label(x)
  member.matrix<-cbind(member.matrix,Label)
  rownames(member.matrix)<-paste("Obs ",
                                 c(1:nrow(member.matrix)),
                                 sep="")
  print(kable(member.matrix,digits = 3))
  center<-centroid(x)
  cat("\nCentroid:")
  rownames(center)<-paste("Cluster ",
                          c(1:nrow(center)),
                          sep="")
  print(kable(center,digits = 3))

}
