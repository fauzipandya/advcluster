#' Print membership
#' @description Print membership object
#' @param x membership object
#' @param ... another parameter
#' @export
#' @import knitr
print.membership<-function(x,...){
  cat("Membership Matrix:")
  member.matrix<-member(x)
  colnames(member.matrix)<-paste("Cluster ",
                                 c(1:ncol(member.matrix)),
                                 sep="")
  rownames(member.matrix)<-paste("Obs ",
                                 c(1:nrow(member.matrix)),
                                 sep="")
  print(kable(member.matrix,digits = 3))
  cat("hard.label:")
  hard.label<-hard.label(x)
  names(hard.label)<-rownames(member.matrix)
  print(kable(as.data.frame(hard.label)))
}
