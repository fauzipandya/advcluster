#' Print Validation Index
#' @description Print Validation Index for fuzzy clustering
#' @param x validation object
#' @param ... another parameter
#' @import knitr
#' @export
print.validation<-function(x,...){
  cat("Validation Index result:")
  valid<-as.data.frame(c(PC(x),
                         MPC(x),
                         CE(x),
                         XB(x),
                         S(x),
                         Kwon(x),
                         Tang(x)))
  colnames(valid)<-"Value"
  rownames(valid)<-c("Partition Coefficient",
                     "Modified Partition Coefficient",
                     "Classification Entropy",
                     "Xie Beni",
                     "Separation",
                     "Kwon",
                     "Tang" )
  print(kable(valid,digits = 3))

}
