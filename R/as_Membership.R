#' As Membership/Membership Matrix
#' @description Convert matrix to membership matrix
#' @param  member membership matrix
#' @return Membership Membership object
#' @slot member membership matrix
#' @slot hard.label hard label
#' @export
"as.membership"<-function(member){
  ifelse(is.membership(member),
         return(member),
         return(membership(member,
                       K=ncol(member),
                       n=nrow(member))))
}
