#' Membership
#' @import methods
#' @name membership-class
#' @rdname membership-class
#' @slot member membership matrix
#' @slot hard.label vector of hard hard.labeling
#' @exportClass membership

setClass("membership",
         representation = representation(member="matrix",
                                         hard.label="vector")
)
setValidity("membership",
            function(object){
              if(length(hard.label(object))!= nrow(member(object)))
                return("hard.label Mismatch\n")
              if(any(member(object)<0 ||member(object)>1))
                return("Constraint of membership matrix violated\n")
              member.object<-round(member(object),5)
              rowSum.membership<-rowSums(member.object)
              if(any(rowSum.membership!=1))
                return("Constraint of membership matrix violated\n")
              if(any(!is.numeric(member(object))))
                return("Not Numeric")
              if(anyNA(object))
                return("Missing value on membership detected\n")
            })

