#' Validation Class
#' @import methods
#' @name validation-class
#' @rdname validation-class
#' @slot PC Partition Coefficient index
#' @slot MPC Modified Partition Coefficient index
#' @slot CE Classification Entropy index
#' @slot S Separation index
#' @slot XB Xie Beni index
#' @slot Kwon Kwon index
#' @slot Tang Tang index
#' @exportClass  validation
setClass("validation",
         representation = representation(PC="numeric",
                                         MPC="numeric",
                                         CE="numeric",
                                         S="numeric",
                                         XB="numeric",
                                         Kwon="numeric",
                                         Tang="numeric"
         ),
         prototype = list(PC=numeric(),
                          MPC=numeric(),
                          CE=numeric(),
                          S=numeric(),
                          XB=numeric(),
                          Kwon=numeric(),
                          Tang=numeric()))
setMethod("show","validation",function(object){
  print.validation(object)
})
