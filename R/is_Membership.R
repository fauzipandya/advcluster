#' Check membership Matrix/Object
#' @description checking object is membership object or not
#' @param object an object that used for membership checking
#' @export
#' @return T/F
is.membership<-function(object){
  is(object,"membership")
  }
