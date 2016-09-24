#' Dissimilarity Measurement of 2 Vector
#'
#' @description Dissimilarity measurement of 2 vector with several algorithm.
#' @param vector.1 1st vector
#' @param vector.2 2nd vector
#' @param method method/algorithm that used to measurement
#'
#' @return diss dissimilarity value
#' @details Both vector must be numeric or integer. Method that provide are "Jaccard" for Jaccard Index Dissimilarity, "Cosine" for cosine dissimilarity, and "Euclideans" for euclideans dissimilarity.
dissimilarity<-function(vector.1,vector.2,method){

  if(length(vector.1)!=length(vector.2))
    stop("Two vector have different length")
  if(method!="Jaccard" && method!="Cosine" && method!="Euclideans")
    stop("method not provided")

  if(method=="Jaccard"){
    sum.min=0
    sum.max=0
    for(i in 1:length(vector.1))
    {
      sum.min=sum.min+min(vector.1[i],vector.2[i])
      sum.max=sum.max+max(vector.1[i],vector.2[i])
    }
    diss=sum.min/sum.max
  } else if(method=="Cosine"){
    diss=t(vector.1)%*%vector.2
    diss= diss/(norm(vector.1,type="2")*norm(vector.2,type="2"))
  } else if(method=="Euclideans"){
    diss=t(vector.1-vector2)%*%(vector1-vector2)
  }
  return(diss)
}