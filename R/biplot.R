#' Biploting Fuzzy Cluster Result
#' @details Make Visualization Biplot from fuzzy cluster / consensus fuzzy cluster analysis result
#' @param object a cluster object
#' @param data.X a data matrix that used for clustering
#' @param scale scaling option (T/F)
#' @import ggplot2
#' @importFrom stats prcomp
#' @export
#' @examples
#' fuzzy.CM(iris[,1:4],K=3,m=2,max.iteration=100,threshold=1e-5,RandomNumber=1234)->cl1
#' biploting(cl1,iris[,1:4])
biploting <- function(object,data.X,scale) {
  if(missing(object)||missing(data.X))
    stop("No object / data detected")
  if(length(hard.label(object))!=nrow(data.X))
    stop("row of cluster not match")
  if(!(is(object,"fuzzycluster")||is(object,"co_fuzzycluster")))
    stop("Cluster is not fuzzycluster class")
  if(missing(scale))
  {
    cat("\nScale default:True")
    scale=T}
  data.X<-as.matrix(data.X)
  label<-hard.label(object)
  data.PCA <- prcomp(data.X,scale. = scale)
  z1 <- as.data.frame(cbind(data.PCA$x[,1:2],label))
  datapc <- data.frame(varnames=rownames(data.PCA$rotation),
                       data.PCA$rotation)
  mult <- min(
    (max(z1[,"PC1"]) - min(z1[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"]))),
    (max(z1[,"PC2"]) - min(z1[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"])))
  )

  datapc <- transform(datapc,
                      v1 = .7 * mult * (get("PC1")),
                      v2 = .7 * mult * (get("PC2"))
  )
  PC1<-z1$PC1
  PC2<-z1$PC2
  pl<-ggplot(z1,
         aes(x = PC1,y = PC2,color=factor(label))) +
    geom_point() +
    labs(color="Cluster")+
    xlab(paste("PC 1 \nVariance Explained: ",
               round(summary(data.PCA)$importance[2,1] *100,2),"%")) +
    ylab(paste("PC 2 \nVariance Explained: ",
               round(summary(data.PCA)$importance[2,2] *100,2),"%"))+
    theme_bw(base_size = 15)+
    coord_equal(ratio = 1)+
    geom_text(data=datapc, aes_string(x="v1", y="v2", label="varnames"),
              size = 5, vjust=1, color="navy",check_overlap = F)+
    geom_segment(data=datapc, aes_string(x="0", y="0", xend="v1", yend="v2"),
                 arrow=arrow(length=unit(0.3,"cm")), color="navy")
  pl<-pl+
    geom_text(data=z1,
              aes(color=factor(label),
                  label=rownames(z1)),
              check_overlap = F,size=5)
  print(pl)
  return(pl)
}
