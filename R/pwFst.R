#' pwFst
#'
#' Calculate pairwise F-statistics between each population
#' @references Sewal Wright. Genetical Structure of Populations. Nature, 166:247–249, 1950
#' @param p allele frequencies \code{data.frame} with row and column names
#' @return a symetric pairwise Fst table of class \code{data.frame}
#' @export

pwFst <- function (p){
  #create an empty matrix filled with 1
  mfst = matrix( rep( 0, ncol(p)^2), nrow = ncol(p), ncol = ncol(p) )
  popName = colnames(p)
  colnames(mfst) = popName
  rownames(mfst) = popName
  max = ncol(p)

  #compute pairwise Fst
  for (i in 1:(max-1) ){
    for ( j in (i+1):max ){
     fst =  fst( data.frame( p[,i], p[,j] ) )
     mfst[i,j] = median(fst)
     mfst[j,i] = median(fst)
    }
  }
  return (as.data.frame(mfst))
}
