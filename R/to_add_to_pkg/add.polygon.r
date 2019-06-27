#' Plot up a simple polygon
#' 
#' @param yr range
#' @param y the function input
#' @return nada
#' @author Ana Parma
#' @examples
#' add.polygon(yrs,q1,q2)
#' @export
#'
add.polygon <- function(yrs,v1,v2,col)
{
  polygon(c(yrs,rev(yrs)),c(v1,rev(v2)),col=col,border=NA) 
}
