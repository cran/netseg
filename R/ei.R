#' Krackhard and Stern's E-I index
#'
#' An index proposed by Krackhard and Stern (1988) to capture relative
#' prevalence of between- and within-group ties. From that perspective it can
#' be interpreted as a measure of network segregation.
#'
#' @template mm-igraph-methods
#'
#' @param object R object, see Details for available methods
#' @param ... other arguments passed to/from other methods
#'
#' @return Numerical value of the E-I index.
#'
#' @references Krackhardt, D., & Stern, R. N. (1988). Informal networks and
#'   organizational crises: An experimental simulation. *Social Psychology
#'   Quarterly*, 123-140.
#'
#' @export
#' @family segregation measures
#'
#' @examples
#' ei(WhiteKinship, "gender")
ei <- function(object, ...) UseMethod("ei")





#' @rdname ei
#' @export
ei.table <- function(object, ...)
{
  # extract contact layer
  if(length(dim(object)) == 3)
    m <- object[,,2]
  else m <- object
  p <- m / sum(m)
  pinternal <- sum(diag(p))
  diag(p) <- 0
  pexternal <- sum(p)
  return( pexternal - pinternal )
}

#' @rdname ei
#' @param vattr character scalar or vector of length equal to the size of
#'   `object`, vertex attribute for which mixing matrix is to be computed
#' @param directed logical whether the network is directed
#' @param loops logical, whether loops are allowed
#' @export
ei.igraph <- function(object, vattr, directed=is.directed(object),
                      loops=any(is.loop(object)), ...)
{
  m <- mixingm(object, rattr=vattr, directed=directed, loops=loops)
  ei(m, ...)
}






#' @rdname ei
#' @export
ei.default <- function(object, ...)
{
  ei.table( as.table(object), ...)
}
