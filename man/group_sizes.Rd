% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_sizes.R
\name{group_sizes}
\alias{group_sizes}
\title{Computing group sizes from square mixing matrices}
\usage{
group_sizes(mm, directed = TRUE, loops = FALSE)
}
\arguments{
\item{mm}{numeric array with \code{dim} of \eqn{(k, k, 2)} for some \eqn{k}}

\item{directed}{logical, whether network is directed}

\item{loops}{logical, whether loops are allowed}
}
\value{
A numeric vector of group sizes
}
\description{
\code{\link[=group_sizes]{group_sizes()}} recomputes group sizes from a full mixing matrix. This is
only limited to square (single-attribute) mixing matrices.
}
