% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ProximalElastic}
\alias{ProximalElastic}
\title{Proximal Mapping for Elastic Net}
\usage{
ProximalElastic(z, lambda = 0, alpha = 1)
}
\arguments{
\item{z}{input numeric object}

\item{lambda}{regularization coefficient}

\item{alpha}{elastic net mixing parameter between 0 (ridge) and 1 (lasso)}
}
\value{
values after proximal mapping
}
\description{
Proximal Mapping for Elastic Net
}
\examples{
ProximalElastic(c(-1, 0, 2, 3), 1)
}
