% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Gaussian}
\alias{Gaussian}
\alias{Multiquadric}
\alias{InverseQuadratic}
\alias{InverseMultiquadric}
\alias{Bump}
\title{Radial Kernels}
\usage{
Gaussian(r, scale = 1)

Multiquadric(r, scale = 1)

InverseQuadratic(r, scale = 1)

InverseMultiquadric(r, scale = 1)

Bump(r, scale = 1)
}
\arguments{
\item{r}{a positive numeric object}

\item{scale}{a positive number for scaling}
}
\value{
an object of the same size of \code{r}
}
\description{
Utility functions for radial kernels.
}
\section{Functions}{
\itemize{
\item \code{Gaussian}: \eqn{\exp(- r^2)}

\item \code{Multiquadric}: \eqn{\sqrt{1 + r^2}}

\item \code{InverseQuadratic}: \eqn{1 / (1 + r^2)}

\item \code{InverseMultiquadric}: \eqn{1 / \sqrt{1 + r^2}}

\item \code{Bump}: \eqn{\exp(- 1 / (1 - r^2)))} when \eqn{r < 1}, and \eqn{0} otherwise.
Note that it is not scaled (\code{Bump(0)} < 1) and it is compactly supported.
}}

\examples{
Gaussian(0 : 6, scale = 2)
}
