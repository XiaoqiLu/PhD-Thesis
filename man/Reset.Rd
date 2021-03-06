% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base.R
\name{Reset}
\alias{Reset}
\alias{Reset.Env}
\title{Reset the Environment}
\usage{
Reset(x)

\method{Reset}{Env}(x)
}
\arguments{
\item{x}{an environment-like object}
}
\value{
the environment-like object with seed set
}
\description{
\code{Reset()} is a generic method that aims to reset an object.
}
\section{Methods (by class)}{
\itemize{
\item \code{Env}: Reset for an environment object, changing its \code{internal_state}
}}

\examples{
ev <- Env()
ev <- Step(ev, 2e5)
ev <- Reset(ev)
}
