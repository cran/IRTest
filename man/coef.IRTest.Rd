% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.IRTest.R
\name{coef.IRTest}
\alias{coef.IRTest}
\title{Extract Model Coefficients}
\usage{
\method{coef}{IRTest}(object, complete = TRUE, ...)
}
\arguments{
\item{object}{An object for which the extraction of model coefficients is meaningful.}

\item{complete}{A logical value indicating if the full coefficient vector should be returned.}

\item{...}{Other arguments.}
}
\value{
Coefficients extracted from the model (\code{object}).
}
\description{
A generic function which extracts model coefficients from objects returned by modeling functions.
}
