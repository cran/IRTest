% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_item.R
\name{plot_item}
\alias{plot_item}
\title{Plot of item response functions}
\usage{
plot_item(x, item.number = 1, type = NULL)
}
\arguments{
\item{x}{A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, \code{IRTest_Cont}, or \code{IRTest_Mix}.}

\item{item.number}{A numeric value indicating the item number.}

\item{type}{A character string required if \code{inherits(x, c("mix")) == TRUE}.
It should be either \code{"d"} (dichotomous item) or \code{"p"} (polytomous item); \code{item.number=1, type="d"} indicates the first dichotomous item.}
}
\value{
This function returns a plot of item response functions.
}
\description{
This function draws item response functions of an item of the fitted model.
}
\examples{
\donttest{
# A preparation of dichotomous item response data

data <- DataGeneration(N=500, nitem_D = 10)$data_D

# Analysis

M1 <- IRTest_Dich(data)

# Plotting item response function

plot_item(M1, item.number = 1)
}
}
\author{
Seewoo Li \email{cu@yonsei.ac.kr}
}
