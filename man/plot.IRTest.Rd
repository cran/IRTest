% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.irtest.R
\name{plot.IRTest}
\alias{plot.IRTest}
\title{Plot of the estimated latent distribution}
\usage{
\method{plot}{IRTest}(x, ...)
}
\arguments{
\item{x}{An object of \code{"IRTest"}-class obtained from either \code{\link{IRTest_Dich}}, \code{\link{IRTest_Poly}}, \code{\link{IRTest_Cont}}, or \code{\link{IRTest_Mix}}.}

\item{...}{Other aesthetic argument(s) for drawing the plot.
Arguments are passed on to \code{ggplot2::stat_function}, if the distribution estimation method is 2NM, KDE, or DC.
Otherwise, they are passed on to \code{ggplot2::geom_line}.}
}
\value{
A plot of estimated latent distribution.
}
\description{
This function draws a plot of the estimated latent distribution (the population distribution of the latent variable).
}
\examples{
\donttest{
# Data generation and model fitting

data <- DataGeneration(N=1000,
                       nitem_D = 15,
                       latent_dist = "2NM",
                       d = 1.664,
                       sd_ratio = 2,
                       prob = 0.3)$data_D

M1 <- IRTest_Dich(data = data, latent_dist = "KDE")

# Plotting the latent distribution

plot(x = M1, linewidth = 1, color = 'red') +
  ggplot2::lims(x = c(-6, 6), y = c(0, .5))
}
}
\author{
Seewoo Li \email{cu@yonsei.ac.kr}
}
