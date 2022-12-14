% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist2.R
\name{dist2}
\alias{dist2}
\title{Re-parameterized two-component normal mixture distribution}
\usage{
dist2(x, prob = 0.5, d = 0, sd_ratio = 1, overallmean = 0, overallsd = 1)
}
\arguments{
\item{x}{A numeric vector. The location to evaluate the density function.}

\item{prob}{A numeric value of \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees who belong to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).}

\item{d}{A numeric value of \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
where \eqn{\mu_1} is the estimated mean of the first Gaussian component,
\eqn{\mu_2} is the estimated mean of the second Gaussian component,
and \eqn{\bar{\sigma}} is the standard deviation of the latent distribution (Li, 2021).
Without loss of generality, \eqn{\mu_2 \ge \mu_1}, thus \eqn{\delta \ge 0}, is assumed.}

\item{sd_ratio}{A numeric value of \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} is the estimated standard deviation of the first Gaussian component, \eqn{\sigma_2} is the estimated standard deviation of the second Gaussian component (Li, 2021).}

\item{overallmean}{A numeric value of \eqn{\bar{\mu}} that determines the overall mean of two-component Gaussian mixture distribution.}

\item{overallsd}{A numeric value of \eqn{\bar{\sigma}} that determines the overall standard deviation of two-component Gaussian mixture distribution.}
}
\value{
The evaluated probability density value(s).
}
\description{
Probability density for the re-parameterized two-component normal mixture distribution.
}
\details{
\describe{
\item{The overall mean and overall standard deviation can be expressed with original parameters of two-component Gaussian mixture distribution;}{
1) Overall mean (\eqn{\bar{\mu}})
\deqn{\bar{\mu}=\pi\mu_1 + (1-\pi)\mu_2}

2) Overall standard deviation (\eqn{\bar{\sigma}})
\deqn{\bar{\sigma}=\sqrt{\pi\sigma_{1}^{2}+(1-\pi)\sigma_{2}^{2}+\pi(1-\pi)(\mu_2-\mu_1)^2}}
}
}
}
\references{
Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
}
\author{
Seewoo Li \email{cu@yonsei.ac.kr}
}
