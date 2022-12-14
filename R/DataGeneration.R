#' Generating artificial item response data
#'
#' @description This function generates artificial item response data with users specified item types, details of item parameters, and latent distribution.
#'
#' @importFrom betafunctions rBeta.4P
#' @importFrom stats dnorm rbinom rchisq rnorm runif
#'
#' @param seed A numeric value that is used on random sampling.
#' The seed number can guarantee the replicability of the result.
#' @param N A numeric value. The number of examinees.
#' @param nitem_D A numeric value. The number of dichotomous items.
#' @param nitem_P A numeric value. The number of polytomous items.
#' @param model_D A vector of length \code{nitem_D}.
#' The \emph{i}th element is the probability model for the \emph{i}th dichotomous item.
#' @param model_P A vector of length \code{nitem_P}.
#' The \emph{i}th element is the probability model for the \emph{i}th polytomous item.
#' @param latent_dist A character string that determines the type of latent distribution.
#' Currently available options are \code{"beta"} (four-parameter beta distribution; \code{\link{rBeta.4P}}),
#' \code{"chi"} (\eqn{\chi^2} distribution; \code{\link{rchisq}}),
#' \code{"normal"} (standard normal distribution; \code{\link{rnorm}}),
#' and \code{"Mixture"} (two-component Gaussian mixture distribution; see Li (2021) for details.)
#' @param prob A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\pi = \frac{n_1}{N}} parameter of two-component Gaussian mixture distribution, where \eqn{n_1} is the estimated number of examinees who belong to the first Gaussian component and \eqn{N} is the total number of examinees (Li, 2021).
#' @param d A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\delta = \frac{\mu_2 - \mu_1}{\bar{\sigma}}} parameter of two-component Gaussian mixture distribution,
#' where \eqn{\mu_1} is the estimated mean of the first Gaussian component,
#' \eqn{\mu_2} is the estimated mean of the second Gaussian component,
#' and \eqn{\bar{\sigma} = 1} is the standard deviation of the latent distribution (Li, 2021).
#' Without loss of generality, \eqn{\mu_2 \ge \mu_1}, thus \eqn{\delta \ge 0}, is assumed.
#' @param sd_ratio A numeric value required when \code{latent_dist = "Mixture"}.
#' It is a \eqn{\zeta = \frac{\sigma_2}{\sigma_1}} parameter of two-component Gaussian mixture distribution, where \eqn{\sigma_1} is the estimated standard deviation of the first Gaussian component, \eqn{\sigma_2} is the estimated standard deviation of the second Gaussian component (Li, 2021).
#' @param a_l A numeric value. The lower bound of item discrimination parameters (\emph{a}).
#' @param a_u A numeric value. The upper bound of item discrimination parameters (\emph{a}).
#' @param c_l A numeric value. The lower bound of item guessing parameters (\emph{c}).
#' @param c_u A numeric value. The lower bound of item guessing parameters (\emph{c}).
#' @param categ A numeric vector of length \code{nitem_P}.
#' The \emph{i}th element equals the number of categories of the \emph{i}th polyotomous item.
#'
#' @return This function returns a \code{list} which contains several objects:
#' \item{theta}{A vector of ability parameters (\eqn{\theta}).}
#' \item{item_D}{A matrix of dichotomous item parameters.}
#' \item{initialitem_D}{A matrix that contains initial item parameter values for dichotomous items.}
#' \item{data_D}{A matrix of dichotomous item responses where rows indicate examinees and columns indicate items.}
#' \item{item_P}{A matrix of polytomous item parameters.}
#' \item{initialitem_P}{A matrix that contains initial item parameter values for polytomous items.}
#' \item{data_P}{A matrix of polytomous item responses where rows indicate examinees and columns indicate items.}
#'
#' @export
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#' Li, S. (2021). Using a two-component normal mixture distribution as a latent distribution in estimating parameters of item response models. \emph{Journal of Educational Evaluation, 34}(4), 759-789.
#'
#'
#' @examples
#' # Dichotomous item responses only
#'
#' Alldata <- DataGeneration(seed = 1,
#'                           model_D = rep(3, 10),
#'                           N=500,
#'                           nitem_D = 10,
#'                           nitem_P = 0,
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_D
#' item <- Alldata$item_D
#' initialitem <- Alldata$initialitem_D
#' theta <- Alldata$theta
#'
#'
#' # Polytomous item responses only
#'
#' Alldata <- DataGeneration(seed = 2,
#'                           N=1000,
#'                           nitem_D = 0,
#'                           nitem_P = 10,
#'                           categ = rep(3:7,each = 2),
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' data <- Alldata$data_P
#' item <- Alldata$item_P
#' initialitem <- Alldata$initialitem_P
#' theta <- Alldata$theta
#'
#'
#' # Mixed-format items
#'
#' Alldata <- DataGeneration(seed = 2,
#'                           model_D = rep(1:2, each=10),# 1PL model is applied to item #1~10
#'                                                       # and 2PL model is applied to item #11~20.
#'                           N=1000,
#'                           nitem_D = 20,
#'                           nitem_P = 10,
#'                           categ = rep(3:7,each = 2),# 3 categories for item #21-22,
#'                                                     # 4 categories for item #23-24,
#'                                                     # ...,
#'                                                     # and 7 categories for item #29-30.
#'                           d = 1.664,
#'                           sd_ratio = 2,
#'                           prob = 0.3)
#'
#' DataD <- Alldata$data_D
#' DataP <- Alldata$data_P
#' itemD <- Alldata$item_D
#' itemP <- Alldata$item_P
#' initialitemD <- Alldata$initialitem_D
#' initialitemP <- Alldata$initialitem_P
#' theta <- Alldata$theta
#'
DataGeneration <- function(seed=1, N=2000, nitem_D=NULL, nitem_P=NULL, model_D, model_P="GPCM",
                           latent_dist="Mixture",
                           prob=0.5, d=1.7, sd_ratio=1, a_l=0.8, a_u=2.5,
                           c_l=0, c_u=0.2, categ){
  item_D=NULL; initialitem_D=NULL; data_D=NULL
  item_P=NULL; initialitem_P=NULL; data_P=NULL



  # ability parameters (i.e., theta)

  if(latent_dist=="beta"){
    set.seed(seed)
    theta <- rBeta.4P(n=N, alpha = 3.79, beta= 10.21, l=-2.36, u=6.36)
  }else if(latent_dist=="chi"){
    set.seed(seed)
    theta <- scale(rchisq(N,df=8))
  }else if(latent_dist=="Mixture"){
    n1 <- round(N*prob)
    n2 <- N-n1
    m1 <- -(1-prob)*d
    m2 <- prob*d
    s1 <- sqrt((1-prob*(1-prob)*d^2)/(prob+(1-prob)*sd_ratio^2))
    s2 <- s1*sd_ratio

    set.seed(seed)
    theta <- c(rnorm(n=n1,mean=m1,sd=s1),rnorm(n=n2,mean=m2,sd=s2))
  }else if(latent_dist=="normal"){
    set.seed(seed)
    theta <- rnorm(n=N,mean=0,sd=1)
  } else stop("Specify the type of the latent distribution.")



  # item parameters for dichotomous items

  if(nitem_D!=0){
    data_D <- matrix(nrow = N, ncol = nitem_D)
    item_D <- matrix(nrow = nitem_D, ncol = 3)
    initialitem_D <- matrix(nrow = nitem_D, ncol = 3)
    set.seed(seed)
    for(i in 1:nitem_D){
      if(model_D[i]==1){
        item_D[i,] <- round(c(1,
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              0), digits = 2)
        initialitem_D[i,] <- c(1,0,0)
      }else if(model_D[i]==2){
        item_D[i,] <- round(c(runif(1,a_l,a_u),
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              0), digits = 2)
        initialitem_D[i,] <- c((a_l+a_u)/2,0,0)
      } else if(model_D[i]==3){
        item_D[i,] <- round(c(runif(1,a_l,a_u),
                              sample(seq(-2,2,by=0.01),1, prob = dnorm(seq(-2,2,by=0.01))),
                              runif(1, min = c_l, max = c_u)), digits = 2)
        initialitem_D[i,] <- c((a_l+a_u)/2,0,0)
      }

      # item responses for dichotomous items

      for(j in 1:N){
        p <- P(theta = theta[j], a = item_D[i,1], b = item_D[i,2], c= item_D[i,3])
        data_D[j,i] <- rbinom(1,1,prob = p)
      }
    }
  }


  # item parameters for polytomous items

  if(nitem_P!=0){
    data_P <- matrix(nrow = N, ncol = nitem_P)
    item_P <- matrix(nrow = nitem_P, ncol = 7)
    initialitem_P <- matrix(nrow = nitem_P, ncol = 7)
    set.seed(seed)
    for(i in 1:nitem_P){
      if(model_P=="GPCM"){
        item_P[i,1] <- round(runif(1,a_l,a_u), digits = 2)
        center <- rnorm(1,0,.5)
        item_P[i,2:(categ[i])] <- sort(rnorm(categ[i]-1,center,.2))

        initialitem_P[i,1] <- 1
        initialitem_P[i,2:(categ[i])] <- 0#(-2:1+.5)/3
        }


      # item responses for polytomous items

      for(j in 1:N){
        pp <- P_P(theta = theta[j], a = item_P[i,1], b = item_P[i,-1])
        pp <- pp[!is.na(pp)]
        data_P[j,i] <- sample(x = 0:(categ[i]-1),1,prob=pp)
      }
    }
  }
  return(list(theta=theta,
              item_D=item_D, initialitem_D=initialitem_D, data_D=data_D,
              item_P=item_P, initialitem_P=initialitem_P, data_P=data_P))
}
