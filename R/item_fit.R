#' Item fit diagnostics
#'
#' @description This function analyzes and reports item-fit test results.
#'
#'
#' @param x A model fit object from either \code{IRTest_Dich}, \code{IRTest_Poly}, or \code{IRTest_Mix}.
#' @param bins The number of bins to be used for calculating the statistics.
#' Following Yen's \eqn{Q_{1}} (1981), the default is 10.
#' @param bin.center A method for calculating the center of each bin.
#' Following Yen's \eqn{Q_{1}} (1981), the default is \code{"mean"}.
#' Use \code{"median"} for Bock's \eqn{\chi^{2}} (1960).
#'
#' @return This function returns a \code{matrix} of item-fit test results.
#'
#' @details
#' Bock's \eqn{\chi^{2}} (1960) or Yen's \eqn{Q_{1}} (1981) is currently available.
#'
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @references
#'
#' Bock, R.D. (1960), Methods and applications of optimal scaling. Chapel Hill, NC: L.L. Thurstone Psychometric Laboratory.
#'
#' Yen, W. M. (1981). Using simulation results to choose a latent trait model. \emph{Applied Psychological Measurement, 5}(2), 245–262.
#'
#'
#' @export
#' @examples
#' \donttest{
#' # A preparation of dichotomous item response data
#'
#' data <- DataGeneration(N=500,
#'                        nitem_D = 10)$data_D
#'
#' # Analysis
#'
#' M1 <- IRTest_Dich(data)
#'
#' # Item fit statistics
#'
#' item_fit(M1)
#'}
item_fit <- function(x, bins=10, bin.center='mean'){
  UseMethod("item_fit",x)
}

#' @export
#' @importFrom stats xtabs
#' @importFrom stats pchisq
#' @importFrom stats median
#' @importFrom stats aggregate
#'
item_fit.dich <- function(x,bins=10, bin.center='mean'){
  result <- matrix(nrow = nrow(x$par_est), ncol = 3)

  for(i in 1:nrow(x$par_est)){
    NA_index <- !is.na(x$Options$data[,i])
    theta_responded <- x$theta[NA_index]

    binned <- binning(theta_responded, bins, i)
    bin_data <- data.frame(
      theta=theta_responded,
      bin=binned[[1]]
    )

    center <- if(bin.center=='mean'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = mean)[[2]]
    }else if(bin.center=='median'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = median)[[2]]
    }

    counts = aggregate(bin_data$theta, by = list(bin_data$bin), FUN = length)[[2]]

    prob <- P(theta = center, a = x$par_est[i,1], b = x$par_est[i,2], c = x$par_est[i,3])
    prob <- cbind(1-prob, prob)
    observed_freq <- as.matrix(
      xtabs(~bin+response,cbind(bin_data, response = x$Options$data[,i][NA_index]))
    )
    expected_freq <- counts*prob
    fit_stat <- sum((observed_freq-expected_freq)^2/expected_freq)

    model <- x$Options$model[i]
    result[i,1] <- fit_stat
    result[i,2] <- (binned[[2]]-1)*(ncol(prob)-1)-
      if(model %in% c(1, "1PL", "Rasch", "RASCH")){
        1
      }else if(model%in% c(2, "2PL")){
        2
      } else if(model%in% c(3, "3PL")){
        3
      }
    result[i,3] <- round(
      x = pchisq(q = result[i,1],
                 df = result[i,2],
                 lower.tail = FALSE),
      digits = 4
    )
  }
  dimnames(result) <- list(item=row.names(x$par_est), c("stat", "df", "p.value"))
  result <- data.frame(result)
  return(result)
}

#' @export
#' @importFrom stats xtabs
#' @importFrom stats pchisq
#' @importFrom stats median
#' @importFrom stats aggregate
#'
item_fit.poly <- function(x,bins=10, bin.center='mean'){
  result <- matrix(nrow = nrow(x$par_est), ncol = 3)

  for(i in 1:nrow(x$par_est)){
    NA_index <- !is.na(x$Options$data[,i])
    theta_responded <- x$theta[NA_index]

    binned <- binning(theta_responded, bins, i)
    bin_data <- data.frame(
      theta=theta_responded,
      bin=binned[[1]]
    )

    center <- if(bin.center=='mean'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = mean)[[2]]
    }else if(bin.center=='median'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = median)[[2]]
    }

    counts = aggregate(bin_data$theta, by = list(bin_data$bin), FUN = length)[[2]]

    if(x$Options$model %in% c("PCM", "GPCM")){
      prob <- P_P(theta = center, a = x$par_est[i,1], b = x$par_est[i,-1])
    } else if(x$Options$model %in% c("GRM")){
      prob <- P_G(theta = center, a = x$par_est[i,1], b = x$par_est[i,-1])
    }
    observed_freq <- as.matrix(
      xtabs(~bin+response,cbind(bin_data, response = x$Options$data[,i][NA_index]))
      )
    expected_freq <- counts*prob
    fit_stat <- sum((observed_freq-expected_freq)^2/expected_freq)

    result[i,1] <- fit_stat
    result[i,2] <- (binned[[2]]-1)*(ncol(prob)-1)-
      if(x$Options$model=="PCM"){
        ncol(prob)-1
      }else if(x$Options$model %in% c("GPCM", "GRM")){
        ncol(prob)
      }
    result[i,3] <- round(
      x = pchisq(q = result[i,1],
      df = result[i,2],
      lower.tail = FALSE),
      digits = 4
      )
  }
  dimnames(result) <- list(item=row.names(x$par_est), c("stat", "df", "p.value"))
  result <- data.frame(result)
  return(result)
}

#' @export
#' @importFrom stats xtabs
#' @importFrom stats pchisq
#' @importFrom stats median
#' @importFrom stats aggregate
#'
item_fit.mix <- function(x,bins=10, bin.center='mean'){
  # dichotomous
  result1 <- matrix(nrow = nrow(x$par_est[[1]]), ncol = 3)

  for(i in 1:nrow(x$par_est[[1]])){
    NA_index <- !is.na(x$Options$data_D[,i])
    theta_responded <- x$theta[NA_index]

    binned <- binning(theta_responded, bins, i)
    bin_data <- data.frame(
      theta=theta_responded,
      bin=binned[[1]]
    )

    center <- if(bin.center=='mean'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = mean)[[2]]
    }else if(bin.center=='median'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = median)[[2]]
    }

    counts = aggregate(bin_data$theta, by = list(bin_data$bin), FUN = length)[[2]]

    prob <- P(theta = center, a = x$par_est[[1]][i,1], b = x$par_est[[1]][i,2], c = x$par_est[[1]][i,3])
    prob <- cbind(1-prob, prob)
    observed_freq <- as.matrix(
      xtabs(~bin+response,cbind(bin_data, response = x$Options$data_D[,i][NA_index]))
    )
    expected_freq <- counts*prob
    fit_stat <- sum((observed_freq-expected_freq)^2/expected_freq)

    model <- x$Options$model_D[i]
    result1[i,1] <- fit_stat
    result1[i,2] <- (binned[[2]]-1)*(ncol(prob)-1)-
      if(model %in% c(1, "1PL", "Rasch", "RASCH")){
        1
      }else if(model%in% c(2, "2PL")){
        2
      } else if(model%in% c(3, "3PL")){
        3
      }
    result1[i,3] <- round(
      x = pchisq(q = result1[i,1],
                 df = result1[i,2],
                 lower.tail = FALSE),
      digits = 4
    )
  }
  dimnames(result1) <- list(item=row.names(x$par_est[[1]]), c("stat", "df", "p.value"))
  result1 <- data.frame(result1)

  # polytomous
  result2 <- matrix(nrow = nrow(x$par_est[[2]]), ncol = 3)

  for(i in 1:nrow(x$par_est[[2]])){
    NA_index <- !is.na(x$Options$data_P[,i])
    theta_responded <- x$theta[NA_index]

    binned <- binning(theta_responded, bins, i)
    bin_data <- data.frame(
      theta=theta_responded,
      bin=binned[[1]]
    )

    center <- if(bin.center=='mean'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = mean)[[2]]
    }else if(bin.center=='median'){
      aggregate(
        x = bin_data$theta,
        by = list(bin_data$bin),
        FUN = median)[[2]]
    }

    counts = aggregate(bin_data$theta, by = list(bin_data$bin), FUN = length)[[2]]

    if(x$Options$model_P %in% c("PCM", "GPCM")){
      prob <- P_P(theta = center, a = x$par_est[[2]][i,1], b = x$par_est[[2]][i,-1])
    } else if(x$Options$model_P %in% c("GRM")){
      prob <- P_G(theta = center, a = x$par_est[[2]][i,1], b = x$par_est[[2]][i,-1])
    }
    observed_freq <- as.matrix(
      xtabs(~bin+response,cbind(bin_data, response = x$Options$data_P[,i][NA_index]))
    )
    expected_freq <- counts*prob
    fit_stat <- sum((observed_freq-expected_freq)^2/expected_freq)

    result2[i,1] <- fit_stat
    result2[i,2] <- (binned[[2]]-1)*(ncol(prob)-1)-
      if(x$Options$model_P=="PCM"){
        ncol(prob)-1
      }else if(x$Options$model_P %in% c("GPCM", "GRM")){
        ncol(prob)
      }
    result2[i,3] <- round(
      x = pchisq(q = result2[i,1],
                 df = result2[i,2],
                 lower.tail = FALSE),
      digits = 4
    )
  }
  dimnames(result2) <- list(item=row.names(x$par_est[[2]]), c("stat", "df", "p.value"))
  result2 <- data.frame(result2)
  return(list(
    Dichotomous = result1,
    Polytomous = result2
    )
  )
}

#' @importFrom ggplot2 cut_number
binning <- function(theta_responded, bins, i, boolFalse=FALSE){
  while(boolFalse==F)
  {
    bins <- bins-1
    tryCatch({
      binned <- ggplot2::cut_number(theta_responded, n = bins, label=1:bins)
      boolFalse<-T
    },error=function(e){
      message(sprintf("Insufficient data values to produce %d bins for Item %i. %d bins will be used.", bins+1, i, bins))
    },finally={})
  }
  return(list(binned, bins+1))
}


