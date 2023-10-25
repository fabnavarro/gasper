#' Level Dependent Stein's Unbiased Risk Estimate (LD-SURE) Thresholding
#'
#' Adaptive threshold selection using the Level Dependent Stein's Unbiased Risk Estimate (LD-SURE).
#' This function applies SURE in a level dependent manner to wavelet coefficients, which aims to
#' minimize SURE at each wavelet scale.
#'
#' @export
#' @param J The finest scale, or the highest frequency. This parameter determines the total number of scales that the function will process (interger).
#' @param wcn A vector of noisy wavelet coefficients that need to be thresholded (numeric).
#' @param diagWWt A vector of weights.
#' @param beta The type of thresholding to be used. If beta=1, soft thresholding is applied.
#'        If beta=2, James-Stein thresholding is applied (Default is 2).
#' @param sigma The standard deviation of the noise present in the wavelet coefficients (numeric).
#' @param hatsigma An optional estimator of the noise standard deviation. If provided, the
#'        function will also compute wavelet coefficient estimates using this estimator (numeric).
#' @param policy The policy for threshold setting. It can be either "uniform" (default) or
#'        "dependent". In the "uniform" policy, the thresholds are set based on the absolute
#'        value of the wavelet coefficients. In the "dependent" policy, the thresholds are
#'        set based on the wavelet coefficients normalized by the weights from `diagWWt`.
#' @param keepSURE A logical flag. If `TRUE`, the function will also return a list containing
#'        the results of the SURE thresholding for each scale.
#' @return A list containing the wavelet coefficient estimates after applying the SURE
#'         thresholding. The list will contain the following components:
#'         \itemize{
#'           \item \code{wcLDSURE}: The wavelet coefficient estimates obtained by minimizing SURE.
#'           \item \code{wcLDhatSURE}: If `hatsigma` is provided, this component contains the
#'                  wavelet coefficient estimates obtained using the `hatsigma` estimator.
#'           \item \code{lev_thresh}: If `keepSURE` is `TRUE`, this component contains a list
#'                  of results similar to the output of \code{SUREthresh} for each scale.
#'         }
#' @seealso
#' \code{\link{SUREthresh}}
#' \note See \code{\link{SUREthresh}} for the underlying thresholding method used at each scale.
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with
#' Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' Stein, C. M. (1981). Estimation of the mean of a multivariate normal distribution. The annals of Statistics, 1135-1151.

LD_SUREthresh <- function(J, wcn, diagWWt, beta = 2, sigma, hatsigma = NA, policy = "uniform", keepSURE = FALSE){
  N <- length(wcn)
  n <- N/(J+1)
  lev_thresh <- list()
  wclevSURE <- rep(0, N)
  wclevhatSURE <- rep(0, N)
  tresh_set_all <- rep(0, N)
  for (k in 0:J){
    indscale <- seq(k*n+1, (k+1)*n)
    wc <- wcn[indscale]
    if (policy == "uniform") {
      thresh_set <- sort(abs(wc))
    }
    else if (policy == "dependent") {
      thresh_set <- sort(abs(wc)/sqrt(diagWWt[indscale]))
      #hatsigma <- sqrt(sum(wc^2)/
      #               sum(zetav(evalues, k, b=2)))
    }
    else {
      warning("Allowable policy are listed above")
      print("uniform")
      print("dependent")
    }
    lev_thresh[[k+1]] <- SUREthresh(wcn = wc,
                                    thresh = thresh_set,
                                    diagWWt = diagWWt[indscale],
                                    beta = beta,
                                    sigma = sigma,
                                    hatsigma = hatsigma,
                                    policy = policy,
                                    keepwc = TRUE)

    wclevSURE[indscale] <- lev_thresh[[k+1]]$wc[,lev_thresh[[k+1]]$min[1]]
    wclevhatSURE[indscale] <- lev_thresh[[k+1]]$wc[,lev_thresh[[k+1]]$min[2]]
  }
  if(keepSURE){
    return(list(lev_thresh = lev_thresh,
                wcLDSURE = wclevSURE,
                wcLDhatSURE = wclevhatSURE))
  }
  else{
    return(list(wcLDSURE = wclevSURE,
                wcLDhatSURE = wclevhatSURE))
  }
}



