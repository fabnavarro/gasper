#' Level Dependent Stein's Unbiased Risk Estimate.
#'
#' Level Dependent adaptive threshold selection using SURE.
#'
#' @export LD_SUREthresh
#' @param J Finest-scale (highest frequency).
#' @param wcn Noisy wavelet coefficents.
#' @param diagWWt Weights.
#' @param beta Thresholding type (beta=1: soft, beta=2: JS).
#' @param sigma Sd of the noise.
#' @param hatsigma Estimator of the sd (if any).
#' @param policy Dependent or uniform.
#' @param keepSURE Boolean allowing to export the dataframe outuput of SUREthresh (FALSE by default).
#' @return \code{wcLDSURE} Wavelet coefficient estimates applying SURE scale by scale.
#' @return \code{wcLDhatSURE} if hatsigma provided, wavelet coefficient estimates applying SURE scale by scale.
#' @seealso \code{\link{SUREthresh}}
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#'Stein, C. M. (1981). Estimation of the mean of a multivariate normal distribution. The annals of Statistics, 1135-1151.

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



