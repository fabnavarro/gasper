#' Stein's Unbiased Risk Estimate.
#'
#' Adaptive Threshold Selection Using Principle of SURE
#' (The irreducible variance term is not included,
#'  it does not change the position of the minimum).
#'
#' @export SUREthresh
#' @param wcn Noisy wavelet coefficients.
#' @param thresh Threshold values.
#' @param diagWWt Weights.
#' @param beta Thresholding type (beta=1: soft, beta=2: JS).
#' @param sigma Sd of the noise.
#' @param hatsigma Estimator of the sd (if any).
#' @param policy Dependent or uniform.
#' @param keepwc Boolean allowing to export the coefficients of the frame after thresholding (TRUE by default).
#' @return \code{res} A dataframe containing SURE, hatSURE and their respective minima and corresponding optimal thresholds.
#' @seealso \code{\link{SURE_MSEthresh}}
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

SUREthresh <- function(wcn, thresh, diagWWt, beta = 2, sigma, hatsigma = NA, policy = "uniform", keepwc=TRUE) {
  nthresh <- length(thresh)
  erisk <- dof  <- MSE <- rep(0, nthresh)
  if(keepwc){
    wcs <- matrix(0, ncol=nthresh, nrow = length(wcn))
    if (policy == "uniform") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i], beta)
        wcs[,i] <- wc
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (beta - 1)*thresh[i]^beta/abs(wcn)^beta)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), beta)
        wcs[,i] <- wc
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i]*sqrt(diagWWt))*
                          (1 + (beta - 1)*(thresh[i]*sqrt(diagWWt))^beta/abs(wcn)^beta)*diagWWt,na.rm=TRUE)
      }
    } else {
      warning("Allowable policy are listed above")
      print("uniform")
      print("dependent")
    }
    SURE <- erisk + dof*sigma^2
    hatSURE <- erisk + dof*hatsigma^2
    minSURE <- which.min(SURE)
    minhatSURE <- which.min(hatSURE)
    opthreshSURE <- thresh[minSURE]
    opthreshhatSURE <- thresh[minhatSURE]
    res <- list("wc"=wcs,
                "res"=data.frame(SURE = SURE, hatSURE = hatSURE),
                "min"=c(xminSURE = minSURE, xminhatSURE = minhatSURE),
                "thr"=c(opthreshSURE = opthreshSURE, opthreshSURE = opthreshhatSURE))
  }
  else{
    if (policy == "uniform") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i], beta)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (beta - 1)*thresh[i]^beta/abs(wcn)^beta)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), beta)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i]*sqrt(diagWWt))*
                          (1 + (beta - 1)*(thresh[i]*sqrt(diagWWt))^beta/abs(wcn)^beta)*diagWWt,
                        na.rm=TRUE)
      }
    } else {
      warning("Allowable policy are listed above")
      print("uniform")
      print("dependent")
    }
    SURE <- erisk + dof*sigma^2
    hatSURE <- erisk + dof*hatsigma^2
    minSURE <- which.min(SURE)
    minhatSURE <- which.min(hatSURE)
    opthreshSURE <- thresh[minSURE]
    opthreshhatSURE <- thresh[minhatSURE]
    res <- list("res"=data.frame(SURE = SURE, hatSURE = hatSURE),
                "min"=c(xminSURE = minSURE, xminhatSURE = minhatSURE),
                "thr"=c(opthreshSURE = opthreshSURE, opthreshhatSURE = opthreshhatSURE))
  }
  return(res)
}
