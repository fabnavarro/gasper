#' Stein's Unbiased Risk Estimate.
#'
#' Adaptive Threshold Selection Using Principle of SURE
#' (The irreductible variance term is not included,
#'  it does not change the position of the minimum).
#'
#' Note:
#'  - the calculation of the MSE is also included for comparison purpose.
#'
#' @export SURE_MSEthresh
#' @param wcn Noisy wavelet coefficents.
#' @param wcf True wavelet coefficients.
#' @param tresh Threshold values.
#' @param diagWWt Weights.
#' @param b Thresholding type (b=1: soft, b=2: JS).
#' @param sigma Sd of the noise.
#' @param hatsigma Estimator of the sd (if any).
#' @param policy Dependent or uniform.
#' @return \code{res} a dataframe contening MSE, SURE, hatSURE and their respective minima
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2019). Data-driven Thresholding in Denoising with Spectral Graph Wavelet Transform. arXiv preprint arXiv:1906.01882.

SURE_MSEthresh <- function(wcn, wcf, tresh, diagWWt, b, sigma, hatsigma, policy) {
  nthresh <- length(tresh)
  erisk <- dof  <- MSE <- rep(0, nthresh)
  wcs <- matrix(0, ncol=nthresh, nrow = length(wcn))
  if (policy == "uniform") {
    for (i in 1:nthresh) {
      wc <- betathresh(wcn, tresh[i], b)
      wcs[,i] <- wc
      MSE[i] <- sum((wcf - wc)^2)
      erisk[i] <- sum((wc - wcn)^2)
      dof[i] <- 2*sum((abs(wcn) > tresh[i])*
                      (1 + (b - 1)*tresh[i]^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
    }
  }
  else if (policy == "dependent") {
    for (i in 1:nthresh) {
      wc <- betathresh(wcn, tresh[i]*sqrt(diagWWt), b)
      wcs[,i] <- wc
      MSE[i] <- sum((wcf - wc)^2)
      erisk[i] <- sum((wc - wcn)^2)
      dof[i] <- 2*sum((abs(wcn) > tresh[i]*sqrt(diagWWt))*
                (1 + (b - 1)*(tresh[i]*sqrt(diagWWt))^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
    }
  } else {
    warning("Allowable policy are listed above")
    print("uniform")
    print("dependent")
  }
  #MSE <- colSums((as.vector(wcf) - wcs)^2)
  #erisk <- colSums((wcs - as.vector(wcn))^2)
  SURE <- erisk + dof*sigma^2
  hatSURE <- erisk + dof*hatsigma^2
  minMSE <- which.min(MSE)
  minSURE <- which.min(SURE)
  minhatSURE <- which.min(hatSURE)
  opthreshMSE <- tresh[minMSE]
  opthreshSURE <- tresh[minSURE]
  opthreshhatSURE <- tresh[minhatSURE]
  res <- list("wc"=wcs,
              "res"=data.frame(MSE = MSE, SURE = SURE, hatSURE = hatSURE),
              "min"=c(minMSE, minSURE, minhatSURE),
              "thr"=c(opthreshMSE, opthreshSURE, opthreshhatSURE))
  return(res)
}
