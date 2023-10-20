#' Stein's Unbiased Risk Estimate.
#'
#' Adaptive Threshold Selection Using Principle of SURE
#' (The irreducible variance term is not included,
#'  it does not change the position of the minimum).
#'
#' Note:
#'  - the calculation of the MSE is also included for comparison purpose.
#'
#' @export SURE_MSEthresh
#' @param wcn Noisy wavelet coefficients.
#' @param wcf True wavelet coefficients.
#' @param thresh Threshold values.
#' @param diagWWt Weights.
#' @param b Threshold type (e.g, b=1: soft, b=2: JS).
#' @param sigma Standard deviation of the noise.
#' @param hatsigma Estimator of the standard deviation (if any).
#' @param policy Dependent or uniform.
#' @param keepwc Boolean allowing to export the coefficients of the frame after thresholding (TRUE by default).
#' @return \code{res} a dataframe containing MSE, SURE, hatSURE and their respective minima
#' @seealso \code{\link{SUREthresh}}
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

SURE_MSEthresh <- function(wcn, wcf, thresh, diagWWt, b = 2, sigma, hatsigma = NA, policy = "uniform", keepwc=TRUE) {
  nthresh <- length(thresh)
  erisk <- dof  <- MSE <- rep(0, nthresh)
  if(keepwc){
    wcs <- matrix(0, ncol=nthresh, nrow = length(wcn))
    if (policy == "uniform") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i], b)
        wcs[,i] <- wc
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (b - 1)*thresh[i]^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), b)
        wcs[,i] <- wc
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i]*sqrt(diagWWt))*
                          (1 + (b - 1)*(thresh[i]*sqrt(diagWWt))^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
      }
    } else {
      warning("Allowable policy are listed above")
      print("uniform")
      print("dependent")
    }
    SURE <- erisk + dof*sigma^2
    hatSURE <- erisk + dof*hatsigma^2
    minMSE <- which.min(MSE)
    minSURE <- which.min(SURE)
    minhatSURE <- which.min(hatSURE)
    opthreshMSE <- thresh[minMSE]
    opthreshSURE <- thresh[minSURE]
    opthreshhatSURE <- thresh[minhatSURE]
    res <- list("wc"=wcs,
                "res"=data.frame(MSE = MSE, SURE = SURE, hatSURE = hatSURE),
                "min"=c(xminMSE = minMSE,
                        xminSURE = minSURE,
                        xminhatSURE = minhatSURE),
                "thr"=c(opthreshMSE = opthreshMSE,
                        opthreshSURE = opthreshSURE,
                        opthreshhatSURE = opthreshhatSURE))
  }
  else{
    if (policy == "uniform") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i], b)
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (b - 1)*thresh[i]^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), b)
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i]*sqrt(diagWWt))*
                          (1 + (b - 1)*(thresh[i]*sqrt(diagWWt))^b/abs(wcn)^b)*diagWWt,na.rm=TRUE)
      }
    } else {
      warning("Allowable policy are listed above")
      print("uniform")
      print("dependent")
    }
    SURE <- erisk + dof*sigma^2
    hatSURE <- erisk + dof*hatsigma^2
    minMSE <- which.min(MSE)
    minSURE <- which.min(SURE)
    minhatSURE <- which.min(hatSURE)
    opthreshMSE <- thresh[minMSE]
    opthreshSURE <- thresh[minSURE]
    opthreshhatSURE <- thresh[minhatSURE]
    res <- list("res"=data.frame(MSE = MSE,
                                 SURE = SURE,
                                 hatSURE = hatSURE),
                "min"=c(xminMSE = minMSE,
                        xminSURE = minSURE,
                        xminhatSURE = minhatSURE),
                "thr"=c(opthreshMSE = opthreshMSE,
                        opthreshSURE = opthreshSURE,
                        opthreshhatSURE = opthreshhatSURE))
  }
  return(res)
}
