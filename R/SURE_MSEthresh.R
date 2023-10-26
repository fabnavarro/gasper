#' Stein's Unbiased Risk Estimate with MSE
#'
#' Adaptive Threshold Selection Using Principle of SURE with the inclusion of Mean Squared Error (MSE) for comparison.
#'
#' @export SURE_MSEthresh
#' @param wcn Numeric vector of the noisy spectral graph wavelet coefficients.
#' @param wcf Numeric vector of the true spectral graph wavelet coefficients.
#' @param thresh Numeric vector of threshold values.
#' @param diagWWt Numeric vector of weights typically derived from the diagonal elements of the wavelet frame matrix.
#' @param beta A numeric value specifying the type of thresholding to be used:
#'  \itemize{
#'             \item 1 for soft thresholding.
#'             \item 2 for James-Stein thresholding.
#'             }
#' @param sigma A numeric value representing the standard deviation (sd) of the noise.
#' @param hatsigma An optional numeric value providing an estimate of the noise standard deviation (default is NA).
#' @param policy A character string determining the thresholding policy. Valid options include:
#' \itemize{
#'     \item "uniform" for a global threshold applied uniformly across all coefficients.
#'     \item "dependent" for threshold values that adaptively depend on the corresponding `diagWWt` weights.
#'               }
#' @param keepwc A logical value determining if the thresholded wavelet coefficients should be returned (Default is TRUE).
#' @return A list containing:
#' \itemize{
#'         \item A dataframe with calculated MSE, SURE, and hatSURE values.
#'         \item Minima of SURE, hatSURE, and MSE, and their corresponding optimal thresholds.
#'         \item Thresholded wavelet coefficients (if \code{keepwc = TRUE}).
#'         }
#' @details
#' \code{SURE_MSEthresh} function extends the \code{SUREthresh} function by providing an MSE between the true coefficients and their thresholded versions for a given thresholding function \eqn{h}. This allows for a more comprehensive evaluation of the denoising quality in simulated scenarios where the true function is known.
#'
#' @seealso \code{\link{SUREthresh}}, \code{\link{GVN}}, \code{\link{HPFVN}}
#'
#' @references
#' Donoho, D. L., & Johnstone, I. M. (1995). Adapting to unknown smoothness via wavelet shrinkage. Journal of the american statistical association, 90(432), 1200-1224.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' Stein, C. M. (1981). Estimation of the mean of a multivariate normal distribution. The annals of Statistics, 1135-1151.

SURE_MSEthresh <- function(wcn, wcf, thresh, diagWWt, beta = 2, sigma, hatsigma = NA, policy = "uniform", keepwc=TRUE) {
  nthresh <- length(thresh)
  erisk <- dof  <- MSE <- rep(0, nthresh)
  if(keepwc){
    wcs <- matrix(0, ncol=nthresh, nrow = length(wcn))
    if (policy == "uniform") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i], beta)
        wcs[,i] <- wc
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (beta - 1)*thresh[i]^beta/abs(wcn)^beta)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), beta)
        wcs[,i] <- wc
        MSE[i] <- sum((wcf - wc)^2)
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
        wc <- betathresh(wcn, thresh[i], beta)
        MSE[i] <- sum((wcf - wc)^2)
        erisk[i] <- sum((wc - wcn)^2)
        dof[i] <- 2*sum((abs(wcn) > thresh[i])*
                          (1 + (beta - 1)*thresh[i]^beta/abs(wcn)^beta)*diagWWt,na.rm=TRUE)
      }
    }
    else if (policy == "dependent") {
      for (i in 1:nthresh) {
        wc <- betathresh(wcn, thresh[i]*sqrt(diagWWt), beta)
        MSE[i] <- sum((wcf - wc)^2)
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
