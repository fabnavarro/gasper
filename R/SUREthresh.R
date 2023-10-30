#' Stein's Unbiased Risk Estimate
#'
#' Adaptive Threshold Selection Using Principle of Stein's Unbiased Risk Estimate (SURE).
#'
#' @export SUREthresh
#' @param wcn Numeric vector of the noisy spectral graph wavelet coefficients.
#' @param thresh Numeric vector of threshold values.
#' @param diagWWt Numeric vector of weights typically derived from the diagonal elements of the wavelet frame matrix.
#' @param beta A numeric value specifying the type of thresholding to be used, for example:
#'  \itemize{
#'             \item 1 for soft thresholding.
#'             \item 2 for James-Stein thresholding.
#'             }
#' @param sigma A numeric value representing the standard deviation (sd) of the noise.
#' @param hatsigma An optional numeric value providing an estimate of the noise standard deviation (default is \code{NA}).
#' @param policy A character string determining the thresholding policy. Valid options include:
#' \itemize{
#'          \item \code{"uniform"} for a global threshold applied uniformly across all coefficients.
#'          \item \code{"dependent"} for threshold values that adaptively depend on the corresponding \code{diagWWt} weights.
#'          }
#' @param keepwc A logical value determining if the thresholded wavelet coefficients should be returned (Default is \code{TRUE}).
#' @return A list containing:
#' \itemize{
#'         \item A dataframe with calculated SURE and hatSURE values.
#'         \item Minima of SURE and hatSURE and their corresponding optimal thresholds.
#'         \item Thresholded wavelet coefficients (if \code{keepwc = TRUE}).
#'         }
#' @seealso \code{\link{SURE_MSEthresh}}, \code{\link{GVN}}, \code{\link{HPFVN}}
#'
#' @details
#' The \code{SUREthresh} function is a data-driven approach to finding the optimal thresholding value for denoising wavelet coefficients. SURE provides a means to evaluate the denoising quality of a given thresholding function \eqn{h}. The expected risk in terms of the mean squared error (MSE) between the original coefficients \eqn{\mathbf{F}} and their thresholded counterparts \eqn{h(\widetilde{\mathbf{F}})}, considering a noise variance \eqn{\sigma^2}, is given by:
#' \deqn{
#' \mathbb E \left[\|\mathbf{F} - h(\widetilde{\mathbf{F}})\|_2^2 \right] = \mathbb E \left[-n\sigma^2 + \|\widetilde{\mathbf{F}} - h(\widetilde{\mathbf{F}})\|_2^2 + 2\sigma^2 \sum_{i,j = 1}^{n(J+1)} \gamma_{ij} \partial_j h_i(\widetilde{\mathbf{F}}) \right]
#' }
#' Where:
#' \itemize{
#'   \item \eqn{\widetilde{\mathbf{F}}} are the noisy wavelet coefficients.
#'   \item \eqn{\gamma_{ij}} represents the elements of the matrix obtained by multiplying the transpose of the wavelet transform matrix \eqn{\mathbf{\Psi}} with itself, i.e., \eqn{\gamma_{ij} = (\mathbf{\Psi}^\top \mathbf{\Psi})_{ij}}.
#'   \item \eqn{h_i} is the \eqn{i^{th}} component of the thresholding function \eqn{h}.
#'   \item \eqn{n} is the sample size.
#' }
#'
#' The thresholding operator, represented by \eqn{h} in the \code{\link{SUREthresh}} function, is obtained using this \code{\link{betathresh}} function. The SURE in the transformed domain can be explicitly stated as:
#' \deqn{
#' \mathbf{SURE}(h) = -n \sigma^2 + \sum_{i=1}^{n(J+1)} \widetilde{F}_i^2 \left ( 1 \wedge \frac{t_i^\beta}{|\widetilde{F}_i|^\beta} \right )^2
#' + 2 \sum_{i=1}^{n(J+1)} \gamma_{ij} \mathbf{1}_{[t_i,\infty)}(|\widetilde{F}_i|) \left [ 1+\frac{(\beta-1) t_i^\beta}{|\widetilde{F}_i|^\beta} \right ].
#' }
#' \code{\link{GVN}} and \code{\link{HPFVN}} provide naive noise variance estimation.
#'
#' @note
#' The vector of thresholds \code{thresh} for evaluating the SURE can be effectively determined by ordering the absolute values of the noisy wavelet coefficients. This approach aligns with Donoho and Johnstone's trick in standard wavelet thresholding, where SURE typically reaches its minimum at one of these coefficients. For further details, see Donoho and Johnstone Section 2.3 and de Loynes et al. Section 3.3.
#'
#' The function intentionally omits the irreducible variance term from the SURE calculations, as it doesn't affect the minimum's location.
#'
#' Also, when `keepwc = TRUE`, the function provides thresholded wavelet coefficients for all evaluated threshold values, offering deeper insights into the effects of different thresholds.
#'
#' @examples
#' #
#' # See example in SURE_MSEthresh
#' #
#'
#' @references
#' Donoho, D. L., & Johnstone, I. M. (1995). Adapting to unknown smoothness via wavelet shrinkage. Journal of the american statistical association, 90(432), 1200-1224.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' Stein, C. M. (1981). Estimation of the mean of a multivariate normal distribution. The annals of Statistics, 1135-1151.


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
