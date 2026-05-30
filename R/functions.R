
#' Title
#'
#' @param Y original aggregate
#' @param rho ratio top contribution / Y
#' @param n positive real, shape parameter
#' @param sg_nu standard error for the monitoring-dominance gaussian noise 
#' @param sg_eps standard error for the monitoring-differenciation gaussian noise 
#'
#' @returns
#' @export
#'
#' @examples
#' set.seed(40889)
#' make_noisy(Y = 100, rho = 0.5, sg_nu = 0.25, sg_eps = 0.01)
make_noisy <- function(Y, rho, n, sg_nu, sg_eps){
  
  nu <- rnorm(1, sd = sg_nu)
  eps <- rnorm(1, sd = sg_eps)
  
  Y * (1 + rho^n * nu + eps)
}


#' Interval confidence of the relative loss Z = (Y'-Y)/Y
#' conditionally to rho
#'
#' @inheritParams make_noisy 
#' @param confidence level of confidence
#'
#' @returns
#' @export
#'
#' @examples
#' IC_relative_loss(rho = 0.5, sg_nu = 0.25, sg_eps = 0.01)
IC_relative_loss <- function(rho, n, sg_nu, sg_eps, confidence = 0.95){
  
  alpha = (1 - confidence)/2
  q <- qnorm(alpha) * c(-1,1)
  std <- sqrt(rho^(2*n) * sg_nu^2 + sg_eps^2)
  
  return(q * std)
}



#' Absolute expectation of the relative loss Z = (Y'-Y)/Y, 
#' conditionally to rho
#'
#' @param rho 
#' @param n 
#' @param sg_nu 
#' @param sg_eps 
#'
#' @returns
#' @export
#'
#' @examples
#' abs_expect_relative_loss(rho = 0.5, sg_nu = 0.25, sg_eps = 0.01)
abs_expect_relative_loss <- function(rho, n, sg_nu, sg_eps){
  
  std <- sqrt(rho^(2*n) * sg_nu^2 + sg_eps^2)
  
  return(sqrt(2/pi) * std)
}


#' Cumulative distribution function of the relative loss Z = (Y'-Y)/Y
#'
#' @param q quantile value
#' @inheritParams make_noisy
#'
#' @returns
#' @export
#'
#' @examples
#' qZloss(0.2, rho = 0.5, sg_nu = 0.25, sg_eps = 0.01)
qZloss <- function(q, rho, n, sg_nu, sg_eps){
  
  std <- sqrt(rho^(2*n) * sg_nu^2 + sg_eps^2)
  
  return( pnorm(q, mean = 0, sd = std))
  
}

#' Assess the risk mu_I = P( |(Y'-X1)/X1| < beta)
#'
#' @inheritParams make_noisy 
#' @param beta threshold
#'
#' @returns
#' @export
#'
#' @examples
assess_risk_I <- function(rho, n, sg_nu, sg_eps, beta){
  
  a <- (1 - beta)*rho - 1 
  b <- (1 + beta)*rho - 1 

  qZloss(b, rho, n, sg_nu, sg_eps) - qZloss(a, rho, n, sg_nu, sg_eps)
}

#' Assess the risk mu_II = P( |(Y'-X2 - X1)/X1| < beta)
#'
#' @inheritParams make_noisy 
#' @param rho2 contribution of the second top contributor to Y
#' @param beta threshold
#'
#' @returns
#' @export
#'
#' @examples
assess_risk_II <- function(rho, n, sg_nu, sg_eps, rho2, beta){
  
  a <- (1 - beta)*rho + rho2 - 1 
  b <- (1 + beta)*rho + rho2 - 1 

  qZloss(b, rho, n, sg_nu, sg_eps) - qZloss(a, rho, n, sg_nu, sg_eps)
}


#' Cumulative distribution function of |Delta| = |(delta' - delta)/delta|
#'
#' @param q quantile value
#' @inheritParams make_noisy
#'
#' @returns
#' @export
#'
#' @examples
#' qZloss(0.2, rho = 0.5, sg_nu = 0.25, sg_eps = 0.01)
qDeltaloss <- function(q, rho, n, sg_nu, sg_eps){
  
  std <- sqrt(2) * sg_eps
  
  return( 2*pnorm(q, mean = 0, sd = std) - 1)
  
}



#' Assess the risk mu_DIFF = P( |(delta'-delta)/delta| < beta)
#'
#' @inheritParams make_noisy 
#' @param beta threshold
#'
#' @returns
#' @export
#'
#' @examples
assess_risk_DIFF <- function(rho, n, sg_eps, beta){
  
  a <- (1 - beta)*rho + rho2 - 1 
  b <- (1 + beta)*rho + rho2 - 1 

  qDeltaloss(b, rho, n, sg_nu, sg_eps) - qDeltaloss(a, rho, n, sg_nu, sg_eps)
}









