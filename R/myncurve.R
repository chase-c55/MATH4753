#' My Normal Curve Generator
#'
#' @param mu The mean
#' @param sigma The standard deviation
#' @param a The a such that P(Y<=a)
#'
#' @importFrom graphics polygon
#'
#' @return A plot of the curve with the area P(Y<=a) filled and the probability outputted to the command line.
#' @export
#'
#' @examples myncurve(10,5,6)

myncurve = function(mu, sigma, a){
  x = NULL
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))
  xcurve = seq(mu-3*sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma,xcurve, a), c(0,ycurve,0), col = "red")
  round(pnorm(a, mean = mu, sd = sigma), 4)
}
