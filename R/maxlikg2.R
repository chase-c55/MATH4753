#' Maximum Likelihood for Two Estimators
#'
#' @param theta1 First estimator values
#' @param theta2 Second Estimator Values
#' @param lfun log likelihood function
#' @param ... Additional parameters for countor map
#'
#' @importFrom graphics axis contour
#' @return Contour map and list of estimates for theta1 and theta2
#' @export
#'
#' @examples \dontrun{maxlikg2(x, y, lfun = logbinpois)}
maxlikg2=function(theta1,theta2,lfun="logbinpois",...){
  n1=length(theta1)
  n2=length(theta2)

  z=outer(theta1,theta2,lfun)

  contour(theta1,theta2,exp(z),...) # exp(z) gives the lik

  maxl=max(exp(z))  # max likelihood

  coord=which(exp(z)==maxl,arr.ind=TRUE)  # find the co-ords of the max

  th1est=theta1[coord[1]] # mxlik estimate of theta1
  th2est=theta2[coord[2]]

  abline(v=th1est,h=th2est)

  axis(3,th1est,round(th1est,2))
  axis(4,th2est,round(th2est,2),las=1)

  list(th1est=th1est,th2est=th2est)
}
