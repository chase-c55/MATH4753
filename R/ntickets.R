#' ntickets
#'
#' @param N Number of seats on the plane
#' @param gamma Probability of overbooking
#' @param p Probability that a passenger "shows"
#'
#' @importFrom stats dnorm optimize pbinom pnorm
#' @importFrom graphics abline curve
#'
#' @return Two graphs of the objective functions vs n (discrete and continuous) and a named list
#' @export
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets = function(N, gamma, p){

  # Create plot of Objective function vs n (discrete)
  objectived = function(gamma, N, p, n){
    # The probability that not everyone shows and we overbook
    1 - gamma - pbinom(N, n, p)
  }

  # The root of the objective function
  minD = which.min(abs(objectived(gamma = gamma,
                                  N = N, p = p,
                                  n = (N + 1):(N*1.1))))
  # The number of seats to book
  nd = N + minD

  plot(x = N:(N*1.1), y = objectived(gamma = gamma,
                                     N = N, p = p,
                                     n = N:(N*1.1)),
       ylab = "Objective Function",
       xlab = "n",
       pch = 21,
       bg = "blue",
       main = paste("Objective vs n to find optimal tickets sold \n(" , nd , ") gamma= " , gamma , " N = " , N , " continous", sep = ""))

  abline(h = 0, col = "red")
  abline(v = nd, col = "red")

  # Create plot of Objective function vs n (continuous)
  objectivec = function(gamma, N, p, n){
    # Continuous estimation
    1 - gamma - pnorm(N + 0.5, mean = n*p,
                      sd = sqrt(n*p*(1-p)))
  }

  # The root of the objective function
  x = NULL
  opt = optimize(function(x) abs(objectivec(
    gamma = gamma, N = N, p = p, n = x)),
    lower = N, upper = N*1.1)

  # The number of seats to book
  nc = opt$minimum

  curve(objectivec(gamma = gamma, N = N, p = p,
                   n = x),
        ylab = "Objective Function", xlab = "n",
        xlim = c(N,N*1.1), lwd = 2,
        main = paste("Objective vs n to find optimal tickets sold \n(" , nc , ") gamma= " , gamma , " N = " , N , " continous", sep = ""))

  abline(v = nc, col = "red")
  abline(h = 0, col = "red")


  # Print Names list containing nd, nc, N, p, and gamma
  list("nd" = nd, "nc" = nc, "gamma" = gamma, "N" = N, "p" = p)
}
