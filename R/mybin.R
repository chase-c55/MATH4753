#' My Binomial
#'
#' @param iter Number of iterations to run
#' @param n Number of trials
#' @param p Probability of Success
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @return A table of probabilities for the # of samples along with outputting a barplot
#' @export
#'
#' @examples mybin(iter = 10, n = 10, p = 0.5)
mybin = function(iter = 100, n = 10, p = 0.5){
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  succ = c()

  for(i in 1:iter){
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob =c(p,1-p))

    succ[i] = sum(sam.mat[,i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  barplot(succ.tab/iter, col = rainbow(n+1), main = "Binomial Simulation", xlab = "Number of Successes")
  tab = succ.tab/iter
}
