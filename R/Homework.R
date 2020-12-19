#' @importFrom graphics barplot 
#' @importFrom stats cor lsfit qchisq rnorm runif var
#' @importFrom bootstrap crossval

#' @title Compute the sample skewness coeff using R.
#' @description Compute the sample skewness coeff using R.
#' @param x The sample we get.
#' @return the sample skewness coeff
#' @examples
#' \dontrun{
#' x<-runif(1000)
#' sk(x)
#' }
#' @export
sk = function(x) {
  xbar = mean(x)
  m3 = mean((x - xbar)^3)
  m2 = mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

#' @title A two sample test  for equality of variance using R.
#' @description It counts the number of extreme points of each sample relative to the range of the other sample using R.
#' @param x One sample we get.
#' @param y The other sample we get.
#' @return the value 1 (reject H0) or 0 (do not reject H0)
#' @examples
#' \dontrun{
#' n1 <- n2 <- 20
#' mu1 <- mu2 <- 0
#' sigma1 <- sgima2 <- 1
#' x <- rnorm(n1, mu1, sigma1)
#' y <- rnorm(n2, mu2, sigma2)
#' x <- x - mean(x)
#' y <- y - mean(y)
#' count5test(x, y)
#' }
#' @export
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
return(as.integer(max(c(outx, outy)) > 5))
}

#' @title Metropolis sampler for Laplace distribution.
#' @description Implement a random walk Metropolis sampler for generating the standard Laplace distribution.
#' @param sigma The Variance of normal distribution.
#' @param x0 Initial value.
#' @param N The length of the chain.
#' @return A chain.
#' @examples
#' \dontrun{
#' N <- 2000
#' sigma <- 0.5
#' x0 <- 25
#' rw1 <- rw.Metropolis(sigma, x0, N)
#' }
#' @export
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (exp(-abs(y))/2 / exp(-abs(x[i-1]))/2))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      } }
  return(list(x=x, k=k))
}

#' @title Gelman-Rubin method of monitoring convergence
#' @description Use the Gelman-Rubin method to monitor convergence of the chain.
#' @param psi A chain.
#' @return The diagnostic statistics.
#' @examples
#' \dontrun{
#' k <- 4
#' n <- 15000
#' b <- 1000
#' X <- matrix(0, nrow=k, ncol=n)
#' for (i in 1:k)
#' X[i, ] <-rw.Metropolis(sigma[i], x0, n)$x
#' psi <- t(apply(X, 1, cumsum)
#' for (i in 1:nrow(psi))
#' psi[i,] <- psi[i,] / (1:ncol(psi))
#' print(Gelman.Rubin(psi))
#' @export
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) 
  B <- n * var(psi.means)
  psi.w <- apply(psi, 1, "var") 
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + (B/n)
  r.hat <- v.hat / W
  return(r.hat)
}

