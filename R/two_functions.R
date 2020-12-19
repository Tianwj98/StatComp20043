#' @title R-square's k-fold cross validation.
#' @description The R-square 's k-fold cross validation is used to evaluate the generalization ability of regression model.
#' @param fit The fitted regression model.
#' @param k k-fold.
#' @return The original R-square and the k-fold cross-validated R-square.
#' @examples
#' \dontrun{
#' fit <- lm(z ~ x+y)
#' k <- 10
#' shrinkage(fit,k)
#' }
#' @export
shrinkage <- function(fit, k){
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit, x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

#' @title relative weight
#' @description Compute the relative weights of each predictor variable of the regression model.
#' @param fit The fitted regression model.
#' @return The relative weights of each predictor variable.
#' @examples
#' \dontrun{
#' fit <- lm(z ~ x+y)
#' relweights(fit)
#' }
#' @export
relweights <- function(fit){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt / rsquare)*100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls, ylab="% of R-square", xlab="Predictor Variables", 
          main="Relative Importance of Predictor Variables", sub=paste("r-square=", round(rsquare, digits=3)))
  return(import)
}