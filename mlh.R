mlh <- function(fit, L, M) {
  
  if(!inherits(fit, "maov"))
    stop("object must be of class \"manova\" or \"maov\"")
  
  
  if(  is.null(dim(L)) )
    L <- t(L)
  
  rss.qr <- qr( crossprod(  fit$residuals %*% M  ) )
  
  X <- as.matrix( model.matrix(fit) )
  B <- as.matrix( fit$coef )
  
  H <- t(M) %*% t(L%*%B)  %*%
    as.matrix(solve(L%*%solve(t(X)%*%X)%*%t(L)))  %*%
    (L%*%B)%*%M
  
  eig <- Re(eigen(qr.coef(rss.qr, H), symmetric = FALSE)$values)
  
  q <- nrow(L); df.res <- fit$df.residual
  
  test <- prod(1/(1 + eig))
  p <- length(eig)
  tmp1 <- df.res - 0.5 * (p - q + 1)
  tmp2 <- (p * q - 2)/4
  tmp3 <- p^2 + q^2 - 5
  tmp3 <-  if(tmp3 > 0) sqrt(((p*q)^2 - 4)/tmp3) else 1
  
  
  wilks <- test
  F     <- ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q
  df1   <- p * q
  df2   <- tmp1 * tmp3 - 2 * tmp2
  Prob  <- pf(F, df1, df2, lower.tail = FALSE)
  
  out <- list(wilks=wilks, F=F, df1=df1, df2=df2, Prob=Prob)
  out
}