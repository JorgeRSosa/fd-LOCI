# funcion para generar curvas con dependencia ar(1)
func.sim <- function(n=2,mdata,mu,sigma,rho=0,mu2=NULL,m=NULL){
  #n numero de curvas
  #mdata cuantos puntos tiene cada curva
  #mu media
  #mu2 es la media de la hip?tesis alternativa y simular datos at?picos 
  #sigma matriz de varianzas y covarianzas
  #rho factor de dependencia
  if(is.null(mu2) ){
  if (rho != 0) sigma <- sigma * sqrt((1+rho)/(1-rho))
  C=svd(t(sigma))
  L.corr.teor=(C$u%*%diag(sqrt(C$d)))
  
  # Generacion de datos simulados: Y = m(X) + s(X) * E
  data.err <- L.corr.teor%*%matrix(rnorm(mdata * n), nrow = mdata)
  #data.err <- data.err * sqrt((1-rho)/(1+rho))
  err <- matrix(nrow = mdata, ncol = n)
  res <- matrix(nrow = mdata, ncol = n)
  
    if (rho != 0) {
      err[, 1] <- data.err[, 1] * sqrt((1-rho)/(1+rho))
      for(i in 2:n)
        err[, i] <- rho*err[, i-1] + (1 - rho)*data.err[, i]
    }
    #Datos simulados: Y = m(x) + S(x)*E
    res <- drop(mu) + err
  }
  else{
    if(!is.null(m)){
    if (rho != 0) sigma <- sigma * sqrt((1+rho)/(1-rho))
    C=svd(t(sigma))
    L.corr.teor=(C$u%*%diag(sqrt(C$d)))
    
    # Generacion de datos simulados: Y = m(X) + s(X) * E
    data.err <- L.corr.teor%*%matrix(rnorm(mdata * (n+m)), nrow = mdata)
    #data.err <- data.err * sqrt((1-rho)/(1+rho))
    err <- matrix(nrow = mdata, ncol = (n+m))
    res <- matrix(nrow = mdata, ncol = (n+m))
    if (rho != 0) {
      err[, 1] <- data.err[, 1] * sqrt((1-rho)/(1+rho))
      for(i in 2:(n+m))
        err[, i] <- rho*err[, i-1] + (1 - rho)*data.err[, i]
    }
    #Datos simulados: Y = m(x) + S(x)*E
    res[,1:n] <- drop(mu) + err[,1:n]
    res[,(n+1):(n+m)] <- drop(mu2) + err[,(n+1):(n+m)]
    }
    else 
      stop("Parameter m must be numeric")
  }
  
  return(t(res))
}