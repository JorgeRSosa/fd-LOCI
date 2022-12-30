
###############################FUNCION LOCI A MODIFICAR#####################

fLOCI <- function (fdataobj, alpha = 0.5, nn = 20, k = 3, dist=c('L2','L2e','L2d','s.L2d5'),nindex=NULL) 
{
  if(is.null(nindex)){
    dist = match.arg(dist)
    if (!is.fdata(fdataobj)) 
      fdataobj = fdata(fdataobj)

    n <- nrow(fdataobj) # numero de curvas
    rowname.ori <- row.names(fdataobj[["data"]])
    rowname.num <- 1:n
    row.names(fdataobj[["data"]]) <- rowname.num
    m <- ncol(fdataobj) # numero de puntos observados en cada curva
    if (is.null(n) && is.null(m))
    {
      stop("Error in the data dimensions")
    }
    
    
    if(dist=='L2')
    {
      distMatrix <- metric.lp(fdataobj)#norma L^2
    }
    else
    { 
      if(dist=='L2e')
      {
        
        distMatrix = matrix(0, n, n)
        #phs_dist = matrix(0, n, n)
        time = seq(0, 1, length.out = m)
        for (f1 in 1:(n - 1)) {
          dist = future_sapply(f1:n, function(y) {
            unlist(elastic.distance(fdataobj$data[f1, ], fdataobj$data[y, ], time)) #fdasrvf
          })
          #phs_dist[f1, f1:n] = dist[2, ]
          distMatrix[f1, f1:n] = dist[1, ]
        }
        distMatrix = distMatrix + t(distMatrix)
        #phs_dist = phs_dist + t(phs_dist)
      }
    }
    
    if (!is.numeric(k) | !is.numeric(alpha) | !is.numeric(nn)) {
      stop("all input parameters alpha, nn and k must be numeric")
    }
    if (k > n || k < 1) {
      stop("k input must be less than number of observations and greater than 0")
    }
    
    
    npar_pi <- NULL
    avg_npar <- NULL
    MDEF <- NULL
    sd_npar <- NULL
    norm_MDEF <- NULL
    class <- NULL
    
    for (i in 1:nrow(distMatrix)) {
      dist_pi=distMatrix[i, ]
      sortVector <-order(dist_pi) #no usar sort
      knn <- sortVector[1:nn]
      radius <- dist_pi[knn[nn]] * alpha
      npar <- NULL
      for (j in 1:nn) {
        np_obs <- knn[j]
        np_obs_allNN <- distMatrix[np_obs, ]
        np_obs_radiusNN <- length(np_obs_allNN[which(np_obs_allNN <= 
                                                       radius)])
        npar[j] <- np_obs_radiusNN
      }
      npar_pi[i] <- length(knn[which(knn <= radius)])
      avg_npar[i] <- (sum(npar)/nn)
      sd_npar[i] <- sd(npar)
      MDEF[i] <- 1 - (npar_pi[i]/avg_npar[i])
      norm_MDEF[i] <- sd_npar[i]/avg_npar[i]
      if ((MDEF[i] > (k * norm_MDEF[i])) == TRUE) {
        class[i] <- "Outlier"
      }
      else class[i] <- "Inlier"
    }
    returnList <- list(npar_pi = npar_pi, avg_npar = avg_npar, 
                       sd_npar = sd_npar, MDEF = MDEF, norm_MDEF = norm_MDEF, 
                       class = class)
    }
  else{
   
    dist = match.arg(dist)
    if (!is.fdata(fdataobj)) 
      fdataobj = fdata(fdataobj)

    n <- nrow(fdataobj) # numero de curvas
    rowname.ori <- row.names(fdataobj[["data"]])
    rowname.num <- 1:n
    row.names(fdataobj[["data"]]) <- rowname.num
    m <- ncol(fdataobj) # numero de puntos observados en cada curva
    if (is.null(n) && is.null(m))
    {
      stop("Error in the data dimensions")
    }
    
    
    if(dist=='L2')
    {
      distMatrix <- metric.lp(fdataobj)#norma L^2
    }
    else
    { 
      if(dist=='L2e')
      {
        
        distMatrix = matrix(0, n, n)
        #phs_dist = matrix(0, n, n)
        time = seq(0, 1, length.out = m)
        for (f1 in 1:(n - 1)) {
          dist = future_sapply(f1:n, function(y) {
            unlist(elastic.distance(fdataobj$data[f1, ], fdataobj$data[y, ], time)) #fdasrvf
          })
          #phs_dist[f1, f1:n] = dist[2, ]
          distMatrix[f1, f1:n] = dist[1, ]
        }
        distMatrix = distMatrix + t(distMatrix)
        #phs_dist = phs_dist + t(phs_dist)
      }
    }
    
    if (!is.numeric(k) | !is.numeric(alpha) | !is.numeric(nn)) {
      stop("all input parameters alpha, nn and k must be numeric")
    }
    if (k > n || k < 1) {
      stop("k input must be less than number of observations and greater than 0")
    }
    
    npar_pi <- NULL
    avg_npar <- NULL
    MDEF <- NULL
    sd_npar <- NULL
    norm_MDEF <- NULL
    class <- NULL
    
    sortVector <- sort(distMatrix[nindex, ])
    knn <- sortVector[1:nn]
    radius <- knn[[nn]] * alpha
    npar <- NULL
    for (j in 1:length(knn)) {
      np_obs <- names(knn)[j]
      np_obs_allNN <- distMatrix[np_obs, ]
      np_obs_radiusNN <- length(np_obs_allNN[which(np_obs_allNN <= 
                                                     radius)])
      npar[j] <- np_obs_radiusNN
    }
    npar_pi <- length(knn[which(knn <= radius)])
    avg_npar <- (sum(npar)/nn)
    sd_npar <- sd(npar)
    MDEF <- 1 - (npar_pi/avg_npar)
    norm_MDEF <- sd_npar/avg_npar
    if ((MDEF > (k * norm_MDEF)) == TRUE) {
      class <- "Outlier"
    }else class <- "Inlier"
    
    returnList <- list(npar_pi = npar_pi, avg_npar = avg_npar, 
                       sd_npar = sd_npar, MDEF = MDEF, norm_MDEF = norm_MDEF, 
                       class = class,mdist=distMatrix)
  }
  return(returnList)
}