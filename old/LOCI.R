
###############################FUNCION LOCI A MODIFICAR#####################

fLOCI <- function (fdataobj, alpha = 0.5, nn = 20, k = 3) 
{
  if (!is.fdata(fdataobj)) 
    fdataobj = fdata(fdataobj)
  x <- fdataobj[["data"]]
  tt <- fdataobj[["argvals"]]
  rtt <- fdataobj[["rangeval"]]
  n <- nrow(fdataobj)
  rowname.ori <- row.names(fdataobj[["data"]])
  rowname.num <- 1:n
  row.names(fdataobj[["data"]]) <- rowname.num
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) 
    stop("Error in the data dimensions")
  
  distMatrix <- metric.lp(fdataobj)#norma L^2
  
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
    sortVector <- sort(distMatrix[i, ])
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
  return(returnList)
}



