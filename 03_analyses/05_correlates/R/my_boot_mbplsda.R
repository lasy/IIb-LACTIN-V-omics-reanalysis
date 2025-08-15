
# -----------------------------------------------------------------------------------------
# bootstrap multiblock plsda
# -----------------------------------------------------------------------------------------


my_boot_mbplsda <- function(object, nrepet = 199, optdim, cpus=1, ...){
  
  if (!inherits(object, "mbplsda")) 
    stop("Object of type 'mbplsda' expected")
  if ((optdim < 0) | (optdim > object$rank)) 
    stop("Wrong number for optimal dimension")
  
  ## packages
  #  library(parallel)  # for jobs repartition 
  #  library(doParallel) # for iterations 
  #  library(foreach)
  
  ## get some arguments 
  appel  <- as.list(object$call)
  method <- as.character(appel[[1]])
  scale  <- eval.parent(appel$scale)
  option <- eval.parent(appel$option)
  if(inherits(try(eval.parent(appel$ktabX), silent = TRUE),"try-error")==TRUE) {
    stop("ktabX must be in the Global Environment")
  }
  X      <- eval.parent(appel$ktabX)
  if(inherits(try(eval.parent(appel$dudiY), silent = TRUE)[1],"try-error")==TRUE) {
    stop("dudiY must be in the Global Environment")
  }
  Y      <- eval.parent(appel$dudiY)  
  nr     <- nrow(Y$tab)  
  ncY    <- ncol(Y$tab)
  h      <- object$rank
  nblo   <- length(object$blo)  ## number of X tables 
  ncX    <- sum(X$blo)          ## total number of variables in X
  
  # Preparation of the parallelized processing
  #nodes <- detectCores()
  cl    <- makeCluster(cpus, type="PSOCK") # initialisation 
  registerDoParallel(cl) 
  on.exit(stopCluster(cl))
  
  resForeach <- NULL
  
  ## bootstrap and outputs
  resForeach <- foreach(i = 1:nrepet, .export=c("mbplsda", "inertie", "ginv"),.packages=c("ade4"), .errorhandling="remove") %dopar%{
    
    set.seed(seed = i)
    
    s     <- sample(x = nr, replace = TRUE)
    Xboot <- X[, s, ]
    Yboot <- Y[s, ] 
    
    resboot <- do.call(method, list(dudiY = Yboot, ktabX = Xboot, scale = scale, option = option, scannf = FALSE, nf = as.integer(optdim)))
    
    resFE<- list()
    resFE$XYcoef <- list()
    resFE$faX <- list()
    
    for (q in 1:ncY){
      resFE$XYcoef[[q]] <- resboot$XYcoef[[q]][, optdim]
    }
    resFE$bipc <- resboot$bipc[, optdim]
    resFE$vipc <- resboot$vipc[, optdim]
    resFE$rel_block_var <- sapply(names(X$blo), function(blo) Xboot[[blo]] |> apply(2, var) |> sum()) # added by LSY
    resFE$rel_block_var <-  resFE$rel_block_var/sum(resFE$rel_block_var)  # added by LSY
    for (l in 1:optdim){
      resFE$faX[[l]]  <- resboot$faX [,l]
    }
    
    resFE
  }
  stopCluster(cl)
  on.exit(stopCluster)
  #  resForeach
  
  nrepetFE <- length(resForeach)
  if((nrepetFE<1.5)|(is.null(nrepetFE)==TRUE)){
    stop("No adjustement of models")
  }
  
  ## prepare the outputs
  res <- list()    
  res$XYcoef <- list() 
  res$XYcoef <- rep(list(matrix(NA, ncol = ncX, nrow = nrepetFE, dimnames = list(NULL, colnames(object$tabX)))), ncY)
  res$bipc   <- matrix(NA, ncol = nblo, nrow = nrepetFE)
  colnames(res$bipc) <- names(X$blo)  
  res$vipc   <- matrix(NA, ncol = ncX, nrow = nrepetFE)
  colnames(res$vipc) <- colnames(object$tabX)
  res$rel_block_var <- matrix(NA, ncol = nblo, nrow = nrepetFE)  # added by LSY
  colnames(res$rel_block_var) <- names(X$blo)  # added by LSY
  res$faX <- list() 
  res$faX <- rep(list(matrix(NA, ncol = ncX, nrow = nrepetFE, dimnames = list(NULL, colnames(object$tabX)))), optdim)
  
  resu <- list()
  resu$XYcoef <- list() 
  resu$faX <- list() 
  
  
  ## results concatenation
  for(j in 1:nrepetFE){
    res$vipc[j,]   <- resForeach[[j]]$vipc
    res$bipc[j,]   <- resForeach[[j]]$bipc
    res$rel_block_var[j, ] <- resForeach[[j]]$rel_block_var  # added by LSY
    
    for (q in 1:ncY){res$XYcoef[[q]][j, ] <- resForeach[[j]]$XYcoef[[q]]}
    for (l in 1:optdim){res$faX[[l]][j, ] <- resForeach[[j]]$faX[[l]]}
  }
  
  ## functions for stat
  IC95 <- function(m){ # m is a vector
    rt <- c(rep(NA,2))
    if((sd(m, na.rm=TRUE)!=0) & (is.na(sd(m, na.rm=TRUE))==FALSE)){
      testt <- t.test(m, conf.level = 0.95)
      rt <- c(round(testt$conf.int[1],5),round(testt$conf.int[2],5))
    }
    return(rt)
  }
  
  
  stat.desc   <- function (x){ # x is a matrix, columns are variables, lines are repetitions
    nombre    <- colSums(!is.na(x))
    moy       <- round(colMeans(x,na.rm = TRUE),5)
    etype     <- round(apply(x,2,sd, na.rm=TRUE),5)
    quartiles <- round(t(apply(x, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)),5)
    IC        <- t(apply(x, 2, IC95))
    result    <- cbind.data.frame(nombre, moy, etype, IC, quartiles, stringsAsFactors = TRUE)
    colnames(result) <- c("nb", "mean", "sd", "95CIinf", "95CIsup","Q2.5", "median", "Q97.5")
    rownames(result) <- colnames(x)
    return(result)
  }
  
  
  
  ## means on repetitions
  block              <- unlist(list(sapply(1:nblo, function(b) rep(names(X$blo)[b], (X$blo)[b]))))
  blocks             <- names(X$blo) 
  variables          <- colnames(object$tabX)
  
  resu$XYcoef        <- lapply(1:ncY, function(x) cbind(variables,block,stat.desc(res$XYcoef[[x]])))
  names(resu$XYcoef) <- colnames(object$tabY)
  for(i in 1:ncY) {rownames(resu$XYcoef[[i]]) <- NULL}
  
  resu$faX           <- lapply(1:optdim, function(x) cbind(variables,block,stat.desc(res$faX[[x]])))
  names(resu$faX)    <- colnames(object$faX)[1:optdim]
  for(i in 1:optdim) {rownames(resu$faX[[i]]) <- NULL}
  
  resu$bipc          <- cbind(blocks,stat.desc(res$bipc))
  rownames(resu$bipc)<- NULL
  resu$vipc          <- cbind(variables,block,stat.desc(res$vipc))
  rownames(resu$vipc)<- NULL
  resu$rel_block_var <- cbind(blocks, stat.desc(res$rel_block_var))  # added by LSY
  rownames(resu$rel_block_var) <- NULL  # added by LSY
  
  resu$call          <- match.call()
  resu$res           <- res  # added by LSY
  class(resu)        <- c("boot_mbplsda")
  #resu
  
  return(resu) 
}



# ---------------------------------------------------------------------------
# 5. Additional functions
# ---------------------------------------------------------------------------


# Generalized inverse of a matrix (from MASS)
ginv <- function(X, tol = sqrt(.Machine$double.eps)){
  if (!is.matrix(X)) 
    X <- as.matrix(X)
  Xsvd <- svd(X)
  #  Xsvd <- svd(X,LINPACK = TRUE)
  if (is.complex(X)) 
    Xsvd$u <- Conj(Xsvd$u)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) 
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if (!any(Positive)) 
    array(0, dim(X)[2L:1L])
  else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
}


# Matrix (biaised) inertia
inertie <- function(tab){
  tab <- as.matrix(scale(tab, center = TRUE, scale = FALSE))
  V   <- tab %*% t(tab) / (dim(tab)[1])
  sum(diag(V))
}
