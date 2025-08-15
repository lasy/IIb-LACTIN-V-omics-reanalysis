


cv_mbplsda <- function(object, nrepet, prop = 2/3, algo = c("max")){
  algo <- match.arg(algo)
  
  cv_res <- map(1:nrepet, one_cv_mbplsda, object = object, prop = prop, algo = algo) |> bind_rows()
  
  summary_by_fold_and_class <- 
    cv_res |> 
    group_by(dim, cv_id, set, true_class, pred_class) |>
    summarize(n = n(), .groups = "drop") |> 
    right_join(
      expand_grid(dim = unique(cv_res$dim), cv_id = unique(cv_res$cv_id), set = unique(cv_res$set), true_class = unique(cv_res$true_class), pred_class = unique(cv_res$pred_class)),
      by = join_by(dim, cv_id, set, true_class, pred_class)
    ) |> 
    arrange(dim, cv_id, set, true_class, pred_class) |>
    mutate(n = n |> replace_na(0)) |> 
    group_by(dim, cv_id, set, true_class) |> 
    mutate(
      TP = max(0, n[true_class == pred_class]),
      P = sum(n),
    ) |> 
    group_by(dim, cv_id, set, pred_class) |>
    mutate(
      PP = sum(n)
    ) |> 
    group_by(dim, cv_id, set) |> 
    mutate(
      precision = TP/PP,
      recall = TP/P
    ) |> 
    filter(true_class == pred_class) |> 
    mutate(
      F1 = 2 * precision * recall / (precision + recall),
      F1 = F1 |> replace_na(0)
    ) |> 
    ungroup()
  
  summary_by_fold <- 
    cv_res |> 
    group_by(dim, cv_id, set) |> 
    summarize(
      n = n(),
      accuracy = mean(pred_class == true_class),
      RMSE = (pred_scores - true_scores)^2 |> rowMeans() |> mean() |> sqrt(),
      .groups = "drop"
    ) |> 
    left_join(
      summary_by_fold_and_class |> 
        group_by(dim, cv_id, set) |> 
        summarize(
          `average F1 score` = mean(F1),
          `weighted F1 score` = weighted.mean(F1, w = P/sum(P)),
          .groups = "drop"
        ),
      by = join_by(dim, cv_id, set)
    ) 
  
  summary <- 
    summary_by_fold |> 
    mutate(`error rate` = 1 - accuracy) |> 
    pivot_longer(cols = c(RMSE, accuracy, `error rate`, `average F1 score`, `weighted F1 score`), names_to = "metric", values_to = "value") |>
    # mutate(dim = ifelse(set == "random permutation", NA, dim)) |> 
    group_by(metric, set, dim, n) |> 
    summarize(
      mean = mean(value),
      sd = sd(value),
      median = median(value),
      IQR_lo = quantile(value, 0.25),
      IQR_hi = quantile(value, 0.75),
      .groups = "drop"
    ) |> 
    mutate(
      `95%CI_lo` = mean - qt(0.975, n - 1) * sd / sqrt(n),
      `95%CI_hi` = mean + qt(0.975, n - 1) * sd / sqrt(n)
    ) |> 
    select(metric, set, n, dim, mean, sd, `95%CI_lo`, `95%CI_hi`, median, IQR_lo, IQR_hi)
  
  
  list(cv_res = cv_res, summary_by_fold_and_class = summary_by_fold_and_class, summary_by_fold = summary_by_fold, summary = summary)
}

one_cv_mbplsda <- function(i, object, prop, algo, threshold){
  N <- nrow(object$tabX)
  ktabX <- tabX_to_ktabX(object)
  dudiY <- object$tabY |> as.dudi(col.w = rep(1,ncol(object$tabY)), row.w = rep(1, N), scannf = FALSE, nf = 3, call = "", type = "")
  true_scores <- object$tabY |> as_tibble()
  true_binary_scores <- true_scores |> apply(1, function(x) (x == max(x)) * 1) |> t() |> as_tibble()
  true_class <- true_binary_scores |> colnames() |> fct_inorder() |> extract(apply(true_binary_scores, 1, which.max))
  
  # split into calibration and validation
  ic <- caret::createDataPartition(true_class, p = prop, list = FALSE) |> as.vector()
  iv <- setdiff(1:N, ic)
  
  # calibration
  ktabX_cal <- ktabX[,ic,]
  dudiY_cal <- dudiY[ic,]

  mod_cal <- mbplsda(dudiY = dudiY_cal, ktabX = ktabX_cal, scale = FALSE, option = "none", scannf = FALSE, nf = object$nf)
  
  ## performance in calibration
  true_class_cal <- true_class[ic]
  true_scores_cal <- true_scores[ic,]
  pred_cal <- 
    map(
      1:object$nf, 
      ~predict_mbplsda(mod_cal, optdim = ., algo = "max") |> 
        mutate(
          true_class = true_class_cal, 
          true_scores = true_scores_cal, 
          dim = ., 
          sample_id = object$tabY[ic,] |> rownames(), 
          cv_id = i
        )
    ) |> 
    bind_rows()
  
  # validation
  tabX_val <- object$tabX[iv,]
  true_class_val <- true_class[iv]
  true_scores_val <- true_scores[iv,]
  pred_val <- 
    map(
      1:object$nf, 
      ~predict_mbplsda(mod_cal, newdata = tabX_val, optdim = ., algo = "max") |> 
        mutate(
          true_class = true_class_val,
          true_scores = true_scores_val, 
          dim = ., 
          sample_id = object$tabY[iv,] |> rownames(), 
          cv_id = i
          )
      ) |> 
    bind_rows()
  
  # random predictions
  pred_rand <- pred_val |> filter(dim == 1) |> mutate(dim = NA)
  i_rand <- sample(1:nrow(true_scores_cal), nrow(pred_rand))
  pred_rand <- 
    pred_rand |> 
    mutate(
      pred_scores = true_scores_cal[i_rand,],
      pred_class = true_class_cal[i_rand]
      )
  
  bind_rows(
    pred_cal |> mutate(set = "calibration"),
    pred_val |> mutate(set = "validation"),
    pred_rand |> mutate(set = "random predictions")
    ) |> 
    mutate(
      set = set |> fct_inorder()
    )
}

predict_mbplsda <- function(object, newdata = NULL, optdim, algo = c("max")){
  
  if (is.null(newdata)) Xmat <- object$tabX |> as.matrix() else Xmat <- newdata |> as.matrix()
  
  coef <- map(object$XYcoef.raw, ~.x[,optdim]) |> bind_cols() |> as.matrix()
  beta_0 <- map(object$intercept.raw, ~.x[,optdim]) |> bind_cols() |> extract(rep(1, nrow(Xmat)),)
  
  Y_hat <- beta_0 + Xmat %*% coef
# Y_hat_class_disj <- Y_hat |> apply(1, function(x) (x == max(x))*1) |> t()
  Y_hat_class <- Y_hat |> colnames() |> fct_inorder() |> extract(apply(Y_hat, 1, which.max))
  tibble(pred_scores = Y_hat |> as_tibble(), pred_class = Y_hat_class)
}


tabX_to_ktabX <- function(object){
  names(object$blo) |> 
    map(~retrieve_block_data(., object)) |> 
    set_names(names(object$blo)) |>
    ktab.list.df()
}

retrieve_block_data <- function(block_name, object){
  object$tabX[, which(object$TC$T == block_name)]
}




cv_mbplsda_mod <- 
  function (object, nrepet = 10, algo = c("max", "gravity", "threshold"), 
            threshold = 0.5, bloY, outputs = c("ER", "ConfMat", "AUC"), 
            cpus = 1) 
  {
    if (!inherits(object, "mbplsda")) 
      stop("Object of type 'mbplsda' expected")
    appel <- as.list(object$call)
    method <- as.character(appel[[1]])
    scale <- eval.parent(appel$scale)
    option <- eval.parent(appel$option)
    if (inherits(try(eval.parent(appel$ktabX), silent = TRUE), 
                 "try-error") == TRUE) {
      stop("ktabX must be in the Global Environment")
    }
    X <- eval.parent(appel$ktabX)
    if (inherits(try(eval.parent(appel$dudiY), silent = TRUE)[1], 
                 "try-error") == TRUE) {
      stop("dudiY must be in the Global Environment")
    }
    Y <- eval.parent(appel$dudiY)
    nr <- nrow(Y$tab)
    q <- ncol(Y$tab)
    h <- object$rank
    Nc <- round(2 * nr/3) # Number of samples in the calibration set
    Nv <- nr - Nc # Number of samples in the validation set
    Ky <- length(bloY)
    Var <- as.factor(rep(1:Ky, bloY)) # ? variables to be predicted
    cnames <- colnames(Y$tab) # Names of the variables to be predicted
    nblo <- length(X$blo)
    blo <- sapply(1:nblo, function(k) dim(X[[k]])[2])
    Bloc <- as.factor(rep(1:nblo, blo))
    nNoBin <- sum(bloY != 2) # Number of non-binary ?
    cl <- makeCluster(cpus, type = "PSOCK")
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
    resForeach <- foreach(i = 1:nrepet, .export = c("mbplsda", 
                                                    "inertia"), .packages = c("ade4", "pROC"), .errorhandling = "remove") %dopar%  # , "ginv"
      {
        set.seed(seed = i)
        s <- sample(x = nr, size = Nc)
        Xc <- X[, s, ]
        Xv <- X[, -s, ]
        Yc <- Y[s, ]
        Yv <- Y[-s, ]
        rnamesXc <- row.names(Xc)
        rnamesXv <- row.names(Xv)
        rnamesYc <- row.names(Yc$tab)
        rnamesYv <- row.names(Yv$tab)
        nbY1c <- sapply(1:q, function(g) sum(Yc$tab[, g] == 1))
        nbY1v <- sapply(1:q, function(g) sum(Yv$tab[, g] == 1))
        rescal <- do.call(method, list(dudiY = Yc, ktabX = Xc, 
                                       scale = scale, option = option, scannf = FALSE, 
                                       nf = h))
        resval <- do.call(method, list(dudiY = Yv, ktabX = Xv, 
                                       scale = scale, option = option, scannf = FALSE, 
                                       nf = h))
        H = min(rescal$rank, resval$rank, h) # max number of components
        Xc.mat <- cbind.data.frame(unclass(Xc)[1:nblo], stringsAsFactors = TRUE)
        rescal$meanX <- colMeans(Xc.mat)
        rescal$sdX <- apply(Xc.mat, 2, sd) * sqrt((Nc - 1)/Nc)
        Xv.raw <- cbind.data.frame(unclass(Xv)[1:nblo], stringsAsFactors = TRUE)
        Xv.c <- sweep(Xv.raw, 2, rescal$meanX, FUN = "-")
        if (scale == TRUE) {
          Xv.cr <- sweep(Xv.c, 2, rescal$sdX, FUN = "/")
          if (option == "uniform") {
            Xv.crw <- Xv.cr * sqrt(matrix(rep(rescal$X.cw, 
                                              each = Nv), nrow = Nv))
          }
          else {
            Xv.crw <- Xv.cr
          }
        }
        if (scale == FALSE) {
          Xv.cr <- Xv.c
          if (option == "uniform") {
            Xv.crw <- Xv.cr * sqrt(matrix(rep(rescal$X.cw, 
                                              each = Nv), nrow = Nv))
          }
          else {
            Xv.crw <- Xv.cr
          }
        }
        RMSE_ErrorRates <- list(NULL)
        if ("RMSE" %in% outputs) {
          RMSE_ErrorRates$RMSEglobal <- matrix(NA, nrow = H, 
                                               ncol = 2, dimnames = list(1:H, c("RMSEC", "RMSEV")))
        }
        classYc <- list(NULL)
        classYv <- list(NULL)
        if ("max" %in% algo) {
          ClassPredYc.max <- list(NULL)
          ClassPredYv.max <- list(NULL)
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcM <- RMSE_ErrorRates$TPvM <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcM <- RMSE_ErrorRates$TNvM <- RMSE_ErrorRates$FPcM <- RMSE_ErrorRates$FPvM <- RMSE_ErrorRates$FNcM <- RMSE_ErrorRates$FNvM <- matrix(NA, 
                                                                                                                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                       cnames))
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcM <- RMSE_ErrorRates$ERvM <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
            RMSE_ErrorRates$ERcMglobal <- RMSE_ErrorRates$ERvMglobal <- matrix(NA, 
                                                                               ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                          c(paste0("Var", 1:Ky), "global")))
          }
        }
        if ("gravity" %in% algo) {
          ClassPredYc.gravity <- list(NULL)
          ClassPredYv.gravity <- list(NULL)
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcG <- RMSE_ErrorRates$TPvG <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcG <- RMSE_ErrorRates$TNvG <- RMSE_ErrorRates$FPcG <- RMSE_ErrorRates$FPvG <- RMSE_ErrorRates$FNcG <- RMSE_ErrorRates$FNvG <- matrix(NA, 
                                                                                                                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                       cnames))
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcG <- RMSE_ErrorRates$ERvG <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
            RMSE_ErrorRates$ERcGglobal <- RMSE_ErrorRates$ERvGglobal <- matrix(NA, 
                                                                               ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                          c(paste0("Var", 1:Ky), "global")))
          }
        }
        if ("threshold" %in% algo) {
          ClassPredYc.threshold <- list(NULL)
          ClassPredYv.threshold <- list(NULL)
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcT <- RMSE_ErrorRates$TPvT <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcT <- RMSE_ErrorRates$TNvT <- RMSE_ErrorRates$FPcT <- RMSE_ErrorRates$FPvT <- RMSE_ErrorRates$FNcT <- RMSE_ErrorRates$FNvT <- matrix(NA, 
                                                                                                                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                       cnames))
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcT <- RMSE_ErrorRates$ERvT <- matrix(NA, 
                                                                   ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                       cnames))
            RMSE_ErrorRates$ERcTglobal <- RMSE_ErrorRates$ERvTglobal <- matrix(NA, 
                                                                               ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                          c(paste0("Var", 1:Ky), "global")))
          }
        }
        if ((nNoBin == 0) & ("AUC" %in% outputs)) {
          RMSE_ErrorRates$AUCc <- matrix(NA, ncol = q, 
                                         nrow = H, dimnames = list(1:H, cnames))
          RMSE_ErrorRates$AUCv <- matrix(NA, ncol = q, 
                                         nrow = H, dimnames = list(1:H, cnames))
          RMSE_ErrorRates$AUCcglobal <- RMSE_ErrorRates$AUCvglobal <- matrix(NA, 
                                                                             ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                        c(paste0("Var", 1:Ky), "Mean")))
        }
        for (j in 1:H) {
          classYc[[j]] <- matrix(NA, Nc, ncol = 3 * Ky, 
                                 dimnames = list(rnamesYc, c(paste0("Ymax_Var", 
                                                                    1:Ky), paste0("Ycentroid_Var", 1:Ky), paste0("Ythreshold_Var", 
                                                                                                                 1:Ky))))
          classYv[[j]] <- matrix(NA, Nv, ncol = 3 * Ky, 
                                 dimnames = list(rnamesYv, c(paste0("Ymax_Var", 
                                                                    1:Ky), paste0("Ycentroid_Var", 1:Ky), paste0("Ythreshold_Var", 
                                                                                                                 1:Ky))))
          if ("max" %in% algo) {
            ClassPredYc.max[[j]] <- matrix(0, Nc, ncol = q, 
                                           dimnames = list(rnamesYc, cnames))
            ClassPredYv.max[[j]] <- matrix(0, Nv, ncol = q, 
                                           dimnames = list(rnamesYv, cnames))
          }
          if ("gravity" %in% algo) {
            ClassPredYc.gravity[[j]] <- matrix(0, Nc, ncol = q, 
                                               dimnames = list(rnamesYc, cnames))
            ClassPredYv.gravity[[j]] <- matrix(0, Nv, ncol = q, 
                                               dimnames = list(rnamesYv, cnames))
          }
          if ("threshold" %in% algo) {
            ClassPredYc.threshold[[j]] <- matrix(NA, Nc, 
                                                 ncol = q, dimnames = list(rnamesYc, cnames))
            ClassPredYv.threshold[[j]] <- matrix(NA, Nv, 
                                                 ncol = q, dimnames = list(rnamesYv, cnames))
          }
          XYcoef.raw.cal <- sapply(rescal$XYcoef.raw, function(x) x[, 
                                                                    j])
          intercept.raw.cal <- sapply(rescal$intercept.raw, 
                                      function(x) x[, j])
          predYc <- matrix(rep(intercept.raw.cal, each = Nc), 
                           ncol = q) + as.matrix(Xc.mat) %*% XYcoef.raw.cal
          predYv <- matrix(rep(intercept.raw.cal, each = Nv), 
                           ncol = q) + as.matrix(Xv.raw) %*% XYcoef.raw.cal
          colnames(predYc) <- colnames(predYv) <- cnames
          if ("RMSE" %in% outputs) {
            residYc <- as.matrix(Yc$tab) - predYc
            RMSE_ErrorRates$RMSEglobal[j, "RMSEC"] <- sqrt(sum(residYc^2)/(Nc * 
                                                                             q))
            residYv <- as.matrix(Yv$tab) - predYv
            RMSE_ErrorRates$RMSEglobal[j, "RMSEV"] <- sqrt(sum(residYv^2)/(Nv * 
                                                                             q))
          }
          if ("max" %in% algo) {
            for (k in 1:Ky) {
              classYc[[j]][, k] <- sapply(1:Nc, function(n) which.max(predYc[n, 
                                                                             Var == k]))
              classYv[[j]][, k] <- sapply(1:Nv, function(n) which.max(predYv[n, 
                                                                             Var == k]))
              for (n in 1:Nc) {
                if (k == 1) 
                  (ClassPredYc.max[[j]][n, classYc[[j]][n, 
                                                        1]] <- 1)
                if (k > 1) 
                  (ClassPredYc.max[[j]][n, (sum(bloY[1:(k - 
                                                          1)]) + classYc[[j]][n, k])] <- 1)
              }
              for (n in 1:Nv) {
                if (k == 1) 
                  (ClassPredYv.max[[j]][n, classYv[[j]][n, 
                                                        1]] <- 1)
                if (k > 1) 
                  (ClassPredYv.max[[j]][n, (sum(bloY[1:(k - 
                                                          1)]) + classYv[[j]][n, k])] <- 1)
              }
            }
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPcM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                                l] == 1 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNcM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                                l] == 0 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FPcM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                                l] == 1 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FNcM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                                l] == 0 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERcM[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYc.max[[j]][, 
                                                                                                 l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.max[[j]][, 
                                                                                                                                                                  l] == 0 & Yc$tab[, l] == 1)))/Nc)
            }
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPvM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                                l] == 1 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNvM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                                l] == 0 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FPvM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                                l] == 1 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FNvM[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                                l] == 0 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERvM[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYv.max[[j]][, 
                                                                                                 l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.max[[j]][, 
                                                                                                                                                                  l] == 0 & Yv$tab[, l] == 1)))/Nv)
            }
          }
          if ("gravity" %in% algo) {
            Gravity <- matrix(0, nrow = q, ncol = j, dimnames = list(cnames, 
                                                                     1:j))
            if (j == 1) {
              Gravity[, 1] <- (sapply(1:q, function(g) {
                if (nbY1c[g] > 1) 
                  (mean(rescal$lX[Yc$tab[, g] == 1, 1]))
                else if (nbY1c[g] == 1) 
                  (rescal$lX[Yc$tab[, g] == 1, 1])
                else if (nbY1c[g] == 0) 
                  (NA)
              }))
            }
            else {
              Gravity[, 1:j] <- (t(sapply(1:q, function(g) {
                if (nbY1c[g] > 1) 
                  (apply(rescal$lX[Yc$tab[, g] == 1, 1:j], 
                         2, mean))
                else if (nbY1c[g] == 1) 
                  (rescal$lX[Yc$tab[, g] == 1, 1:j])
                else if (nbY1c[g] == 0) 
                  (rep(NA, times = j))
              })))
            }
            dist.eucl.gravity.Yc <- sapply(1:q, function(g) apply((rescal$lX[, 
                                                                             1:j] - matrix(rep(Gravity[g, ], each = Nc), 
                                                                                           nrow = Nc))^2, 1, sum))
            dist.eucl.gravity.Yv <- sapply(1:q, function(g) apply(((as.matrix(Xv.crw) %*% 
                                                                      rescal$faX[, 1:j]) - matrix(rep(Gravity[g, 
                                                                      ], each = Nv), nrow = Nv))^2, 1, sum))
            for (k in 1:Ky) {
              classYc[[j]][, (k + Ky)] <- sapply(1:Nc, 
                                                 function(n) which.min(dist.eucl.gravity.Yc[n, 
                                                                                            Var == k]))
              classYv[[j]][, (k + Ky)] <- sapply(1:Nv, 
                                                 function(n) which.min(dist.eucl.gravity.Yv[n, 
                                                                                            Var == k]))
              for (n in 1:Nc) {
                if (k == 1) 
                  (ClassPredYc.gravity[[j]][n, classYc[[j]][n, 
                                                            (1 + Ky)]] <- 1)
                if (k > 1) 
                  (ClassPredYc.gravity[[j]][n, (sum(bloY[1:(k - 
                                                              1)]) + classYc[[j]][n, (k + Ky)])] <- 1)
              }
              for (n in 1:Nv) {
                if (k == 1) 
                  (ClassPredYv.gravity[[j]][n, classYv[[j]][n, 
                                                            (1 + Ky)]] <- 1)
                if (k > 1) 
                  (ClassPredYv.gravity[[j]][n, (sum(bloY[1:(k - 
                                                              1)]) + classYv[[j]][n, (k + Ky)])] <- 1)
              }
            }
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPcG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                    l] == 1 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNcG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                    l] == 0 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FPcG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                    l] == 1 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FNcG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                    l] == 0 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERcG[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYc.gravity[[j]][, 
                                                                                                     l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.gravity[[j]][, 
                                                                                                                                                                          l] == 0 & Yc$tab[, l] == 1)))/Nc)
            }
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPvG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                    l] == 1 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNvG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                    l] == 0 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FPvG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                    l] == 1 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FNvG[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                    l] == 0 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERvG[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYv.gravity[[j]][, 
                                                                                                     l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.gravity[[j]][, 
                                                                                                                                                                          l] == 0 & Yv$tab[, l] == 1)))/Nv)
            }
          }
          if ("threshold" %in% algo) {
            ClassPredYc.threshold[[j]][predYc >= threshold] <- 1
            ClassPredYv.threshold[[j]][predYv >= threshold] <- 1
            ClassPredYc.threshold[[j]][predYc < threshold] <- 0
            ClassPredYv.threshold[[j]][predYv < threshold] <- 0
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPcT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                      l] == 1 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNcT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                      l] == 0 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FPcT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                      l] == 1 & Yc$tab[, l] == 0))/Nc)
              RMSE_ErrorRates$FNcT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                      l] == 0 & Yc$tab[, l] == 1))/Nc)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERcT[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYc.threshold[[j]][, 
                                                                                                       l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.threshold[[j]][, 
                                                                                                                                                                              l] == 0 & Yc$tab[, l] == 1)))/Nc)
            }
            if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
              RMSE_ErrorRates$TPvT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                      l] == 1 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ConfMat" %in% outputs) {
              RMSE_ErrorRates$TNvT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                      l] == 0 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FPvT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                      l] == 1 & Yv$tab[, l] == 0))/Nv)
              RMSE_ErrorRates$FNvT[j, ] <- sapply(1:q, 
                                                  function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                      l] == 0 & Yv$tab[, l] == 1))/Nv)
            }
            if ("ER" %in% outputs) {
              RMSE_ErrorRates$ERvT[j, ] <- sapply(1:q, 
                                                  function(l) (length(which(ClassPredYv.threshold[[j]][, 
                                                                                                       l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.threshold[[j]][, 
                                                                                                                                                                              l] == 0 & Yv$tab[, l] == 1)))/Nv)
            }
            for (k in 1:Ky) {
              classYc[[j]][, (2 * Ky + k)] <- sapply(1:Nc, 
                                                     function(n) if (sum(ClassPredYc.threshold[[j]][n, 
                                                                                                    Var == k], na.rm = T) == 1) 
                                                       (which.max(ClassPredYc.threshold[[j]][n, 
                                                                                             Var == k])))
              classYv[[j]][, (2 * Ky + k)] <- sapply(1:Nv, 
                                                     function(n) if (sum(ClassPredYv.threshold[[j]][n, 
                                                                                                    Var == k], na.rm = T) == 1) 
                                                       (which.max(ClassPredYv.threshold[[j]][n, 
                                                                                             Var == k])))
            }
          }
          if ((nNoBin == 0) & (sum(sum(nbY1v == 0), sum(nbY1c == 
                                                        0)) == 0) & ("AUC" %in% outputs)) {
            RMSE_ErrorRates$AUCc[j, ] <- sapply(1:q, function(l) auc(Yc$tab[, 
                                                                            l], predYc[, l]))
            RMSE_ErrorRates$AUCv[j, ] <- sapply(1:q, function(l) auc(Yv$tab[, 
                                                                            l], predYv[, l]))
          }
          if ("ER" %in% outputs) {
            if ("max" %in% algo) {
              RMSE_ErrorRates$ERcMglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum(RMSE_ErrorRates$TPcM[j, 
                                                                                                      Var == k])))
              RMSE_ErrorRates$ERvMglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum(RMSE_ErrorRates$TPvM[j, 
                                                                                                      Var == k])))
              RMSE_ErrorRates$ERcMglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYc.max[[j]] - Yc$tab)^2), 
                           sum, MARGIN = 1)) == 0)/Nc
              RMSE_ErrorRates$ERvMglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYv.max[[j]] - Yv$tab)^2), 
                           sum, MARGIN = 1)) == 0)/Nv
            }
            if ("gravity" %in% algo) {
              RMSE_ErrorRates$ERcGglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum(RMSE_ErrorRates$TPcG[j, 
                                                                                                      Var == k])))
              RMSE_ErrorRates$ERvGglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum(RMSE_ErrorRates$TPvG[j, 
                                                                                                      Var == k])))
              RMSE_ErrorRates$ERcGglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYc.gravity[[j]] - 
                               Yc$tab)^2), sum, MARGIN = 1)) == 0)/Nc
              RMSE_ErrorRates$ERvGglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYv.gravity[[j]] - 
                               Yv$tab)^2), sum, MARGIN = 1)) == 0)/Nv
            }
            if ("threshold" %in% algo) {
              RMSE_ErrorRates$ERcTglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum((apply(((ClassPredYc.threshold[[j]][, 
                                                                                                                     Var == k] - Yc$tab[, Var == k])^2), sum, 
                                                                                        MARGIN = 1)) == 0)/Nc))
              RMSE_ErrorRates$ERvTglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                            function(k) (1 - sum((apply(((ClassPredYv.threshold[[j]][, 
                                                                                                                     Var == k] - Yv$tab[, Var == k])^2), sum, 
                                                                                        MARGIN = 1)) == 0)/Nv))
              RMSE_ErrorRates$ERcTglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYc.threshold[[j]] - 
                               Yc$tab)^2), sum, MARGIN = 1)) == 0)/Nc
              RMSE_ErrorRates$ERvTglobal[j, "global"] <- 1 - 
                sum((apply(((ClassPredYv.threshold[[j]] - 
                               Yv$tab)^2), sum, MARGIN = 1)) == 0)/Nv
            }
          }
          if ((nNoBin == 0) & ("AUC" %in% outputs)) {
            RMSE_ErrorRates$AUCcglobal[j, 1:Ky] <- RMSE_ErrorRates$AUCc[j, 
                                                                        seq(from = 1, to = q, by = 2)]
            RMSE_ErrorRates$AUCvglobal[j, 1:Ky] <- RMSE_ErrorRates$AUCv[j, 
                                                                        seq(from = 1, to = q, by = 2)]
            RMSE_ErrorRates$AUCcglobal[j, "Mean"] <- mean(RMSE_ErrorRates$AUCc[j, 
                                                                               seq(from = 1, to = q, by = 2)], na.rm = TRUE)
            RMSE_ErrorRates$AUCvglobal[j, "Mean"] <- mean(RMSE_ErrorRates$AUCv[j, 
                                                                               seq(from = 1, to = q, by = 2)], na.rm = TRUE)
          }
        }
        RMSE_ErrorRates
      }
    stopCluster(cl)
    on.exit(stopCluster)
    nrepetFE <- length(resForeach)
    if ((nrepetFE < 1.5) | (is.null(nrepetFE) == TRUE)) {
      stop("No adjustement of models. Try with less components")
    }
    res <- NULL
    res$TRUEnrepet <- nrepetFE
    dimlab <- paste("Ax", (1:h), sep = "")
    if ("RMSE" %in% outputs) {
      RMSECglobalm <- RMSEVglobalm <- matrix(NA, nrow = nrepetFE, 
                                             ncol = h)
      colnames(RMSECglobalm) <- colnames(RMSEVglobalm) <- dimlab
      rownames(RMSECglobalm) <- rownames(RMSEVglobalm) <- 1:nrepetFE
    }
    if ("max" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcM <- TPvM <- TNcM <- TNvM <- FPcM <- FPvM <- FNcM <- FNvM <- list()
        TPcMall <- TPvMall <- TNcMall <- TNvMall <- FPcMall <- FPvMall <- FNcMall <- FNvMall <- matrix(NA, 
                                                                                                       nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                      paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                                 nrepetFE)))))
      }
      if ("ER" %in% outputs) {
        ERcM <- ERvM <- ERcM.global <- ERvM.global <- list()
        ERcMall <- ERvMall <- matrix(NA, nrow = h, ncol = q * 
                                       nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                                "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
        ERcMglobalm <- ERvMglobalm <- matrix(NA, nrow = h, 
                                             ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                         paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                                rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
      }
    }
    if ("gravity" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcG <- TPvG <- TNcG <- TNvG <- FPcG <- FPvG <- FNcG <- FNvG <- list()
        TPcGall <- TPvGall <- TNcGall <- TNvGall <- FPcGall <- FPvGall <- FNcGall <- FNvGall <- matrix(NA, 
                                                                                                       nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                      paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                                 nrepetFE)))))
      }
      if ("ER" %in% outputs) {
        ERcG <- ERvG <- ERcG.global <- ERvG.global <- list()
        ERcGall <- ERvGall <- matrix(NA, nrow = h, ncol = q * 
                                       nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                                "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
        ERcGglobalm <- ERvGglobalm <- matrix(NA, nrow = h, 
                                             ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                         paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                                rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
      }
    }
    if ("threshold" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcT <- TPvT <- TNcT <- TNvT <- FPcT <- FPvT <- FNcT <- FNvT <- list()
        TPcTall <- TPvTall <- TNcTall <- TNvTall <- FPcTall <- FPvTall <- FNcTall <- FNvTall <- matrix(NA, 
                                                                                                       nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                      paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                                 nrepetFE)))))
      }
      if ("ER" %in% outputs) {
        ERcT <- ERvT <- ERcT.global <- ERvT.global <- list()
        ERcTall <- ERvTall <- matrix(NA, nrow = h, ncol = q * 
                                       nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                                "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
        ERcTglobalm <- ERvTglobalm <- matrix(NA, nrow = h, 
                                             ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                         paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                                rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      AUCc <- AUCv <- AUCc.global <- AUCv.global <- list(NULL)
      AUCcall <- AUCvall <- matrix(NA, nrow = h, ncol = q * 
                                     nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                              "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
      AUCcglobalm <- AUCvglobalm <- matrix(NA, nrow = h, ncol = (Ky + 
                                                                   1) * nrepetFE, dimnames = list(dimlab, paste0(c(paste0("Var", 
                                                                                                                          1:Ky), "Mean"), "_rep", rep(1:nrepetFE, rep((Ky + 
                                                                                                                                                                         1), nrepetFE)))))
    }
    IC95 <- function(m) {
      rt <- c(rep(NA, 2))
      if ((sd(m, na.rm = TRUE) != 0) & (is.na(sd(m, na.rm = TRUE)) == 
                                        FALSE)) {
        testt <- t.test(m, conf.level = 0.95)
        rt <- c(round(testt$conf.int[1], 5), round(testt$conf.int[2], 
                                                   5))
      }
      return(rt)
    }
    stat.desc <- function(x) {
      nombre <- colSums(!is.na(x))
      moy <- round(colMeans(x, na.rm = TRUE), 5)
      etype <- round(apply(x, 2, sd, na.rm = TRUE), 5)
      quartiles <- round(t(apply(x, 2, quantile, probs = c(0.025, 
                                                           0.5, 0.975), na.rm = TRUE)), 5)
      IC <- t(apply(x, 2, IC95))
      result <- cbind.data.frame(nombre, moy, etype, IC, quartiles, 
                                 stringsAsFactors = TRUE)
      colnames(result) <- c("nb", "mean", "sd", "95CIinf", 
                            "95CIsup", "Q2.5", "median", "Q97.5")
      rownames(result) <- colnames(x)
      return(result)
    }
    if ("RMSE" %in% outputs) {
      for (i in 1:nrepetFE) {
        RMSECglobalm[i, 1:(dim(resForeach[[i]][["RMSEglobal"]])[1])] <- resForeach[[i]][["RMSEglobal"]][, 
                                                                                                        "RMSEC"]
        RMSEVglobalm[i, 1:(dim(resForeach[[i]][["RMSEglobal"]])[1])] <- resForeach[[i]][["RMSEglobal"]][, 
                                                                                                        "RMSEV"]
      }
      res$RMSEc.global <- data.frame(dimlab, stat.desc(RMSECglobalm), 
                                     stringsAsFactors = TRUE)
      res$RMSEv.global <- data.frame(dimlab, stat.desc(RMSEVglobalm), 
                                     stringsAsFactors = TRUE)
    }
    repetitions <- as.factor(rep(1:nrepetFE, rep(q, nrepetFE)))
    levels(repetitions)
    for (i in 1:nrepetFE) {
      if ("max" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcMall[1:(dim(resForeach[[i]][["TPcM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPcM"]])
          TPvMall[1:(dim(resForeach[[i]][["TPvM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPvM"]])
          TNcMall[1:(dim(resForeach[[i]][["TNcM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNcM"]])
          TNvMall[1:(dim(resForeach[[i]][["TNvM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNvM"]])
          FPcMall[1:(dim(resForeach[[i]][["FPcM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPcM"]])
          FPvMall[1:(dim(resForeach[[i]][["FPvM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPvM"]])
          FNcMall[1:(dim(resForeach[[i]][["FNcM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNcM"]])
          FNvMall[1:(dim(resForeach[[i]][["FNvM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNvM"]])
        }
        if ("ER" %in% outputs) {
          ERcMall[1:(dim(resForeach[[i]][["ERcM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERcM"]])
          ERvMall[1:(dim(resForeach[[i]][["ERvM"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERvM"]])
        }
      }
      if ("gravity" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcGall[1:(dim(resForeach[[i]][["TPcG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPcG"]])
          TPvGall[1:(dim(resForeach[[i]][["TPvG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPvG"]])
          TNcGall[1:(dim(resForeach[[i]][["TNcG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNcG"]])
          TNvGall[1:(dim(resForeach[[i]][["TNvG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNvG"]])
          FPcGall[1:(dim(resForeach[[i]][["FPcG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPcG"]])
          FPvGall[1:(dim(resForeach[[i]][["FPvG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPvG"]])
          FNcGall[1:(dim(resForeach[[i]][["FNcG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNcG"]])
          FNvGall[1:(dim(resForeach[[i]][["FNvG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNvG"]])
        }
        if ("ER" %in% outputs) {
          ERcGall[1:(dim(resForeach[[i]][["ERcG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERcG"]])
          ERvGall[1:(dim(resForeach[[i]][["ERvG"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERvG"]])
        }
      }
      if ("threshold" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcTall[1:(dim(resForeach[[i]][["TPcT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPcT"]])
          TPvTall[1:(dim(resForeach[[i]][["TPvT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TPvT"]])
          TNcTall[1:(dim(resForeach[[i]][["TNcT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNcT"]])
          TNvTall[1:(dim(resForeach[[i]][["TNvT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["TNvT"]])
          FPcTall[1:(dim(resForeach[[i]][["FPcT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPcT"]])
          FPvTall[1:(dim(resForeach[[i]][["FPvT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FPvT"]])
          FNcTall[1:(dim(resForeach[[i]][["FNcT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNcT"]])
          FNvTall[1:(dim(resForeach[[i]][["FNvT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["FNvT"]])
        }
        if ("ER" %in% outputs) {
          ERcTall[1:(dim(resForeach[[i]][["ERcT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERcT"]])
          ERvTall[1:(dim(resForeach[[i]][["ERvT"]])[1]), 
                  repetitions == i] <- (resForeach[[i]][["ERvT"]])
        }
      }
      if ((nNoBin == 0) & ("AUC" %in% outputs)) {
        AUCcall[1:(dim(resForeach[[i]][["AUCc"]])[1]), repetitions == 
                  i] <- (resForeach[[i]][["AUCc"]])
        AUCvall[1:(dim(resForeach[[i]][["AUCv"]])[1]), repetitions == 
                  i] <- (resForeach[[i]][["AUCv"]])
      }
    }
    for (l in 1:q) {
      index <- seq(from = l, to = q * nrepetFE, by = q)
      if ("max" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPcMall[, index])))
          TPvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPvMall[, index])))
          TNcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNcMall[, index])))
          TNvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNvMall[, index])))
          FPcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPcMall[, index])))
          FPvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPvMall[, index])))
          FNcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNcMall[, index])))
          FNvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNvMall[, index])))
        }
        if ("ER" %in% outputs) {
          ERcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERcMall[, index])))
          ERvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERvMall[, index])))
        }
      }
      if ("gravity" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPcGall[, index])))
          TPvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPvGall[, index])))
          TNcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNcGall[, index])))
          TNvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNvGall[, index])))
          FPcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPcGall[, index])))
          FPvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPvGall[, index])))
          FNcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNcGall[, index])))
          FNvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNvGall[, index])))
        }
        if ("ER" %in% outputs) {
          ERcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERcGall[, index])))
          ERvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERvGall[, index])))
        }
      }
      if ("threshold" %in% algo) {
        if ("ConfMat" %in% outputs) {
          TPcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPcTall[, index])))
          TPvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TPvTall[, index])))
          TNcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNcTall[, index])))
          TNvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(TNvTall[, index])))
          FPcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPcTall[, index])))
          FPvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FPvTall[, index])))
          FNcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNcTall[, index])))
          FNvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(FNvTall[, index])))
        }
        if ("ER" %in% outputs) {
          ERcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERcTall[, index])))
          ERvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                             dimlab, stat.desc(t(ERvTall[, index])))
        }
      }
      if ((nNoBin == 0) & ("AUC" %in% outputs)) {
        AUCc[[l]] <- cbind(cname = rep(cnames[l], h), dimlab, 
                           stat.desc(t(AUCcall[, index])))
        AUCv[[l]] <- cbind(cname = rep(cnames[l], h), dimlab, 
                           stat.desc(t(AUCvall[, index])))
      }
    }
    repetitionsGlobal <- as.factor(rep(1:nrepetFE, rep((Ky + 
                                                          1), nrepetFE)))
    levels(repetitionsGlobal)
    for (i in 1:nrepetFE) {
      if ("ER" %in% outputs) {
        if ("max" %in% algo) {
          ERcMglobalm[1:(dim(resForeach[[i]][["ERcMglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERcMglobal"]])
          ERvMglobalm[1:(dim(resForeach[[i]][["ERvMglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERvMglobal"]])
        }
        if ("gravity" %in% algo) {
          ERcGglobalm[1:(dim(resForeach[[i]][["ERcGglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERcGglobal"]])
          ERvGglobalm[1:(dim(resForeach[[i]][["ERvGglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERvGglobal"]])
        }
        if ("threshold" %in% algo) {
          ERcTglobalm[1:(dim(resForeach[[i]][["ERcTglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERcTglobal"]])
          ERvTglobalm[1:(dim(resForeach[[i]][["ERvTglobal"]])[1]), 
                      repetitionsGlobal == i] <- (resForeach[[i]][["ERvTglobal"]])
        }
      }
      if ((nNoBin == 0) & ("AUC" %in% outputs)) {
        AUCcglobalm[1:(dim(resForeach[[i]][["AUCcglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["AUCcglobal"]])
        AUCvglobalm[1:(dim(resForeach[[i]][["AUCvglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["AUCvglobal"]])
      }
    }
    for (l in 1:(Ky + 1)) {
      indexG <- seq(from = l, to = (Ky + 1) * nrepetFE, by = (Ky + 
                                                                1))
      if ("ER" %in% outputs) {
        if ("max" %in% algo) {
          ERcM.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERcMglobalm[, 
                                                                                                             indexG])))
          ERvM.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERvMglobalm[, 
                                                                                                             indexG])))
        }
        if ("gravity" %in% algo) {
          ERcG.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERcGglobalm[, 
                                                                                                             indexG])))
          ERvG.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERvGglobalm[, 
                                                                                                             indexG])))
        }
        if ("threshold" %in% algo) {
          ERcT.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERcTglobalm[, 
                                                                                                             indexG])))
          ERvT.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                        1:Ky), "global")[l], dimlab, stat.desc(t(ERvTglobalm[, 
                                                                                                             indexG])))
        }
      }
      if ((nNoBin == 0) & ("AUC" %in% outputs)) {
        AUCc.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "Mean")[l], dimlab, stat.desc(t(AUCcglobalm[, 
                                                                                                         indexG])))
        AUCv.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "Mean")[l], dimlab, stat.desc(t(AUCvglobalm[, 
                                                                                                         indexG])))
      }
    }
    if ("max" %in% algo) {
      if ("ConfMat" %in% outputs) {
        res$TruePosC.max <- do.call("rbind", TPcM)
        res$TruePosV.max <- do.call("rbind", TPvM)
        res$TrueNegC.max <- do.call("rbind", TNcM)
        res$TrueNegV.max <- do.call("rbind", TNvM)
        res$FalsePosC.max <- do.call("rbind", FPcM)
        res$FalsePosV.max <- do.call("rbind", FPvM)
        res$FalseNegC.max <- do.call("rbind", FNcM)
        res$FalseNegV.max <- do.call("rbind", FNvM)
      }
      if ("ER" %in% outputs) {
        res$ErrorRateC.max <- do.call("rbind", ERcM)
        res$ErrorRateV.max <- do.call("rbind", ERvM)
        res$ErrorRateCglobal.max <- do.call("rbind", ERcM.global)
        res$ErrorRateVglobal.max <- do.call("rbind", ERvM.global)
      }
    }
    if ("gravity" %in% algo) {
      if ("ConfMat" %in% outputs) {
        res$TruePosC.gravity <- do.call("rbind", TPcG)
        res$TruePosV.gravity <- do.call("rbind", TPvG)
        res$TrueNegC.gravity <- do.call("rbind", TNcG)
        res$TrueNegV.gravity <- do.call("rbind", TNvG)
        res$FalsePosC.gravity <- do.call("rbind", FPcG)
        res$FalsePosV.gravity <- do.call("rbind", FPvG)
        res$FalseNegC.gravity <- do.call("rbind", FNcG)
        res$FalseNegV.gravity <- do.call("rbind", FNvG)
      }
      if ("ER" %in% outputs) {
        res$ErrorRateC.gravity <- do.call("rbind", ERcG)
        res$ErrorRateV.gravity <- do.call("rbind", ERvG)
        res$ErrorRateCglobal.gravity <- do.call("rbind", 
                                                ERcG.global)
        res$ErrorRateVglobal.gravity <- do.call("rbind", 
                                                ERvG.global)
      }
    }
    if ("threshold" %in% algo) {
      if ("ConfMat" %in% outputs) {
        res$TruePosC.threshold <- do.call("rbind", TPcT)
        res$TruePosV.threshold <- do.call("rbind", TPvT)
        res$TrueNegC.threshold <- do.call("rbind", TNcT)
        res$TrueNegV.threshold <- do.call("rbind", TNvT)
        res$FalsePosC.threshold <- do.call("rbind", FPcT)
        res$FalsePosV.threshold <- do.call("rbind", FPvT)
        res$FalseNegC.threshold <- do.call("rbind", FNcT)
        res$FalseNegV.threshold <- do.call("rbind", FNvT)
      }
      if ("ER" %in% outputs) {
        res$ErrorRateC.threshold <- do.call("rbind", ERcT)
        res$ErrorRateV.threshold <- do.call("rbind", ERvT)
        res$ErrorRateCglobal.threshold <- do.call("rbind", 
                                                  ERcT.global)
        res$ErrorRateVglobal.threshold <- do.call("rbind", 
                                                  ERvT.global)
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      res$AUCc <- do.call("rbind", AUCc)
      res$AUCv <- do.call("rbind", AUCv)
      res$AUCc.global <- do.call("rbind", AUCc.global)
      res$AUCv.global <- do.call("rbind", AUCv.global)
    }
    if ("RMSE" %in% outputs) {
      rownames(res$RMSEc.global) <- rownames(res$RMSEv.global) <- NULL
    }
    if ("max" %in% algo) {
      if ("ConfMat" %in% outputs) {
        rownames(res$TruePosC.max) <- rownames(res$TruePosV.max) <- rownames(res$TrueNegC.max) <- rownames(res$TrueNegV.max) <- rownames(res$FalsePosC.max) <- rownames(res$FalsePosV.max) <- rownames(res$FalseNegC.max) <- rownames(res$FalseNegV.max) <- NULL
      }
      if ("ER" %in% outputs) {
        rownames(res$ErrorRateC.max) <- rownames(res$ErrorRateV.max) <- rownames(res$ErrorRateCglobal.max) <- rownames(res$ErrorRateVglobal.max) <- NULL
      }
    }
    if ("gravity" %in% algo) {
      if ("ConfMat" %in% outputs) {
        rownames(res$TruePosC.gravity) <- rownames(res$TruePosV.gravity) <- rownames(res$TrueNegC.gravity) <- rownames(res$TrueNegV.gravity) <- rownames(res$FalsePosC.gravity) <- rownames(res$FalsePosV.gravity) <- rownames(res$FalseNegC.gravity) <- rownames(res$FalseNegV.gravity) <- NULL
      }
      if ("ER" %in% outputs) {
        rownames(res$ErrorRateC.gravity) <- rownames(res$ErrorRateV.gravity) <- rownames(res$ErrorRateCglobal.gravity) <- rownames(res$ErrorRateVglobal.gravity) <- NULL
      }
    }
    if ("threshold" %in% algo) {
      if ("ConfMat" %in% outputs) {
        rownames(res$TruePosC.threshold) <- rownames(res$TruePosV.threshold) <- rownames(res$TrueNegC.threshold) <- rownames(res$TrueNegV.threshold) <- rownames(res$FalsePosC.threshold) <- rownames(res$FalsePosV.threshold) <- rownames(res$FalseNegC.threshold) <- rownames(res$FalseNegV.threshold) <- NULL
      }
      if ("ER" %in% outputs) {
        rownames(res$ErrorRateC.threshold) <- rownames(res$ErrorRateV.threshold) <- rownames(res$ErrorRateCglobal.threshold) <- rownames(res$ErrorRateVglobal.threshold) <- NULL
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      rownames(res$AUCc) <- rownames(res$AUCv) <- rownames(res$AUCc.global) <- rownames(res$AUCv.global) <- NULL
    }
    res$call <- match.call()
    class(res) <- c("testdim_mbplsda")
    return(res)
  }

cv_mbplsda_original <- 
  function (object, nrepet = 100, algo = c("max", "gravity", "threshold"), 
          threshold = 0.5, bloY, outputs = c("ER", "ConfMat", "AUC"), 
          cpus = 1) 
{
  if (!inherits(object, "mbplsda")) 
    stop("Object of type 'mbplsda' expected")
  appel <- as.list(object$call)
  method <- as.character(appel[[1]])
  scale <- eval.parent(appel$scale)
  option <- eval.parent(appel$option)
  if (inherits(try(eval.parent(appel$ktabX), silent = TRUE), 
               "try-error") == TRUE) {
    stop("ktabX must be in the Global Environment")
  }
  X <- eval.parent(appel$ktabX)
  if (inherits(try(eval.parent(appel$dudiY), silent = TRUE)[1], 
               "try-error") == TRUE) {
    stop("dudiY must be in the Global Environment")
  }
  Y <- eval.parent(appel$dudiY)
  nr <- nrow(Y$tab)
  q <- ncol(Y$tab)
  h <- object$rank
  Nc <- round(2 * nr/3)
  Nv <- nr - Nc
  Ky <- length(bloY)
  Var <- as.factor(rep(1:Ky, bloY))
  cnames <- colnames(Y$tab)
  nblo <- length(X$blo)
  blo <- sapply(1:nblo, function(k) dim(X[[k]])[2])
  Bloc <- as.factor(rep(1:nblo, blo))
  nNoBin <- sum(bloY != 2)
  cl <- makeCluster(cpus, type = "PSOCK")
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  resForeach <- foreach(i = 1:nrepet, .export = c("mbplsda", 
                                                  "inertie", "ginv"), .packages = c("ade4", "pROC"), .errorhandling = "remove") %dopar% 
    {
      set.seed(seed = i)
      s <- sample(x = nr, size = Nc)
      Xc <- X[, s, ]
      Xv <- X[, -s, ]
      Yc <- Y[s, ]
      Yv <- Y[-s, ]
      rnamesXc <- row.names(Xc)
      rnamesXv <- row.names(Xv)
      rnamesYc <- row.names(Yc$tab)
      rnamesYv <- row.names(Yv$tab)
      nbY1c <- sapply(1:q, function(g) sum(Yc$tab[, g] == 
                                             1))
      nbY1v <- sapply(1:q, function(g) sum(Yv$tab[, g] == 
                                             1))
      rescal <- do.call(method, list(dudiY = Yc, ktabX = Xc, 
                                     scale = scale, option = option, scannf = FALSE, 
                                     nf = h))
      resval <- do.call(method, list(dudiY = Yv, ktabX = Xv, 
                                     scale = scale, option = option, scannf = FALSE, 
                                     nf = h))
      H = min(rescal$rank, resval$rank, h)
      Xc.mat <- cbind.data.frame(unclass(Xc)[1:nblo], stringsAsFactors = TRUE)
      rescal$meanX <- colMeans(Xc.mat)
      rescal$sdX <- apply(Xc.mat, 2, sd) * sqrt((Nc - 1)/Nc)
      Xv.raw <- cbind.data.frame(unclass(Xv)[1:nblo], stringsAsFactors = TRUE)
      Xv.c <- sweep(Xv.raw, 2, rescal$meanX, FUN = "-")
      if (scale == TRUE) {
        Xv.cr <- sweep(Xv.c, 2, rescal$sdX, FUN = "/")
        if (option == "uniform") {
          Xv.crw <- Xv.cr * sqrt(matrix(rep(rescal$X.cw, 
                                            each = Nv), nrow = Nv))
        }
        else {
          Xv.crw <- Xv.cr
        }
      }
      if (scale == FALSE) {
        Xv.cr <- Xv.c
        if (option == "uniform") {
          Xv.crw <- Xv.cr * sqrt(matrix(rep(rescal$X.cw, 
                                            each = Nv), nrow = Nv))
        }
        else {
          Xv.crw <- Xv.cr
        }
      }
      RMSE_ErrorRates <- list(NULL)
      if ("RMSE" %in% outputs) {
        RMSE_ErrorRates$RMSEglobal <- matrix(NA, nrow = H, 
                                             ncol = 2, dimnames = list(1:H, c("RMSEC", "RMSEV")))
      }
      classYc <- list(NULL)
      classYv <- list(NULL)
      if ("max" %in% algo) {
        ClassPredYc.max <- list(NULL)
        ClassPredYv.max <- list(NULL)
        if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
          RMSE_ErrorRates$TPcM <- RMSE_ErrorRates$TPvM <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
        }
        if ("ConfMat" %in% outputs) {
          RMSE_ErrorRates$TNcM <- RMSE_ErrorRates$TNvM <- RMSE_ErrorRates$FPcM <- RMSE_ErrorRates$FPvM <- RMSE_ErrorRates$FNcM <- RMSE_ErrorRates$FNvM <- matrix(NA, 
                                                                                                                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                     cnames))
        }
        if ("ER" %in% outputs) {
          RMSE_ErrorRates$ERcM <- RMSE_ErrorRates$ERvM <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
          RMSE_ErrorRates$ERcMglobal <- RMSE_ErrorRates$ERvMglobal <- matrix(NA, 
                                                                             ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                        c(paste0("Var", 1:Ky), "global")))
        }
      }
      if ("gravity" %in% algo) {
        ClassPredYc.gravity <- list(NULL)
        ClassPredYv.gravity <- list(NULL)
        if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
          RMSE_ErrorRates$TPcG <- RMSE_ErrorRates$TPvG <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
        }
        if ("ConfMat" %in% outputs) {
          RMSE_ErrorRates$TNcG <- RMSE_ErrorRates$TNvG <- RMSE_ErrorRates$FPcG <- RMSE_ErrorRates$FPvG <- RMSE_ErrorRates$FNcG <- RMSE_ErrorRates$FNvG <- matrix(NA, 
                                                                                                                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                     cnames))
        }
        if ("ER" %in% outputs) {
          RMSE_ErrorRates$ERcG <- RMSE_ErrorRates$ERvG <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
          RMSE_ErrorRates$ERcGglobal <- RMSE_ErrorRates$ERvGglobal <- matrix(NA, 
                                                                             ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                        c(paste0("Var", 1:Ky), "global")))
        }
      }
      if ("threshold" %in% algo) {
        ClassPredYc.threshold <- list(NULL)
        ClassPredYv.threshold <- list(NULL)
        if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
          RMSE_ErrorRates$TPcT <- RMSE_ErrorRates$TPvT <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
        }
        if ("ConfMat" %in% outputs) {
          RMSE_ErrorRates$TNcT <- RMSE_ErrorRates$TNvT <- RMSE_ErrorRates$FPcT <- RMSE_ErrorRates$FPvT <- RMSE_ErrorRates$FNcT <- RMSE_ErrorRates$FNvT <- matrix(NA, 
                                                                                                                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                                                                                                                     cnames))
        }
        if ("ER" %in% outputs) {
          RMSE_ErrorRates$ERcT <- RMSE_ErrorRates$ERvT <- matrix(NA, 
                                                                 ncol = q, nrow = H, dimnames = list(1:H, 
                                                                                                     cnames))
          RMSE_ErrorRates$ERcTglobal <- RMSE_ErrorRates$ERvTglobal <- matrix(NA, 
                                                                             ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                        c(paste0("Var", 1:Ky), "global")))
        }
      }
      if ((nNoBin == 0) & ("AUC" %in% outputs)) {
        RMSE_ErrorRates$AUCc <- matrix(NA, ncol = q, 
                                       nrow = H, dimnames = list(1:H, cnames))
        RMSE_ErrorRates$AUCv <- matrix(NA, ncol = q, 
                                       nrow = H, dimnames = list(1:H, cnames))
        RMSE_ErrorRates$AUCcglobal <- RMSE_ErrorRates$AUCvglobal <- matrix(NA, 
                                                                           ncol = (Ky + 1), nrow = H, dimnames = list(1:H, 
                                                                                                                      c(paste0("Var", 1:Ky), "Mean")))
      }
      for (j in 1:H) {
        classYc[[j]] <- matrix(NA, Nc, ncol = 3 * Ky, 
                               dimnames = list(rnamesYc, c(paste0("Ymax_Var", 
                                                                  1:Ky), paste0("Ycentroid_Var", 1:Ky), paste0("Ythreshold_Var", 
                                                                                                               1:Ky))))
        classYv[[j]] <- matrix(NA, Nv, ncol = 3 * Ky, 
                               dimnames = list(rnamesYv, c(paste0("Ymax_Var", 
                                                                  1:Ky), paste0("Ycentroid_Var", 1:Ky), paste0("Ythreshold_Var", 
                                                                                                               1:Ky))))
        if ("max" %in% algo) {
          ClassPredYc.max[[j]] <- matrix(0, Nc, ncol = q, 
                                         dimnames = list(rnamesYc, cnames))
          ClassPredYv.max[[j]] <- matrix(0, Nv, ncol = q, 
                                         dimnames = list(rnamesYv, cnames))
        }
        if ("gravity" %in% algo) {
          ClassPredYc.gravity[[j]] <- matrix(0, Nc, ncol = q, 
                                             dimnames = list(rnamesYc, cnames))
          ClassPredYv.gravity[[j]] <- matrix(0, Nv, ncol = q, 
                                             dimnames = list(rnamesYv, cnames))
        }
        if ("threshold" %in% algo) {
          ClassPredYc.threshold[[j]] <- matrix(NA, Nc, 
                                               ncol = q, dimnames = list(rnamesYc, cnames))
          ClassPredYv.threshold[[j]] <- matrix(NA, Nv, 
                                               ncol = q, dimnames = list(rnamesYv, cnames))
        }
        XYcoef.raw.cal <- sapply(rescal$XYcoef.raw, function(x) x[, 
                                                                  j])
        intercept.raw.cal <- sapply(rescal$intercept.raw, 
                                    function(x) x[, j])
        predYc <- matrix(rep(intercept.raw.cal, each = Nc), 
                         ncol = q) + as.matrix(Xc.mat) %*% XYcoef.raw.cal
        predYv <- matrix(rep(intercept.raw.cal, each = Nv), 
                         ncol = q) + as.matrix(Xv.raw) %*% XYcoef.raw.cal
        colnames(predYc) <- colnames(predYv) <- cnames
        if ("RMSE" %in% outputs) {
          residYc <- as.matrix(Yc$tab) - predYc
          RMSE_ErrorRates$RMSEglobal[j, "RMSEC"] <- sqrt(sum(residYc^2)/(Nc * 
                                                                           q))
          residYv <- as.matrix(Yv$tab) - predYv
          RMSE_ErrorRates$RMSEglobal[j, "RMSEV"] <- sqrt(sum(residYv^2)/(Nv * 
                                                                           q))
        }
        if ("max" %in% algo) {
          for (k in 1:Ky) {
            classYc[[j]][, k] <- sapply(1:Nc, function(n) which.max(predYc[n, 
                                                                           Var == k]))
            classYv[[j]][, k] <- sapply(1:Nv, function(n) which.max(predYv[n, 
                                                                           Var == k]))
            for (n in 1:Nc) {
              if (k == 1) 
                (ClassPredYc.max[[j]][n, classYc[[j]][n, 
                                                      1]] <- 1)
              if (k > 1) 
                (ClassPredYc.max[[j]][n, (sum(bloY[1:(k - 
                                                        1)]) + classYc[[j]][n, k])] <- 1)
            }
            for (n in 1:Nv) {
              if (k == 1) 
                (ClassPredYv.max[[j]][n, classYv[[j]][n, 
                                                      1]] <- 1)
              if (k > 1) 
                (ClassPredYv.max[[j]][n, (sum(bloY[1:(k - 
                                                        1)]) + classYv[[j]][n, k])] <- 1)
            }
          }
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                              l] == 1 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                              l] == 0 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FPcM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                              l] == 1 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FNcM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.max[[j]][, 
                                                                                              l] == 0 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcM[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYc.max[[j]][, 
                                                                                               l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.max[[j]][, 
                                                                                                                                                                l] == 0 & Yc$tab[, l] == 1)))/Nc)
          }
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPvM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                              l] == 1 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNvM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                              l] == 0 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FPvM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                              l] == 1 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FNvM[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.max[[j]][, 
                                                                                              l] == 0 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERvM[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYv.max[[j]][, 
                                                                                               l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.max[[j]][, 
                                                                                                                                                                l] == 0 & Yv$tab[, l] == 1)))/Nv)
          }
        }
        if ("gravity" %in% algo) {
          Gravity <- matrix(0, nrow = q, ncol = j, dimnames = list(cnames, 
                                                                   1:j))
          if (j == 1) {
            Gravity[, 1] <- (sapply(1:q, function(g) {
              if (nbY1c[g] > 1) 
                (mean(rescal$lX[Yc$tab[, g] == 1, 1]))
              else if (nbY1c[g] == 1) 
                (rescal$lX[Yc$tab[, g] == 1, 1])
              else if (nbY1c[g] == 0) 
                (NA)
            }))
          }
          else {
            Gravity[, 1:j] <- (t(sapply(1:q, function(g) {
              if (nbY1c[g] > 1) 
                (apply(rescal$lX[Yc$tab[, g] == 1, 1:j], 
                       2, mean))
              else if (nbY1c[g] == 1) 
                (rescal$lX[Yc$tab[, g] == 1, 1:j])
              else if (nbY1c[g] == 0) 
                (rep(NA, times = j))
            })))
          }
          dist.eucl.gravity.Yc <- sapply(1:q, function(g) apply((rescal$lX[, 
                                                                           1:j] - matrix(rep(Gravity[g, ], each = Nc), 
                                                                                         nrow = Nc))^2, 1, sum))
          dist.eucl.gravity.Yv <- sapply(1:q, function(g) apply(((as.matrix(Xv.crw) %*% 
                                                                    rescal$faX[, 1:j]) - matrix(rep(Gravity[g, 
                                                                    ], each = Nv), nrow = Nv))^2, 1, sum))
          for (k in 1:Ky) {
            classYc[[j]][, (k + Ky)] <- sapply(1:Nc, 
                                               function(n) which.min(dist.eucl.gravity.Yc[n, 
                                                                                          Var == k]))
            classYv[[j]][, (k + Ky)] <- sapply(1:Nv, 
                                               function(n) which.min(dist.eucl.gravity.Yv[n, 
                                                                                          Var == k]))
            for (n in 1:Nc) {
              if (k == 1) 
                (ClassPredYc.gravity[[j]][n, classYc[[j]][n, 
                                                          (1 + Ky)]] <- 1)
              if (k > 1) 
                (ClassPredYc.gravity[[j]][n, (sum(bloY[1:(k - 
                                                            1)]) + classYc[[j]][n, (k + Ky)])] <- 1)
            }
            for (n in 1:Nv) {
              if (k == 1) 
                (ClassPredYv.gravity[[j]][n, classYv[[j]][n, 
                                                          (1 + Ky)]] <- 1)
              if (k > 1) 
                (ClassPredYv.gravity[[j]][n, (sum(bloY[1:(k - 
                                                            1)]) + classYv[[j]][n, (k + Ky)])] <- 1)
            }
          }
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                  l] == 1 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                  l] == 0 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FPcG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                  l] == 1 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FNcG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.gravity[[j]][, 
                                                                                                  l] == 0 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcG[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYc.gravity[[j]][, 
                                                                                                   l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.gravity[[j]][, 
                                                                                                                                                                        l] == 0 & Yc$tab[, l] == 1)))/Nc)
          }
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPvG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                  l] == 1 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNvG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                  l] == 0 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FPvG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                  l] == 1 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FNvG[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.gravity[[j]][, 
                                                                                                  l] == 0 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERvG[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYv.gravity[[j]][, 
                                                                                                   l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.gravity[[j]][, 
                                                                                                                                                                        l] == 0 & Yv$tab[, l] == 1)))/Nv)
          }
        }
        if ("threshold" %in% algo) {
          ClassPredYc.threshold[[j]][predYc >= threshold] <- 1
          ClassPredYv.threshold[[j]][predYv >= threshold] <- 1
          ClassPredYc.threshold[[j]][predYc < threshold] <- 0
          ClassPredYv.threshold[[j]][predYv < threshold] <- 0
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPcT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                    l] == 1 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNcT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                    l] == 0 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FPcT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                    l] == 1 & Yc$tab[, l] == 0))/Nc)
            RMSE_ErrorRates$FNcT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYc.threshold[[j]][, 
                                                                                                    l] == 0 & Yc$tab[, l] == 1))/Nc)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERcT[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYc.threshold[[j]][, 
                                                                                                     l] == 1 & Yc$tab[, l] == 0)) + length(which(ClassPredYc.threshold[[j]][, 
                                                                                                                                                                            l] == 0 & Yc$tab[, l] == 1)))/Nc)
          }
          if (("ConfMat" %in% outputs) | ("ER" %in% outputs)) {
            RMSE_ErrorRates$TPvT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                    l] == 1 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ConfMat" %in% outputs) {
            RMSE_ErrorRates$TNvT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                    l] == 0 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FPvT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                    l] == 1 & Yv$tab[, l] == 0))/Nv)
            RMSE_ErrorRates$FNvT[j, ] <- sapply(1:q, 
                                                function(l) length(which(ClassPredYv.threshold[[j]][, 
                                                                                                    l] == 0 & Yv$tab[, l] == 1))/Nv)
          }
          if ("ER" %in% outputs) {
            RMSE_ErrorRates$ERvT[j, ] <- sapply(1:q, 
                                                function(l) (length(which(ClassPredYv.threshold[[j]][, 
                                                                                                     l] == 1 & Yv$tab[, l] == 0)) + length(which(ClassPredYv.threshold[[j]][, 
                                                                                                                                                                            l] == 0 & Yv$tab[, l] == 1)))/Nv)
          }
          for (k in 1:Ky) {
            classYc[[j]][, (2 * Ky + k)] <- sapply(1:Nc, 
                                                   function(n) if (sum(ClassPredYc.threshold[[j]][n, 
                                                                                                  Var == k], na.rm = T) == 1) 
                                                     (which.max(ClassPredYc.threshold[[j]][n, 
                                                                                           Var == k])))
            classYv[[j]][, (2 * Ky + k)] <- sapply(1:Nv, 
                                                   function(n) if (sum(ClassPredYv.threshold[[j]][n, 
                                                                                                  Var == k], na.rm = T) == 1) 
                                                     (which.max(ClassPredYv.threshold[[j]][n, 
                                                                                           Var == k])))
          }
        }
        if ((nNoBin == 0) & (sum(sum(nbY1v == 0), sum(nbY1c == 
                                                      0)) == 0) & ("AUC" %in% outputs)) {
          RMSE_ErrorRates$AUCc[j, ] <- sapply(1:q, function(l) auc(Yc$tab[, 
                                                                          l], predYc[, l]))
          RMSE_ErrorRates$AUCv[j, ] <- sapply(1:q, function(l) auc(Yv$tab[, 
                                                                          l], predYv[, l]))
        }
        if ("ER" %in% outputs) {
          if ("max" %in% algo) {
            RMSE_ErrorRates$ERcMglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum(RMSE_ErrorRates$TPcM[j, 
                                                                                                    Var == k])))
            RMSE_ErrorRates$ERvMglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum(RMSE_ErrorRates$TPvM[j, 
                                                                                                    Var == k])))
            RMSE_ErrorRates$ERcMglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYc.max[[j]] - Yc$tab)^2), 
                         sum, MARGIN = 1)) == 0)/Nc
            RMSE_ErrorRates$ERvMglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYv.max[[j]] - Yv$tab)^2), 
                         sum, MARGIN = 1)) == 0)/Nv
          }
          if ("gravity" %in% algo) {
            RMSE_ErrorRates$ERcGglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum(RMSE_ErrorRates$TPcG[j, 
                                                                                                    Var == k])))
            RMSE_ErrorRates$ERvGglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum(RMSE_ErrorRates$TPvG[j, 
                                                                                                    Var == k])))
            RMSE_ErrorRates$ERcGglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYc.gravity[[j]] - 
                             Yc$tab)^2), sum, MARGIN = 1)) == 0)/Nc
            RMSE_ErrorRates$ERvGglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYv.gravity[[j]] - 
                             Yv$tab)^2), sum, MARGIN = 1)) == 0)/Nv
          }
          if ("threshold" %in% algo) {
            RMSE_ErrorRates$ERcTglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum((apply(((ClassPredYc.threshold[[j]][, 
                                                                                                                   Var == k] - Yc$tab[, Var == k])^2), sum, 
                                                                                      MARGIN = 1)) == 0)/Nc))
            RMSE_ErrorRates$ERvTglobal[j, 1:Ky] <- sapply(1:Ky, 
                                                          function(k) (1 - sum((apply(((ClassPredYv.threshold[[j]][, 
                                                                                                                   Var == k] - Yv$tab[, Var == k])^2), sum, 
                                                                                      MARGIN = 1)) == 0)/Nv))
            RMSE_ErrorRates$ERcTglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYc.threshold[[j]] - 
                             Yc$tab)^2), sum, MARGIN = 1)) == 0)/Nc
            RMSE_ErrorRates$ERvTglobal[j, "global"] <- 1 - 
              sum((apply(((ClassPredYv.threshold[[j]] - 
                             Yv$tab)^2), sum, MARGIN = 1)) == 0)/Nv
          }
        }
        if ((nNoBin == 0) & ("AUC" %in% outputs)) {
          RMSE_ErrorRates$AUCcglobal[j, 1:Ky] <- RMSE_ErrorRates$AUCc[j, 
                                                                      seq(from = 1, to = q, by = 2)]
          RMSE_ErrorRates$AUCvglobal[j, 1:Ky] <- RMSE_ErrorRates$AUCv[j, 
                                                                      seq(from = 1, to = q, by = 2)]
          RMSE_ErrorRates$AUCcglobal[j, "Mean"] <- mean(RMSE_ErrorRates$AUCc[j, 
                                                                             seq(from = 1, to = q, by = 2)], na.rm = TRUE)
          RMSE_ErrorRates$AUCvglobal[j, "Mean"] <- mean(RMSE_ErrorRates$AUCv[j, 
                                                                             seq(from = 1, to = q, by = 2)], na.rm = TRUE)
        }
      }
      RMSE_ErrorRates
    }
  stopCluster(cl)
  on.exit(stopCluster)
  nrepetFE <- length(resForeach)
  if ((nrepetFE < 1.5) | (is.null(nrepetFE) == TRUE)) {
    stop("No adjustement of models. Try with less components")
  }
  res <- NULL
  res$TRUEnrepet <- nrepetFE
  dimlab <- paste("Ax", (1:h), sep = "")
  if ("RMSE" %in% outputs) {
    RMSECglobalm <- RMSEVglobalm <- matrix(NA, nrow = nrepetFE, 
                                           ncol = h)
    colnames(RMSECglobalm) <- colnames(RMSEVglobalm) <- dimlab
    rownames(RMSECglobalm) <- rownames(RMSEVglobalm) <- 1:nrepetFE
  }
  if ("max" %in% algo) {
    if ("ConfMat" %in% outputs) {
      TPcM <- TPvM <- TNcM <- TNvM <- FPcM <- FPvM <- FNcM <- FNvM <- list()
      TPcMall <- TPvMall <- TNcMall <- TNvMall <- FPcMall <- FPvMall <- FNcMall <- FNvMall <- matrix(NA, 
                                                                                                     nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                    paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                               nrepetFE)))))
    }
    if ("ER" %in% outputs) {
      ERcM <- ERvM <- ERcM.global <- ERvM.global <- list()
      ERcMall <- ERvMall <- matrix(NA, nrow = h, ncol = q * 
                                     nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                              "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
      ERcMglobalm <- ERvMglobalm <- matrix(NA, nrow = h, 
                                           ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                       paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                              rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
    }
  }
  if ("gravity" %in% algo) {
    if ("ConfMat" %in% outputs) {
      TPcG <- TPvG <- TNcG <- TNvG <- FPcG <- FPvG <- FNcG <- FNvG <- list()
      TPcGall <- TPvGall <- TNcGall <- TNvGall <- FPcGall <- FPvGall <- FNcGall <- FNvGall <- matrix(NA, 
                                                                                                     nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                    paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                               nrepetFE)))))
    }
    if ("ER" %in% outputs) {
      ERcG <- ERvG <- ERcG.global <- ERvG.global <- list()
      ERcGall <- ERvGall <- matrix(NA, nrow = h, ncol = q * 
                                     nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                              "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
      ERcGglobalm <- ERvGglobalm <- matrix(NA, nrow = h, 
                                           ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                       paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                              rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
    }
  }
  if ("threshold" %in% algo) {
    if ("ConfMat" %in% outputs) {
      TPcT <- TPvT <- TNcT <- TNvT <- FPcT <- FPvT <- FNcT <- FNvT <- list()
      TPcTall <- TPvTall <- TNcTall <- TNvTall <- FPcTall <- FPvTall <- FNcTall <- FNvTall <- matrix(NA, 
                                                                                                     nrow = h, ncol = q * nrepetFE, dimnames = list(dimlab, 
                                                                                                                                                    paste0(cnames, "_rep", rep(1:nrepetFE, rep(q, 
                                                                                                                                                                                               nrepetFE)))))
    }
    if ("ER" %in% outputs) {
      ERcT <- ERvT <- ERcT.global <- ERvT.global <- list()
      ERcTall <- ERvTall <- matrix(NA, nrow = h, ncol = q * 
                                     nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                              "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
      ERcTglobalm <- ERvTglobalm <- matrix(NA, nrow = h, 
                                           ncol = (Ky + 1) * nrepetFE, dimnames = list(dimlab, 
                                                                                       paste0(c(paste0("Var", 1:Ky), "global"), "_rep", 
                                                                                              rep(1:nrepetFE, rep((Ky + 1), nrepetFE)))))
    }
  }
  if ((nNoBin == 0) & ("AUC" %in% outputs)) {
    AUCc <- AUCv <- AUCc.global <- AUCv.global <- list(NULL)
    AUCcall <- AUCvall <- matrix(NA, nrow = h, ncol = q * 
                                   nrepetFE, dimnames = list(dimlab, paste0(cnames, 
                                                                            "_rep", rep(1:nrepetFE, rep(q, nrepetFE)))))
    AUCcglobalm <- AUCvglobalm <- matrix(NA, nrow = h, ncol = (Ky + 
                                                                 1) * nrepetFE, dimnames = list(dimlab, paste0(c(paste0("Var", 
                                                                                                                        1:Ky), "Mean"), "_rep", rep(1:nrepetFE, rep((Ky + 
                                                                                                                                                                       1), nrepetFE)))))
  }
  IC95 <- function(m) {
    rt <- c(rep(NA, 2))
    if ((sd(m, na.rm = TRUE) != 0) & (is.na(sd(m, na.rm = TRUE)) == 
                                      FALSE)) {
      testt <- t.test(m, conf.level = 0.95)
      rt <- c(round(testt$conf.int[1], 5), round(testt$conf.int[2], 
                                                 5))
    }
    return(rt)
  }
  stat.desc <- function(x) {
    nombre <- colSums(!is.na(x))
    moy <- round(colMeans(x, na.rm = TRUE), 5)
    etype <- round(apply(x, 2, sd, na.rm = TRUE), 5)
    quartiles <- round(t(apply(x, 2, quantile, probs = c(0.025, 
                                                         0.5, 0.975), na.rm = TRUE)), 5)
    IC <- t(apply(x, 2, IC95))
    result <- cbind.data.frame(nombre, moy, etype, IC, quartiles, 
                               stringsAsFactors = TRUE)
    colnames(result) <- c("nb", "mean", "sd", "95CIinf", 
                          "95CIsup", "Q2.5", "median", "Q97.5")
    rownames(result) <- colnames(x)
    return(result)
  }
  if ("RMSE" %in% outputs) {
    for (i in 1:nrepetFE) {
      RMSECglobalm[i, 1:(dim(resForeach[[i]][["RMSEglobal"]])[1])] <- resForeach[[i]][["RMSEglobal"]][, 
                                                                                                      "RMSEC"]
      RMSEVglobalm[i, 1:(dim(resForeach[[i]][["RMSEglobal"]])[1])] <- resForeach[[i]][["RMSEglobal"]][, 
                                                                                                      "RMSEV"]
    }
    res$RMSEc.global <- data.frame(dimlab, stat.desc(RMSECglobalm), 
                                   stringsAsFactors = TRUE)
    res$RMSEv.global <- data.frame(dimlab, stat.desc(RMSEVglobalm), 
                                   stringsAsFactors = TRUE)
  }
  repetitions <- as.factor(rep(1:nrepetFE, rep(q, nrepetFE)))
  levels(repetitions)
  for (i in 1:nrepetFE) {
    if ("max" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcMall[1:(dim(resForeach[[i]][["TPcM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPcM"]])
        TPvMall[1:(dim(resForeach[[i]][["TPvM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPvM"]])
        TNcMall[1:(dim(resForeach[[i]][["TNcM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNcM"]])
        TNvMall[1:(dim(resForeach[[i]][["TNvM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNvM"]])
        FPcMall[1:(dim(resForeach[[i]][["FPcM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPcM"]])
        FPvMall[1:(dim(resForeach[[i]][["FPvM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPvM"]])
        FNcMall[1:(dim(resForeach[[i]][["FNcM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNcM"]])
        FNvMall[1:(dim(resForeach[[i]][["FNvM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNvM"]])
      }
      if ("ER" %in% outputs) {
        ERcMall[1:(dim(resForeach[[i]][["ERcM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERcM"]])
        ERvMall[1:(dim(resForeach[[i]][["ERvM"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERvM"]])
      }
    }
    if ("gravity" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcGall[1:(dim(resForeach[[i]][["TPcG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPcG"]])
        TPvGall[1:(dim(resForeach[[i]][["TPvG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPvG"]])
        TNcGall[1:(dim(resForeach[[i]][["TNcG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNcG"]])
        TNvGall[1:(dim(resForeach[[i]][["TNvG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNvG"]])
        FPcGall[1:(dim(resForeach[[i]][["FPcG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPcG"]])
        FPvGall[1:(dim(resForeach[[i]][["FPvG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPvG"]])
        FNcGall[1:(dim(resForeach[[i]][["FNcG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNcG"]])
        FNvGall[1:(dim(resForeach[[i]][["FNvG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNvG"]])
      }
      if ("ER" %in% outputs) {
        ERcGall[1:(dim(resForeach[[i]][["ERcG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERcG"]])
        ERvGall[1:(dim(resForeach[[i]][["ERvG"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERvG"]])
      }
    }
    if ("threshold" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcTall[1:(dim(resForeach[[i]][["TPcT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPcT"]])
        TPvTall[1:(dim(resForeach[[i]][["TPvT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TPvT"]])
        TNcTall[1:(dim(resForeach[[i]][["TNcT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNcT"]])
        TNvTall[1:(dim(resForeach[[i]][["TNvT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["TNvT"]])
        FPcTall[1:(dim(resForeach[[i]][["FPcT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPcT"]])
        FPvTall[1:(dim(resForeach[[i]][["FPvT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FPvT"]])
        FNcTall[1:(dim(resForeach[[i]][["FNcT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNcT"]])
        FNvTall[1:(dim(resForeach[[i]][["FNvT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["FNvT"]])
      }
      if ("ER" %in% outputs) {
        ERcTall[1:(dim(resForeach[[i]][["ERcT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERcT"]])
        ERvTall[1:(dim(resForeach[[i]][["ERvT"]])[1]), 
                repetitions == i] <- (resForeach[[i]][["ERvT"]])
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      AUCcall[1:(dim(resForeach[[i]][["AUCc"]])[1]), repetitions == 
                i] <- (resForeach[[i]][["AUCc"]])
      AUCvall[1:(dim(resForeach[[i]][["AUCv"]])[1]), repetitions == 
                i] <- (resForeach[[i]][["AUCv"]])
    }
  }
  for (l in 1:q) {
    index <- seq(from = l, to = q * nrepetFE, by = q)
    if ("max" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPcMall[, index])))
        TPvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPvMall[, index])))
        TNcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNcMall[, index])))
        TNvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNvMall[, index])))
        FPcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPcMall[, index])))
        FPvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPvMall[, index])))
        FNcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNcMall[, index])))
        FNvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNvMall[, index])))
      }
      if ("ER" %in% outputs) {
        ERcM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERcMall[, index])))
        ERvM[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERvMall[, index])))
      }
    }
    if ("gravity" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPcGall[, index])))
        TPvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPvGall[, index])))
        TNcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNcGall[, index])))
        TNvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNvGall[, index])))
        FPcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPcGall[, index])))
        FPvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPvGall[, index])))
        FNcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNcGall[, index])))
        FNvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNvGall[, index])))
      }
      if ("ER" %in% outputs) {
        ERcG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERcGall[, index])))
        ERvG[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERvGall[, index])))
      }
    }
    if ("threshold" %in% algo) {
      if ("ConfMat" %in% outputs) {
        TPcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPcTall[, index])))
        TPvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TPvTall[, index])))
        TNcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNcTall[, index])))
        TNvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(TNvTall[, index])))
        FPcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPcTall[, index])))
        FPvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FPvTall[, index])))
        FNcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNcTall[, index])))
        FNvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(FNvTall[, index])))
      }
      if ("ER" %in% outputs) {
        ERcT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERcTall[, index])))
        ERvT[[l]] <- cbind(cname = rep(cnames[l], h), 
                           dimlab, stat.desc(t(ERvTall[, index])))
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      AUCc[[l]] <- cbind(cname = rep(cnames[l], h), dimlab, 
                         stat.desc(t(AUCcall[, index])))
      AUCv[[l]] <- cbind(cname = rep(cnames[l], h), dimlab, 
                         stat.desc(t(AUCvall[, index])))
    }
  }
  repetitionsGlobal <- as.factor(rep(1:nrepetFE, rep((Ky + 
                                                        1), nrepetFE)))
  levels(repetitionsGlobal)
  for (i in 1:nrepetFE) {
    if ("ER" %in% outputs) {
      if ("max" %in% algo) {
        ERcMglobalm[1:(dim(resForeach[[i]][["ERcMglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERcMglobal"]])
        ERvMglobalm[1:(dim(resForeach[[i]][["ERvMglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERvMglobal"]])
      }
      if ("gravity" %in% algo) {
        ERcGglobalm[1:(dim(resForeach[[i]][["ERcGglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERcGglobal"]])
        ERvGglobalm[1:(dim(resForeach[[i]][["ERvGglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERvGglobal"]])
      }
      if ("threshold" %in% algo) {
        ERcTglobalm[1:(dim(resForeach[[i]][["ERcTglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERcTglobal"]])
        ERvTglobalm[1:(dim(resForeach[[i]][["ERvTglobal"]])[1]), 
                    repetitionsGlobal == i] <- (resForeach[[i]][["ERvTglobal"]])
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      AUCcglobalm[1:(dim(resForeach[[i]][["AUCcglobal"]])[1]), 
                  repetitionsGlobal == i] <- (resForeach[[i]][["AUCcglobal"]])
      AUCvglobalm[1:(dim(resForeach[[i]][["AUCvglobal"]])[1]), 
                  repetitionsGlobal == i] <- (resForeach[[i]][["AUCvglobal"]])
    }
  }
  for (l in 1:(Ky + 1)) {
    indexG <- seq(from = l, to = (Ky + 1) * nrepetFE, by = (Ky + 
                                                              1))
    if ("ER" %in% outputs) {
      if ("max" %in% algo) {
        ERcM.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERcMglobalm[, 
                                                                                                           indexG])))
        ERvM.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERvMglobalm[, 
                                                                                                           indexG])))
      }
      if ("gravity" %in% algo) {
        ERcG.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERcGglobalm[, 
                                                                                                           indexG])))
        ERvG.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERvGglobalm[, 
                                                                                                           indexG])))
      }
      if ("threshold" %in% algo) {
        ERcT.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERcTglobalm[, 
                                                                                                           indexG])))
        ERvT.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                      1:Ky), "global")[l], dimlab, stat.desc(t(ERvTglobalm[, 
                                                                                                           indexG])))
      }
    }
    if ((nNoBin == 0) & ("AUC" %in% outputs)) {
      AUCc.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                    1:Ky), "Mean")[l], dimlab, stat.desc(t(AUCcglobalm[, 
                                                                                                       indexG])))
      AUCv.global[[l]] <- cbind(variable = c(paste0("Var", 
                                                    1:Ky), "Mean")[l], dimlab, stat.desc(t(AUCvglobalm[, 
                                                                                                       indexG])))
    }
  }
  if ("max" %in% algo) {
    if ("ConfMat" %in% outputs) {
      res$TruePosC.max <- do.call("rbind", TPcM)
      res$TruePosV.max <- do.call("rbind", TPvM)
      res$TrueNegC.max <- do.call("rbind", TNcM)
      res$TrueNegV.max <- do.call("rbind", TNvM)
      res$FalsePosC.max <- do.call("rbind", FPcM)
      res$FalsePosV.max <- do.call("rbind", FPvM)
      res$FalseNegC.max <- do.call("rbind", FNcM)
      res$FalseNegV.max <- do.call("rbind", FNvM)
    }
    if ("ER" %in% outputs) {
      res$ErrorRateC.max <- do.call("rbind", ERcM)
      res$ErrorRateV.max <- do.call("rbind", ERvM)
      res$ErrorRateCglobal.max <- do.call("rbind", ERcM.global)
      res$ErrorRateVglobal.max <- do.call("rbind", ERvM.global)
    }
  }
  if ("gravity" %in% algo) {
    if ("ConfMat" %in% outputs) {
      res$TruePosC.gravity <- do.call("rbind", TPcG)
      res$TruePosV.gravity <- do.call("rbind", TPvG)
      res$TrueNegC.gravity <- do.call("rbind", TNcG)
      res$TrueNegV.gravity <- do.call("rbind", TNvG)
      res$FalsePosC.gravity <- do.call("rbind", FPcG)
      res$FalsePosV.gravity <- do.call("rbind", FPvG)
      res$FalseNegC.gravity <- do.call("rbind", FNcG)
      res$FalseNegV.gravity <- do.call("rbind", FNvG)
    }
    if ("ER" %in% outputs) {
      res$ErrorRateC.gravity <- do.call("rbind", ERcG)
      res$ErrorRateV.gravity <- do.call("rbind", ERvG)
      res$ErrorRateCglobal.gravity <- do.call("rbind", 
                                              ERcG.global)
      res$ErrorRateVglobal.gravity <- do.call("rbind", 
                                              ERvG.global)
    }
  }
  if ("threshold" %in% algo) {
    if ("ConfMat" %in% outputs) {
      res$TruePosC.threshold <- do.call("rbind", TPcT)
      res$TruePosV.threshold <- do.call("rbind", TPvT)
      res$TrueNegC.threshold <- do.call("rbind", TNcT)
      res$TrueNegV.threshold <- do.call("rbind", TNvT)
      res$FalsePosC.threshold <- do.call("rbind", FPcT)
      res$FalsePosV.threshold <- do.call("rbind", FPvT)
      res$FalseNegC.threshold <- do.call("rbind", FNcT)
      res$FalseNegV.threshold <- do.call("rbind", FNvT)
    }
    if ("ER" %in% outputs) {
      res$ErrorRateC.threshold <- do.call("rbind", ERcT)
      res$ErrorRateV.threshold <- do.call("rbind", ERvT)
      res$ErrorRateCglobal.threshold <- do.call("rbind", 
                                                ERcT.global)
      res$ErrorRateVglobal.threshold <- do.call("rbind", 
                                                ERvT.global)
    }
  }
  if ((nNoBin == 0) & ("AUC" %in% outputs)) {
    res$AUCc <- do.call("rbind", AUCc)
    res$AUCv <- do.call("rbind", AUCv)
    res$AUCc.global <- do.call("rbind", AUCc.global)
    res$AUCv.global <- do.call("rbind", AUCv.global)
  }
  if ("RMSE" %in% outputs) {
    rownames(res$RMSEc.global) <- rownames(res$RMSEv.global) <- NULL
  }
  if ("max" %in% algo) {
    if ("ConfMat" %in% outputs) {
      rownames(res$TruePosC.max) <- rownames(res$TruePosV.max) <- rownames(res$TrueNegC.max) <- rownames(res$TrueNegV.max) <- rownames(res$FalsePosC.max) <- rownames(res$FalsePosV.max) <- rownames(res$FalseNegC.max) <- rownames(res$FalseNegV.max) <- NULL
    }
    if ("ER" %in% outputs) {
      rownames(res$ErrorRateC.max) <- rownames(res$ErrorRateV.max) <- rownames(res$ErrorRateCglobal.max) <- rownames(res$ErrorRateVglobal.max) <- NULL
    }
  }
  if ("gravity" %in% algo) {
    if ("ConfMat" %in% outputs) {
      rownames(res$TruePosC.gravity) <- rownames(res$TruePosV.gravity) <- rownames(res$TrueNegC.gravity) <- rownames(res$TrueNegV.gravity) <- rownames(res$FalsePosC.gravity) <- rownames(res$FalsePosV.gravity) <- rownames(res$FalseNegC.gravity) <- rownames(res$FalseNegV.gravity) <- NULL
    }
    if ("ER" %in% outputs) {
      rownames(res$ErrorRateC.gravity) <- rownames(res$ErrorRateV.gravity) <- rownames(res$ErrorRateCglobal.gravity) <- rownames(res$ErrorRateVglobal.gravity) <- NULL
    }
  }
  if ("threshold" %in% algo) {
    if ("ConfMat" %in% outputs) {
      rownames(res$TruePosC.threshold) <- rownames(res$TruePosV.threshold) <- rownames(res$TrueNegC.threshold) <- rownames(res$TrueNegV.threshold) <- rownames(res$FalsePosC.threshold) <- rownames(res$FalsePosV.threshold) <- rownames(res$FalseNegC.threshold) <- rownames(res$FalseNegV.threshold) <- NULL
    }
    if ("ER" %in% outputs) {
      rownames(res$ErrorRateC.threshold) <- rownames(res$ErrorRateV.threshold) <- rownames(res$ErrorRateCglobal.threshold) <- rownames(res$ErrorRateVglobal.threshold) <- NULL
    }
  }
  if ((nNoBin == 0) & ("AUC" %in% outputs)) {
    rownames(res$AUCc) <- rownames(res$AUCv) <- rownames(res$AUCc.global) <- rownames(res$AUCv.global) <- NULL
  }
  res$call <- match.call()
  class(res) <- c("testdim_mbplsda")
  return(res)
}