intraarm_postprocessres <- function(data_df, fit_res){
  
  #browser()
  
  m2resloglik <- sapply(fit_res, function(x){-2*x$mgls$logLik})
  s_mgls <- lapply(fit_res, function(x){summary(x$mgls)})
  aic <- sapply(s_mgls, "[[", "AIC")
  var_res <- sapply(fit_res, function(x){x$mgls$sigma^2})

  sigmas <- sapply(fit_res, function(x){stats::coef(x$mgls$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) * x$mgls$sigma})
  vars <- t(cbind(sigmas^2))
  colnames(vars) <- levels(data_df$stim)
  #rownames(vars) <- levels(data_df$time)[-1]

  # model output ----
  res_lik <- cbind("AIC" = aic, "-2 Res. logLikelihood" = m2resloglik)

  res_2plot <- lapply(fit_res, function(x){x$res_tab[grep("Vaccine effect", rownames(x$res_tab)), ]})
  res_2plot <- lapply(res_2plot, function(y){
    temp <- cbind.data.frame(do.call(rbind, lapply(strsplit(gsub("on response in stimulation ", "",
                                        sapply(strsplit(rownames(y), "Vaccine effect "), "[", 2)),
                                   " at "),
                          function(x){c(x[1], strsplit(x[2], " compared to baseline ")[[1]][1])})),
                     "pvalue" = y[, 3])
    rownames(temp) <- NULL
    colnames(temp)[1:2] <- c("Stimulation", "Timepoint")
    return(temp)
  })

  pval_2plot <- make_nice_pvals(do.call(rbind.data.frame, res_2plot), data_df, auxvar = "time")
  #maybe TODO compute group2 correctly when more than 2 Timepoints
  #pval_2plot <- do.call(rbind, pval_2plot)


  return(list(
    "vars"  = vars,
    "pval_2plot" = pval_2plot,
    "res_lik" = res_lik,
    "res_2plot" = res_2plot,
    "pval_2plot" = pval_2plot
  ))
}
