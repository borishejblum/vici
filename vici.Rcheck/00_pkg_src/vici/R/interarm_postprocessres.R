interarm_postprocessres <- function(data_df, fit_res){
  m2resloglik <- -2*fit_res$mgls$logLik
  s_mgls <- summary(fit_res$mgls)
  aic <- s_mgls$AIC
  var_res <- fit_res$mgls$sigma^2

  sigmas <- stats::coef(fit_res$mgls$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) * fit_res$mgls$sigma
  vars <- t(cbind(sigmas^2))
  colnames(vars) <- levels(data_df$stim)
  rownames(vars) <- c("Variance")

  # model output ----
  res_lik <- t(c("AIC" = aic, "-2 Res. logLikelihood" = m2resloglik))

  res_2plot <- fit_res$res_tab[grep("Effect of arm", rownames(fit_res$res_tab)), ]
  metainfo_2plot <- do.call(rbind, strsplit(gsub(" reference", "", gsub(" stimulation", "",
                                                                        sapply(strsplit(rownames(res_2plot), "Effect of arm "), "[", 2))),
                                            " on response in "))
  res_2plot <- cbind.data.frame("Arm" = metainfo_2plot[, 1],
                                "Stimulation" = metainfo_2plot[, 2],
                                "pvalue" = res_2plot[, 3])

  pval_2plot <- make_nice_pvals(res_2plot, data_df, auxvar = "arm")


  return(list(
    "vars"  = vars,
    "pval_2plot" = pval_2plot,
    "res_lik" = res_lik,
    "res_2plot" = res_2plot,
    "pval_2plot" = pval_2plot
  ))
}
