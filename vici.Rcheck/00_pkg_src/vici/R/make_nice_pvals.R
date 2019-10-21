make_nice_pvals <- function(res_2plot, data_df, auxvar = "arm"){
  data_df$stim <- as.factor(as.numeric(data_df$stim))
  pval_2plot <- res_2plot
  naux <- nlevels(data_df[, auxvar])
  nstim <- nlevels(data_df$stim)
  pval_2plot$y.position <- as.vector(by(data_df$response, INDICES = data_df$stim, FUN = max, na.rm=TRUE)) +
    0.05*max(data_df$response, na.rm = TRUE)
  pval_2plot$group1 <- 1:nlevels(data_df$stim) - (naux-1)*0.4/naux
  pval_2plot$group2 <- pval_2plot$group1
  for(i in 2:(naux)){
    pval_2plot$group2[(i-2)*(nstim) +1:nstim] <- pval_2plot$group1[(i-2)*(nstim) +1:nstim] + 2*(i-1)*0.4/naux
    if(i>2){
      pval_2plot$y.position[(i-2)*(nstim) +1:nstim] <- pval_2plot$y.position[(i-2)*(nstim) +1:nstim] + (i-2)*0.1*max(data_df$response, na.rm = TRUE)
    }
  }
  pval_2plot$pvalue_format <- formatC(pval_2plot$pvalue, format = "e", digits=2)
  if(length(pval_2plot$pvalue > 0.01) > 0){
    pval_2plot$pvalue_format[pval_2plot$pvalue > 0.01] <- formatC(pval_2plot$pvalue[pval_2plot$pvalue > 0.01], digits=3)
  }
  if(length(pval_2plot$pvalue > 0.05) > 0){
    pval_2plot$pvalue_format[pval_2plot$pvalue > 0.05] <- paste0("NS (p-value = ", formatC(pval_2plot$pvalue[pval_2plot$pvalue > 0.05], digits=3), ")")
  }
  if(length(pval_2plot$pvalue < 0.05) > 0){
    pval_2plot$pvalue_format[pval_2plot$pvalue < 0.05] <- paste0("* (p-value = ", pval_2plot$pvalue_format[pval_2plot$pvalue < 0.05], ")")
  }
  if(length(pval_2plot$pvalue < 0.01) > 0){
    pval_2plot$pvalue_format[pval_2plot$pvalue < 0.01] <- paste0("*", pval_2plot$pvalue_format[pval_2plot$pvalue < 0.01])
  }
  if(length(pval_2plot$pvalue < 0.001) > 0){
    pval_2plot$pvalue_format[pval_2plot$pvalue < 0.001] <- paste0("*", pval_2plot$pvalue_format[pval_2plot$pvalue < 0.001])
  }
  return(pval_2plot)
}
