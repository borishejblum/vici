#'Fitting GLS For Intra-Arm Setting
#'
#'
#' @keywords internal
#' @importFrom stats na.omit
intraarm_fit <- function(transformed_data, tested_time, input){

  res_tab <- NULL
  res_lik <- NULL
  res_error <- NULL

  bkg_inter_mat <- model.matrix(data = stats::na.omit(transformed_data), ~ -1 + stim:bkg)[, -1, drop=FALSE]
  colnames(bkg_inter_mat) <- gsub(":", "_", colnames(bkg_inter_mat), fixed = TRUE)
  transformed_data <- cbind.data.frame(stats::na.omit(transformed_data), bkg_inter_mat)
  myformul <- as.formula(paste0("response ~ -1 + stim", "+", paste(colnames(bkg_inter_mat), collapse = " + ")))
  mgls <- try(nlme::gls(myformul,
                        data = transformed_data,
                        #correlation =  nlme::corCompSymm(form= ~ 1 | signal),
                        weights = nlme::varIdent(value = c("1" = 1), form = ~ 1 | stim),
                        method="REML", na.action = stats::na.omit
  ), silent = TRUE)
  # nlme::lme(fixed = response ~ -1 + signal + nosignal + signal:arm + nosignal:arm + bkg,
  #           data = transformed_data,
  #           random = list(Subject = nlme::pdDiag(form = ~ -1 + signal)),
  #           #correlation =  nlme::corCompSymm(form= ~ -1 + signal | Subject),
  #           weights = nlme::varIdent(form = ~ 1 | signal),
  #           method="REML"
  # )

  if(!inherits(mgls, "try-error")){

    # getting coef
    s_mgls <- summary(mgls)
    res_lik <- mgls$logLik
    res_tab <- s_mgls$tTable[, c(1,2,4)]
    colnames(res_tab) <- c("Estimate", "Standard error", "p-value")
    sigmas <- stats::coef(mgls$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) * mgls$sigma
    res_nparam <- renderText({paste0("<b>Number of estimated model parameters:</b> ", nrow(res_tab) + length(sigmas))})

    # pretty coef names
    rownames(res_tab)[1] <- paste0("Vaccine effect on response in reference stimulation ", input$selectRefStim,
                                   " at ", tested_time, " compared to baseline ", input$selectRefTime)
    nstim <- nlevels(transformed_data$stim)
    for(i in 1:(nstim-1)){
      rownames(res_tab)[1 + i] <- paste0("Vaccine effect on response in stimulation ", levels(transformed_data$stim)[1 + i],
                                         " at ", tested_time, " compared to baseline ", input$selectRefTime)
      rownames(res_tab)[nstim + i] <- paste0("Effect of reference stimulation ", input$selectRefStim,
                                             " on response in stimulation ", levels(transformed_data$stim)[1 + i],
                                             "  at ", tested_time, " compared to baseline ", input$selectRefTime)
    }

  }else{
    res_error <- paste0("Model was not able to run with the following error message:\n\n", mgls[1],
                        "\nMake sure analysis parameters are correct")
  }

  return(list("mgls" = mgls,
              "res_error" = res_error,
              "res_tab" = res_tab,
              "res_lik" = res_lik))
}
