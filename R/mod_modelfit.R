# Module UI
  
#' @title   mod_modelfit_ui and mod_modelfit_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data internal
#' @param parent internal
#' @param origin internal
#'
#' @rdname mod_modelfit
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 


mod_modelfit_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("fit"), label = "Fit model",
                 class = "btn-primary")
  )
}
    
# Module Server
    
#' @rdname mod_modelfit
#' @export
#' @keywords internal
    
mod_modelfit_server <- function(input, output, session, data,parent,origin){
  ns <- session$ns

  # Run whenever fit button is pressed
  
  
  observeEvent(input$fit, {#crash here
   #browser()
    origin$output$res_error <- reactive("Please select adequate analysis parameters...")
    responses_res <- list()
    boxplot_print <- list()
    heatmap_data2plot <- list()
    toomuchdata <- FALSE

    for(response in parent$selectResponse){
      if(!is.null(data$df) & parent$selectSubject %in% colnames(data$df) &
         parent$selectStim %in% colnames(data$df) & data$fact_stim_OK &
         (parent$selectArm %in% colnames(data$df) & data$fact_arm_OK) |
          (parent$selectTime %in% colnames(data$df) & data$fact_time_OK))
       {
        if(parent$selectModel == 1){
          # data tansformation
          if(parent$selectTime2 != ''){
            data_df <- data$df[data$df[, parent$selectTime2] == parent$selectRefTime2,
                               c(parent$selectSubject, response, parent$selectStim, parent$selectArm)]
          }else{
            data_df <- data$df[, c(parent$selectSubject, response, parent$selectStim, parent$selectArm)]
          }
          colnames(data_df) <- c("Subject", "response", "stim", "arm")
          transformed_data <- data_df
          transformed_data$bkg <- 0 # intialize bkg ground
          transformed_data <- transformed_data[order(transformed_data$stim, transformed_data$Subject), ] # align stimulations so that subject order matches in the following loop
          for(l in levels(transformed_data$stim)){
            if(l != parent$selectRefStim){
              transformed_data[transformed_data$stim == l, "bkg"] <- transformed_data[transformed_data$stim == parent$selectRefStim, "response"]
            }
          }
          transformed_data$arm <- stats::relevel(transformed_data$arm, ref=parent$selectRefArm)
          transformed_data$stim <- stats::relevel(transformed_data$stim, ref=parent$selectRefStim)
          data_df$stim <- relevel(data_df$stim, ref=parent$selectRefStim)

          # model fit ----
          fit_res <- interarm_fit(transformed_data, parent, response)
          if(!inherits(fit_res$mgls, "try-error")){
            responses_res[[response]]$res_error <- NULL
            responses_res[[response]]$postprocess_res <- interarm_postprocessres(data_df, fit_res)

            boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                      response_name = response, input = parent)
            heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
            heatmap_data2plot[[response]]$response <- response
            heatmap_data2plot[[response]]$pvalue <- cut(heatmap_data2plot[[response]]$pvalue,
                                                        breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                        right = FALSE)
            responses_res[[response]]$res_tab <- fit_res$res_tab

          }

        }else if(parent$selectModel == 2){

          # data tansformation
          if(parent$selectArm2 != ''){
            data_df <- data$df[data$df[, parent$selectArm2] == parent$selectRefArm2,
                               c(parent$selectSubject, response, parent$selectStim, parent$selectTime)]
          }else{
            data_df <- data$df[, c(parent$selectSubject, response, parent$selectStim, parent$selectTime)]
          }
          colnames(data_df) <- c("Subject", "response", "stim", "time")
          data_df$stim <- stats::relevel(data_df$stim, ref=parent$selectRefStim)
          transformed_data <- data_df

          transformed_data$time <- stats::relevel(transformed_data$time, ref=parent$selectRefTime)
          transformed_data <- try(tidyr::spread(transformed_data, key = "time", value = "response"),
                                  silent = TRUE)

          if(inherits(transformed_data, "try-error")){
            clean_output(output)
            toomuchdata <- TRUE
            origin$output$res_error <- reactive(paste0("Too many observation in time point ", parent$selectRefTime,
                                                "... Perhaps the Arm to analyzed was not specified"))
          }else{
            for(i in ncol(transformed_data):3){
              transformed_data[, i] <- (transformed_data[, i] - transformed_data[, 3])
            }

            fit_res <- list()
            for(t in 4:ncol(transformed_data)){
              tp <- colnames(transformed_data)[t]
              transformed_data_temp <- transformed_data[, c(1:2, t), drop=FALSE]
              colnames(transformed_data_temp)[3] <- "response"
              transformed_data_temp$bkg <- 0 # intialize bkg ground
              transformed_data_temp <- transformed_data_temp[order(transformed_data_temp$stim,
                                                                   transformed_data_temp$Subject), ] # align stimulations so that subject order matches in the following loop
              for(l in levels(transformed_data_temp$stim)){
                if(l != parent$selectRefStim){
                  transformed_data_temp[transformed_data_temp$stim == l, "bkg"] <- transformed_data_temp[transformed_data_temp$stim ==
                                                                                                           parent$selectRefStim, "response"]
                }
              }
              transformed_data_temp$stim <- stats::relevel(transformed_data_temp$stim, ref=parent$selectRefStim)

              # model fit ----
              fit_res[[tp]] <- intraarm_fit(transformed_data = transformed_data_temp,
                                            tested_time = tp, input = parent, resp = response)
            }

            if(!prod(sapply(fit_res, function(x){inherits(x$mgls, "try-error")}))){
              responses_res[[response]]$res_error <- NULL
              responses_res[[response]]$postprocess_res <- intraarm_postprocessres(data_df, fit_res)
              #res_data <<- responses_res[[response]]$postprocess_res
              #responses_res[[response]]$postprocess_res$pval_2plot <- do.call(rbind, responses_res[[response]]$postprocess_res$pval_2plot)
              boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                        response_name = response,
                                                        input = parent,
                                                        inter = FALSE,
                                                        baseline = parent$selectRefTime)
              responses_res[[response]]$res_tab <- do.call(rbind, lapply(fit_res, "[[", "res_tab"))
              heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
              for(l in 1:length(heatmap_data2plot[[response]])){
                heatmap_data2plot[[response]][[l]]$response <- response
                heatmap_data2plot[[response]][[l]]$pvalue <- cut(heatmap_data2plot[[response]][[l]]$pvalue,
                                                                 breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                                 right = FALSE)
              }
              heatmap_data2plot[[response]] <- do.call(rbind.data.frame,
                                                       heatmap_data2plot[[response]])
              #output$res_tab <- renderTable(fit_res$res_tab, rownames = TRUE, digits=5)
            }
          }
        }

      }
      #res_sentence <- renderText("A sentence to be copied and pasted in your analysis report.")
      
    }
    if(!toomuchdata){
      if(length(responses_res)<1){
        clean_output(output)
        origin$output$res_error <- reactive("Please select adequate analysis parameters before trying to fit the model...")
      }else{
        myTabs <- lapply(parent$selectResponse, function(resp) {
          if(is.null(session$userData$res_data)){
            session$userData$res_data<<- responses_res[[resp]]$res_tab
          }else{
            session$userData$res_data<<- rbind(session$userData$res_data,responses_res[[resp]]$res_tab)
          }
          tabPanel(title = resp, value = resp,
                   wellPanel(
                     fluidRow(
                       renderPlot(boxplot_print[[resp]])),
                     h6(""),
                     myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_print[[resp]],
                                               outputArgs = list(label = "Download boxplot [PNG]", class = "btn-primary")),
                     h3(""),
                     h4(paste("Numerical results for", resp)),
                     renderTable(responses_res[[resp]]$res_tab, rownames = TRUE, digits=5)
                   )
          )
        })
        origin$output$boxplotsAndTabs <- renderUI({
          do.call(tabsetPanel, myTabs)
        })

        if(parent$selectModel == 1){
          origin$output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                  nrow(responses_res[[1]]$res_tab) + ncol(responses_res[[1]]$postprocess_res$vars))})
          heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", rownames(x)), ]})
        }else if(parent$selectModel == 2){
          origin$output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                  nrow(responses_res[[1]]$res_tab) + length(responses_res[[1]]$postprocess_res$vars))})
          heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", x$Stimulation), ]})
        }

        origin$output$res_error <- reactive(NULL)
        res_lik_all <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "res_lik")
        res_lik_all <- do.call(rbind.data.frame, res_lik_all)
        #colnames(res_lik_all) <- c("AIC", "-2 Res. logLikelihood")
        origin$output$res_lik <- renderTable(res_lik_all,
                                      rownames = TRUE, digits = 4)
        all_vars <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "vars")
        all_vars <- do.call(rbind.data.frame, all_vars)
        #colames(all_vars) <- levels(data_df$stim)
        origin$output$res_var <- renderTable(all_vars, rownames = TRUE, digits=6)
        #boxplot_dnl <- cowplot::plot_grid(plotlist = boxplot_print)
        #output$boxplot <- renderPlot(boxplot_dnl)
        #output$downloadBP <- myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_dnl)

        hm_data2plot_all <- do.call(rbind.data.frame, heatmap_data2plot_noref)
        hm_data2plot_all$response <- factor(hm_data2plot_all$response, ordered = TRUE,
                                            levels = rev(parent$selectResponse))
        #TODO if model 2: cross response x time points on y-axis
        if(parent$selectModel == 2)
          heatmap_print <- heatmap_vici(hm_data2plot_all, inter = FALSE,
                                        baseline = parent$selectRefTime)
        else{
          heatmap_print <- heatmap_vici(hm_data2plot_all, inter = TRUE)
        }
        origin$output$heatmap <- renderPlot(heatmap_print)
        origin$output$downloadHM <- myDownloadHandlerForPlots(name = "VICIheatmap.png", plot_obj = heatmap_print)
      }
    }
    
    updateTabsetPanel(origin, "inTabset", selected = "resTab")
  })
  
}
    
## To be copied in the UI
# mod_modelfit_ui("modelfit_ui_1")
    
## To be copied in the server
# callModule(mod_modelfit_server, "modelfit_ui_1")
 
