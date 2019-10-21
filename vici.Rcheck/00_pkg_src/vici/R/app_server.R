#' @import shiny
#' @import ggpubr
#' @importFrom nlme gls varIdent
#' @importFrom utils read.csv write.table
#' @importFrom stats coef relevel as.formula model.matrix
#' @importFrom tidyr spread
#' @importFrom cowplot plot_grid
app_server <- function(input, output, session) {

  # initialize everything ----
  output$mod <- reactive(NULL)
  output$mod_display <- reactive(FALSE)
  output$res_sentence <- reactive(NULL)
  #output$res_tab <- reactive(NULL)
  output$res_error <- reactive(NULL)
  output$res_lik <- reactive(NULL)
  output$heatmap <- reactive(NULL)
  output$boxplot <- reactive(NULL)
  output$downloadHM <- reactive(NULL)
  output$downloadBP <- reactive(NULL)
  output$res_var <- reactive(NULL)
  output$armisfactor <- reactive(TRUE)
  output$arm2isfactor <- reactive(TRUE)
  output$timeisfactor <- reactive(TRUE)
  output$time2isfactor <- reactive(TRUE)
  output$stimisfactor <- reactive(TRUE)
  output$warningarmisfactor <- reactive(NULL)
  output$warningarm2isfactor <- reactive(NULL)
  output$warningstimisfactor <- reactive(NULL)
  output$warningtimeisfactor <- reactive(NULL)
  output$warningtime2isfactor <- reactive(NULL)
  outputOptions(output, "mod_display", suspendWhenHidden = FALSE)
  outputOptions(output, "warningarmisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "warningarm2isfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "warningstimisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "armisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "arm2isfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "stimisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "res_error", suspendWhenHidden = FALSE)
  #outputOptions(output, "res_tab", suspendWhenHidden = FALSE)
  outputOptions(output, "res_lik", suspendWhenHidden = FALSE)
  outputOptions(output, "res_var", suspendWhenHidden = FALSE)
  outputOptions(output, "heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "downloadHM", suspendWhenHidden = FALSE)
  outputOptions(output, "downloadBP", suspendWhenHidden = FALSE)

  data <- reactiveValues()
  data$fact_stim_OK <- TRUE
  data$fact_arm_OK <- TRUE
  data$fact_time_OK <- TRUE
  data$fact_time2_OK <- TRUE
  
  res_data <- NULL


  # data import ----
  output$table2render <- DT::renderDataTable({
    req(input$datafile)
    data$df
  },
  options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
  )

  output$downloadRes <- downloadHandler(
    filename = "ResVICI.txt",
    
    content = function(file){
      utils::write.table(res_data,file,row.names = TRUE, sep = "\t", quote = FALSE)
    }
  )

  # example data
  output$downloadExData <- downloadHandler(   #Fait appel à une lib externe donc pas besoin de tester
    filename = "exampleICSdata.txt",
    content = function(file) {
      utils::write.table(vici::ICS_ex, file, row.names = FALSE, sep="\t", quote = FALSE)
    }
  )
  observeEvent(input$loadExample,{
    #cat("observe loadExample", "\n")
    data$df <- vici::ICS_ex
    clean_output(output)
    output$table2render <- DT::renderDataTable(data$df,
                                               options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
    )
    #Ensemble setter
    output$mod <- reactive(NULL)
    output$mod_display <- reactive(FALSE)
    updateRadioButtons(session, inputId = "sep", selected = "\t")
    updateCheckboxInput(session, inputId = "header", value = TRUE)
    updateSelectInput(session, "selectModel", selected = 1)
    available_vars_init <- colnames(data$df)
    updateSelectizeInput(session, "selectSubject",
                         selected = 'Subject',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectResponse",
                         selected = c('Response1', 'Response2'),
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectStim",
                         selected = 'StimulationPool',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefStim",
                         selected = 'NS',
                         choices = c(levels(data$df$StimulationPool))
    )
    updateSelectizeInput(session, "selectArm",
                         selected = 'Arm',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefArm",
                         selected = 'Placebo',
                         choices = c(levels(data$df$Arm))
    )
    updateSelectizeInput(session, "selectArm2",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTime2",
                         selected = 'TimePoint',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefTime2",
                         selected = 'D1',
                         choices = c(levels(data$df$TimePoint))
    )
    updateTabsetPanel(session, "inTabset", selected = "dataTab")
  })


  observeEvent({input$datafile; input$header; input$sep}, {
    #cat("observe datainput", "\n")
    req(input$datafile)
    data$df <- {
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- utils::read.csv(input$datafile$datapath,
                                header = input$header,
                                sep = input$sep)
        },
        error = function(e){ stop(safeError(e)) } # return a safeError if a parsing error occurs
      )
      
      #Setters 
      clean_output(output)
      output$mod <- reactive(NULL)
      output$mod_display <- reactive(FALSE)
      df}

    available_vars_init <- colnames(data$df)
    updateSelectizeInput(session, "selectSubject",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectResponse",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectStim",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm2",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTime",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTime2",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefTime",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefTime2",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefArm",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefArm2",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefStim",
                         selected = ''
    )
    updateTabsetPanel(session, "inTabset", selected = "dataTab")
  })


  # update available variables for selection ----
  # observeEvent available_vars ----
  observeEvent(data$available_vars, {
    #cat("observe available_vars", "\n")
    updateSelectizeInput(session, "selectSubject",
                         choices = c(input$selectSubject, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectResponse",
                         selected = input$selectResponse,
                         choices = as.list(c(input$selectResponse, data$available_vars, intToUtf8(160))),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectStim",
                         choices = c(input$selectStim, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm",
                         choices = c(input$selectArm, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm2",
                         choices = c(input$selectArm2, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTime",
                         choices = c(input$selectTime, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTime2",
                         choices = c(input$selectTime2, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
  }
  )

  observeEvent(input$selectSubject, {
    #cat("observe selectSubj", "\n")
    if (input$selectSubject != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df)) #A tester
    }
    clean_output(output)
  })

  observeEvent(input$selectStim, {
    #cat("observe selectStim", "\n")
    if (input$selectStim != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      if (input$selectStim %in% colnames(data$df)){
        selected_stim_var <- data$df[, input$selectStim]
        if(is.factor(selected_stim_var)){
          output$stimisfactor <- reactive(TRUE)
          possible_stims <- levels(selected_stim_var)
          output$warningstimisfactor <- reactive(NULL)
          data$fact_stim_OK <- TRUE
        }else{
          output$stimisfactor <- reactive(FALSE)
          output$warningstimisfactor <- reactive(paste0("WARNING: '", input$selectStim, "' is not a factor"))
          data$fact_stim_OK <- FALSE
          possible_stims <- paste0("Error: '", input$selectStim, "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefStim",
                             choices = c(possible_stims[1], possible_stims),
                             selected = ifelse(input$selectRefStim=='', possible_stims[1], input$selectRefStim)
        )
      }else{
        output$stimisfactor <- reactive(FALSE)
        output$warningstimisfactor <- reactive(paste0("WARNING: '", input$selectStim, "' is not a column in the input data"))
        data$fact_stim_OK <- FALSE
      }
    }
    clean_output(output)
  })

  observeEvent(input$selectResponse, {
    #cat("observe selectResp", "\n")
    if (length(input$selectResponse) >= 1){
      if (input$selectResponse[1] != ''){

        data$available_vars <- update_vars(input, possibilities = colnames(data$df))

        clean_output(output)
      }
    }
  })

  observeEvent(input$selectArm, {
    #cat("observe selectArm", "\n")
    if (input$selectArm != ''){

      data$available_vars <- update_vars(input, possibilities = colnames(data$df))


      if (input$selectArm %in% colnames(data$df)){
        selected_arm_var <- data$df[, input$selectArm]
        if(is.factor(selected_arm_var)){
          output$armisfactor <- reactive(TRUE)
          possible_arms <- levels(selected_arm_var)
          output$warningarmisfactor <- reactive(NULL)
          data$fact_arm_OK <- TRUE
          # if(length(possible_arms) > 2){
          #   output$armisfactor <- reactive(FALSE)
          #   output$warningarmisfactor <- reactive(paste0("Error: '", input$selectArm, "' has more than 2 levels\n This is not implemented yet"))
          #   data$fact_arm_OK <- FALSE
          # }
        }else{
          output$armisfactor <- reactive(FALSE)
          output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArm,
                                                       "' is not a factor"))
          data$fact_arm_OK <- FALSE
          possible_arms <- paste0("Error: '", input$selectArm,
                                  "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefArm",
                             choices = c(possible_arms[1], possible_arms),
                             selected = ifelse(is.null(input$selectRefArm) | (length(input$selectRefArm)>0 && input$selectRefArm==''),
                                               possible_arms[1], input$selectRefArm)
        )
      }else if(input$selectArm != intToUtf8(160)){
        output$armisfactor <- reactive(FALSE)
        output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArm,
                                                     "' is not a column in the input data"))
        data$fact_arm_OK <- FALSE
      }else{
        output$armisfactor <- reactive(FALSE)
        output$warningarmisfactor <- reactive(NULL)
        data$fact_arm_OK <- FALSE

        data$available_vars <-  update_vars(input, possibilities = colnames(data$df)) #A tester

      }
    }else{

      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))

    }
    clean_output(output)
  })


  observeEvent(input$selectArm2, {
    #cat("observe selectArm2", "\n")
    if (input$selectArm2 != ''){

      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))


      if (input$selectArm2 %in% colnames(data$df)){
        selected_arm2_var <- data$df[, input$selectArm2]
        if(is.factor(selected_arm2_var)){
          output$arm2isfactor <- reactive(TRUE)
          possible_arm2s <- levels(selected_arm2_var)
          output$warningarm2isfactor <- reactive(NULL)
          data$fact_arm2_OK <- TRUE
          # if(length(possible_arm2s) > 2){
          #   output$arm2isfactor <- reactive(FALSE)
          #   output$warningarm2isfactor <- reactive(paste0("Error: '", input$selectArm2, "' has more than 2 levels\n This is not implemented yet"))
          #   data$fact_arm2_OK <- FALSE
          # }
        }else{
          output$arm2isfactor <- reactive(FALSE)
          output$warningarm2isfactor <- reactive(paste0("WARNING: '", input$selectArm2, #paste concatène chaine caractère 
                                                        "' is not a factor"))
          data$fact_arm2_OK <- FALSE
          possible_arm2s <- paste0("Error: '", input$selectArm2,
                                   "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefArm2",
                             choices = c(possible_arm2s[1], possible_arm2s),
                             selected = ifelse(is.null(input$selectRefArm2) | (length(input$selectRefArm2)>0 && input$selectRefArm2==''),
                                               possible_arm2s[1], input$selectRefArm2)
        )
      }else if(input$selectArm2 != intToUtf8(160)){
        output$arm2isfactor <- reactive(FALSE)
        output$warningarm2isfactor <- reactive(paste0("WARNING: '", input$selectArm2,
                                                      "' is not a column in the input data"))
        data$fact_arm2_OK <- FALSE
      }else{
        output$arm2isfactor <- reactive(FALSE)
        output$warningarm2isfactor <- reactive(NULL)
        data$fact_arm2_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })

  observeEvent(input$selectModel, {
    #cat("observe selectModel", "\n")
    if(!is.null(data$available_vars)){
      updateSelectizeInput(session, "selectArm",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArm, input$selectArm2), input$selectTime))
      )
      updateSelectizeInput(session, "selectArm2",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArm, input$selectArm2), input$selectTime))
      )
      updateSelectizeInput(session, "selectTime",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArm, input$selectArm2), input$selectTime))
      )
      updateSelectizeInput(session, "selectTime2",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArm, input$selectArm2), input$selectTime))
      )
      updateSelectizeInput(session, "selectRefTime",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefTime2",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefArm",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefArm2",
                           selected = ''
      )
    }
    clean_output(output)
  }
  )

  # observe time ----
  observeEvent(input$selectTime, {
    #cat("observe selectTime", "\n")
    if (input$selectTime != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      if(input$selectTime %in% colnames(data$df)){
        data$df[, input$selectTime] <- as.factor(as.character(data$df[, input$selectTime]))
        selected_time_var <- data$df[, input$selectTime]
        output$timeisfactor <- reactive(TRUE)
        possible_times <- levels(selected_time_var)
        output$warnintimeisfactor <- reactive(NULL)
        data$fact_time_OK <- TRUE

        updateSelectizeInput(session, "selectRefTime",
                             choices = c(possible_times[1], possible_times),
                             selected = ifelse(is.null(input$selectRefTime) | (length(input$selectRefTime)>0 && input$selectRefTime==''),
                                               possible_times[1], input$selectRefTime)
        )
      }else if(input$selectTime != intToUtf8(160)){
        output$timeisfactor <- reactive(FALSE)
        output$warnintimeisfactor <- reactive(paste0("WARNING: '", input$selectTime,
                                                     "' is not a column in the input data"))
        data$fact_time_OK <- FALSE
      }else{
        output$timeisfactor <- reactive(FALSE)
        output$warnintimeisfactor <- reactive(NULL)
        data$fact_time_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })

  # observe time ----
  observeEvent(input$selectTime2, {
    #cat("observe selectTime2", "\n")
    if (input$selectTime2 != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      if(input$selectTime2 %in% colnames(data$df)){
        data$df[, input$selectTime2] <- as.factor(as.character(data$df[, input$selectTime2]))
        selected_time2_var <- data$df[, input$selectTime2]
        output$time2isfactor <- reactive(TRUE)
        possible_times2 <- levels(selected_time2_var)
        output$warnintime2isfactor <- reactive(NULL)
        data$fact_time2_OK <- TRUE

        updateSelectizeInput(session, "selectRefTime2",
                             choices = c(possible_times2[1], possible_times2),
                             selected = ifelse(is.null(input$selectRefTime2) | (length(input$selectRefTime2)>0 && input$selectRefTime2==''),
                                               possible_times2[1], input$selectRefTime2)
        )
      }else if(input$selectTime2 != intToUtf8(160)){
        output$time2isfactor <- reactive(FALSE)
        output$warnintime2isfactor <- reactive(paste0("WARNING: '", input$selectTime2,
                                                     "' is not a column in the input data"))
        data$fact_time2_OK <- FALSE
      }else{
        output$time2isfactor <- reactive(FALSE)
        output$warnintime2isfactor <- reactive(NULL)
        data$fact_time2_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })


  observeEvent({input$selectRefArm; input$selectRefArm2; input$selectRefStim; input$selectRefTime; input$selectRefTime2}, {
    #cat("observe selectRefs", "\n")
    clean_output(output) # a tester
  })


  observeEvent(
    # Run whenever fit button is pressed
    input$modelfit, {
      #cat("observe modelfit", "\n")
      output$res_error <- reactive("Please select adequate analysis parameters...")
      responses_res <- list()
      boxplot_print <- list()
      heatmap_data2plot <- list()
      toomuchdata <- FALSE

      for(response in input$selectResponse){
        if(!is.null(data$df) & input$selectSubject %in% colnames(data$df) &
           input$selectStim %in% colnames(data$df) & data$fact_stim_OK &
           ((input$selectArm %in% colnames(data$df) & data$fact_arm_OK) |
            (input$selectTime %in% colnames(data$df) & data$fact_time_OK))
        ) {
          if(input$selectModel == 1){

            # data tansformation
            if(input$selectTime2 != ''){
              data_df <- data$df[data$df[, input$selectTime2] == input$selectRefTime2,
                                 c(input$selectSubject, response, input$selectStim, input$selectArm)]
            }else{
              data_df <- data$df[, c(input$selectSubject, response, input$selectStim, input$selectArm)]
            }
            colnames(data_df) <- c("Subject", "response", "stim", "arm")
            transformed_data <- data_df
            transformed_data$bkg <- 0 # intialize bkg ground
            transformed_data <- transformed_data[order(transformed_data$stim, transformed_data$Subject), ] # align stimulations so that subject order matches in the following loop
            for(l in levels(transformed_data$stim)){
              if(l != input$selectRefStim){
                transformed_data[transformed_data$stim == l, "bkg"] <- transformed_data[transformed_data$stim == input$selectRefStim, "response"]
              }
            }
            transformed_data$arm <- stats::relevel(transformed_data$arm, ref=input$selectRefArm)
            transformed_data$stim <- stats::relevel(transformed_data$stim, ref=input$selectRefStim)
            data_df$stim <- relevel(data_df$stim, ref=input$selectRefStim)

            # model fit ----
            fit_res <- interarm_fit(transformed_data, input)

            if(!inherits(fit_res$mgls, "try-error")){
              responses_res[[response]]$res_error <- NULL
              responses_res[[response]]$postprocess_res <- interarm_postprocessres(data_df, fit_res)
             # if(is.null(res_data)){
            #    res_data <<- responses_res[[response]]$postprocess_res
              #}else{
              #  
              #}
              #cat("res_data should update","\n")
              boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                        response_name = response, input = input)
              heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
              heatmap_data2plot[[response]]$response <- response
              heatmap_data2plot[[response]]$pvalue <- cut(heatmap_data2plot[[response]]$pvalue,
                                                          breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                          right = FALSE)
              responses_res[[response]]$res_tab <- fit_res$res_tab
              #output$res_tab <- renderTable(fit_res$res_tab, rownames = TRUE, digits=5)

            }

          }else if(input$selectModel == 2){

            # data tansformation
            if(input$selectArm2 != ''){
              data_df <- data$df[data$df[, input$selectArm2] == input$selectRefArm2,
                                 c(input$selectSubject, response, input$selectStim, input$selectTime)]
            }else{
              data_df <- data$df[, c(input$selectSubject, response, input$selectStim, input$selectTime)]
            }
            colnames(data_df) <- c("Subject", "response", "stim", "time")
            data_df$stim <- stats::relevel(data_df$stim, ref=input$selectRefStim)
            transformed_data <- data_df

            transformed_data$time <- stats::relevel(transformed_data$time, ref=input$selectRefTime)
            transformed_data <- try(tidyr::spread(transformed_data, key = "time", value = "response"),
                                    silent = TRUE)

            if(inherits(transformed_data, "try-error")){
              clean_output(output)
              toomuchdata <- TRUE
              output$res_error <- reactive(paste0("Too many observation in time point ", input$selectRefTime,
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
                  if(l != input$selectRefStim){
                    transformed_data_temp[transformed_data_temp$stim == l, "bkg"] <- transformed_data_temp[transformed_data_temp$stim ==
                                                                                                             input$selectRefStim, "response"]
                  }
                }
                transformed_data_temp$stim <- stats::relevel(transformed_data_temp$stim, ref=input$selectRefStim)

                # model fit ----
                #cat("intraarm_fit","\n")
                fit_res[[tp]] <- intraarm_fit(transformed_data = transformed_data_temp,
                                              tested_time = tp, input = input)
              }
              #cat("fit_res => ")
              #cat(str(fit_res),"\n")
              if(!prod(sapply(fit_res, function(x){inherits(x$mgls, "try-error")}))){
                responses_res[[response]]$res_error <- NULL
                responses_res[[response]]$postprocess_res <- intraarm_postprocessres(data_df, fit_res)
                #res_data <<- responses_res[[response]]$postprocess_res
                cat("res_data should update","\n")
                #responses_res[[response]]$postprocess_res$pval_2plot <- do.call(rbind, responses_res[[response]]$postprocess_res$pval_2plot)
                boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                               response_name = response,
                               input = input,
                               inter = FALSE,
                               baseline = input$selectRefTime)
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
          output$res_error <- reactive("Please select adequate analysis parameters before trying to fit the model...")
        }else{
          myTabs <- lapply(input$selectResponse, function(resp) {
            cat("res_data => ")
            cat(as.character(res_data),"\n")
            if(is.null(res_data)){
              res_data <<- responses_res[[resp]]$res_tab
            }else{
              res_data <<- rbind(res_data,responses_res[[resp]]$res_tab)
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
          output$boxplotsAndTabs <- renderUI({
            do.call(tabsetPanel, myTabs)
          })

          if(input$selectModel == 1){
            output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                    nrow(responses_res[[1]]$res_tab) + ncol(responses_res[[1]]$postprocess_res$vars))})
            heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", rownames(x)), ]})
          }else if(input$selectModel == 2){
            output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                    nrow(responses_res[[1]]$res_tab) + length(responses_res[[1]]$postprocess_res$vars))})
            heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", x$Stimulation), ]})
          }

          output$res_error <- reactive(NULL)
          res_lik_all <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "res_lik")
          res_lik_all <- do.call(rbind.data.frame, res_lik_all)
          #colnames(res_lik_all) <- c("AIC", "-2 Res. logLikelihood")
          output$res_lik <- renderTable(res_lik_all,
                                        rownames = TRUE, digits = 4)
          all_vars <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "vars")
          all_vars <- do.call(rbind.data.frame, all_vars)
          #colames(all_vars) <- levels(data_df$stim)
          output$res_var <- renderTable(all_vars, rownames = TRUE, digits=6)
          #boxplot_dnl <- cowplot::plot_grid(plotlist = boxplot_print)
          #output$boxplot <- renderPlot(boxplot_dnl)
          #output$downloadBP <- myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_dnl)

          hm_data2plot_all <- do.call(rbind.data.frame, heatmap_data2plot_noref)
          hm_data2plot_all$response <- factor(hm_data2plot_all$response, ordered = TRUE,
                                              levels = rev(input$selectResponse))
          #TODO if model 2: cross response x time points on y-axis
          if(input$selectModel == 2)
            heatmap_print <- heatmap_vici(hm_data2plot_all, inter = FALSE,
                                          baseline = input$selectRefTime)
          else{
            heatmap_print <- heatmap_vici(hm_data2plot_all, inter = TRUE)
          }
          output$heatmap <- renderPlot(heatmap_print)
          output$downloadHM <- myDownloadHandlerForPlots(name = "VICIheatmap.png", plot_obj = heatmap_print)
        }
      }
      updateTabsetPanel(session, "inTabset", selected = "resTab")
    })

  observeEvent({input$selectModel;
    input$selectStim; input$selectRefStim;
    input$selectArm; input$selectRefArm;
    input$selectTime; input$selectRefTime}, {
      #cat("observe moddisplay", "\n")

      # write LaTeX model ----

      if(input$selectModel == 1 & input$selectRefStim != '' & input$selectRefArm != '' & input$selectStim !='' &
         input$selectArm %in% colnames(data$df) & input$selectStim %in% colnames(data$df)){
        output$mod_display <- reactive(TRUE)
        arm_coefs <- NULL
        for(a in levels(data$df[, input$selectArm])){
          if(a != input$selectRefArm){
            arm_coefs <- paste0(arm_coefs, '+ \\beta_{', a,'}^{', input$selectRefStim, '}', a,
                                '_i')
          }
        }
        statmodel <- paste0('$$y_i^{', input$selectRefStim, '} = \\beta_0^{', input$selectRefStim,
                            '}', arm_coefs, '+ \\varepsilon_i^{', input$selectRefStim, '}$$')
        for(s in levels(data$df[, input$selectStim])){

          if(s != input$selectRefStim){
            arm_coefs <- NULL
            for(a in levels(data$df[, input$selectArm])){
              if(a != input$selectRefArm){
                arm_coefs <- paste0(arm_coefs, '+ \\beta_{', a,'}^{', s, '}', a,
                                    '_i')
              }
            }
            statmodel <- paste0(statmodel, '$$y_i^{', s, '} = \\beta_0^{', s, '} ',
                                arm_coefs, '+ \\beta_{', input$selectRefStim, '}^{', s, '} y^{',
                                input$selectRefStim, '}_i + \\varepsilon_i^{', s, '}$$'
            )
          }
        }
        output$mod <- renderUI({
          withMathJax(statmodel)
        })
      }else if(input$selectModel == 2 & input$selectRefStim != '' & input$selectRefTime != '' & input$selectStim !='' &
               input$selectTime %in% colnames(data$df) & input$selectStim %in% colnames(data$df)) {
        output$mod_display <- reactive(TRUE)

        statmodel <- NULL
        for(t in levels(data$df[, input$selectTime])){
          if(t != input$selectRefTime){
            statmodel <- paste0(statmodel, '$$y_{diff\\,',t ,'\\, _i}^{', input$selectRefStim, '} = \\beta_{0\\,',t ,'}^{', input$selectRefStim,
                                '} ', '+ \\varepsilon_{',t ,'\\, _i}^{', input$selectRefStim, '}$$')
          }
        }
        for(s in levels(data$df[, input$selectStim])){
          if(s != input$selectRefStim){
            for(t in levels(data$df[, input$selectTime])){
              if(t != input$selectRefTime){
                statmodel <- paste0(statmodel, '$$y_{diff\\,',t ,'\\, _i}^{', s, '} = \\beta_{0\\,',t ,'}^{', s,
                                    '} + \\beta_{', input$selectRefStim, '\\,',t ,'}^{', s, '} \\,y^{',
                                    input$selectRefStim, '}_{diff\\,',t ,'\\, _i} + \\varepsilon_{',t ,'\\, _i}^{', s, '}$$'
                )
              }
            }
          }
        }
        diffdef <- paste0('where \\(y_{diff\\,\\{\\textsf{t}\\}\\, _i}^{\\{\\textsf{s}\\}} = y_i^{\\{\\textsf{s}\\}}(\\{\\textsf{t}\\}) - y_i^{\\{\\textsf{s}\\}}(',
                          input$selectRefTime, ')\\)'
        )
        output$mod <- renderUI({
          tagList(
            withMathJax(statmodel),
            div(""),
            div(diffdef)
          )
        })
      }else{
        output$mod <- reactive(NULL)
        output$mod_display <- reactive(FALSE)
      }
      clean_output(output)
    })
}
