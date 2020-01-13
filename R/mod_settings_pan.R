# Module UI
  
#' @title   mod_settings_pan_ui and mod_settings_pan_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data internal
#' @param parent
#'
#' @rdname mod_settings_pan
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_settings_pan_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      # Input: Select a file ----
      h3("Data input"),
      fileInput(ns("datafile"), label = "Choose a CSV/TXT file to import",
                #multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput(ns("header"), "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons(ns("sep"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      
      # Input: Select quotes ----
      # radioButtons("quote", "Quote",
      #              choices = c(None = "",
      #                          "Double Quote" = '"',
      #                          "Single Quote" = "'"),
      #              selected = '"'),
      
      # Horizontal line ----
      # tags$hr(),
      
      h3("Input parameters"),
      selectInput(ns("selectModel"), label = "Model choice",
                  choices = list("inter-arm" = 1, "intra-arm" = 2),
                  selected = 1),
      
      h4("Variable specification"),
      selectizeInput(ns("selectSubject"), label = "Select the column that identifies the subject ID",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below')
      ),
      
      selectizeInput(ns("selectResponse"), label = "Select the column(s) that identify ICS response",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below'),
                     multiple = TRUE
      ),
      
      selectizeInput(ns("selectStim"), label = "Select the column that identifies the stimulation",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below')
      ),
      conditionalPanel(
        condition = sprintf("input['%s']!= '' & !output.stimisfactor",ns("selectStim")),#"input.selectStim != '' & !output.stimisfactor",
        verbatimTextOutput(ns("warningstimisfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != '' & output.stimisfactor",ns("selectStim")),#"input.selectStim != '' & output.stimisfactor",
        selectizeInput(ns("selectRefStim"), label = "Select the value that identifies background samples",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 1",ns("selectModel")),#"input.selectModel == 1",
        selectizeInput(ns("selectArm"), label = "Select the column that identifies the arm",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' & !output.armisfactor & output.warningarmisfactor != null",ns("selectModel"),ns("selectArm")),#"input.selectModel == 1 & input.selectArm != '' & !output.armisfactor & output.warningarmisfactor != null",
        verbatimTextOutput(ns("warningarmisfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' & output.armisfactor",ns("selectModel"),ns("selectArm")),#"input.selectModel == 1 & input.selectArm != '' & output.armisfactor",
        selectizeInput(ns("selectRefArm"), label = "Select the value that identifies the reference arm",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 1",ns("selectModel")),#"input.selectModel == 1",
        selectizeInput(ns("selectTime2"), label = "If several time-points (optional), please select the column that identifies the observation's time-point",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' ",ns("selectModel"),ns("selectTime2")) ,#"input.selectModel == 1 & input.selectTime2 != '' ",
        selectizeInput(ns("selectRefTime2"), label = "Select the time-point to analyze",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 2",ns("selectModel")),#"input.selectModel == 2",
        selectizeInput(ns("selectTime"), label = "Select the column that identifies the time-points",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != ''",ns("selectModel"),ns("selectTime")),#"input.selectModel == 2 & input.selectTime != ''",
        selectizeInput(ns("selectRefTime"), label = "Select the value that identifies the reference time-point",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 2",ns("selectModel")),#"input.selectModel == 2",
        selectizeInput(ns("selectArm2"), label = "If several arms (optional) please select the column that identifies the observation's arm",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != '' & !output.armisfactor2 & output.warningarm2isfactor != null",ns("selectModel"),ns("selectArm2")) ,#"input.selectModel == 2 & input.selectArm2 != '' & !output.armisfactor2 & output.warningarm2isfactor != null",
        verbatimTextOutput(ns("warningarm2isfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != '' & output.arm2isfactor",ns("selectModel"),ns("selectArm2")) ,#"input.selectModel == 2 & input.selectArm2 != '' & output.arm2isfactor",
        selectizeInput(ns("selectRefArm2"), label = "Select the arm to analyze",
                       choices =c(Choose = "", NULL))
      ),
      
      
      tags$hr(),
      h3("Run analysis"),
      mod_modelfit_ui("modelfit_ui_1"),
      
      h3(),
      tags$hr(),
      h3("Example data"),
      fluidRow(
        actionButton(ns("loadExample"), label = "load example data")
      ),
      fluidRow(
        downloadButton(ns("downloadExData"), label = "download example data")
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_settings_pan
#' @export
#' @keywords internal
    
mod_settings_pan_server <- function(input, output, session,data,parent){
  ns <- session$ns
  #callModule(module = mod_modelfit_server, id = "modelfit_ui_1",data = data,parent = parent,parentModule = session)
  #browser()
  # example data
  output$downloadExData <- downloadHandler(   #Fait appel à une lib externe donc pas besoin de tester
    filename = "exampleICSdata.txt",
    content = function(file) {
      utils::write.table(vici::ICS_ex, file, row.names = FALSE, sep="\t", quote = FALSE)
    }
  )
  
  observeEvent(input$loadExample,{
    #cat("observe loadExample", "\n")

    data$df <<- vici::ICS_ex
    
    clean_output(output)

    parent$output$table2render <- DT::renderDataTable(data$df,
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
    updateTabsetPanel(parent, "inTabset", selected = "dataTab")
  })
  
  
  observeEvent({input$datafile; input$header; input$sep}, {
    cat("observe datainput", "\n")
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
      clean_output(parent$output)
      parent$output$mod <- reactive(NULL)
      parent$output$mod_display <- reactive(FALSE)
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
    updateTabsetPanel(parent, "inTabset", selected = "dataTab")
  })
  
  parent$output$table2render <- DT::renderDataTable(
    {
      req(input$datafile)
      data$df
    },
    options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
  )
  
  
  # update available variables for selection ----
  # observeEvent available_vars ----
  observeEvent(data$available_vars, {
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
    )}
  )
  
  observeEvent(input$selectSubject, {
    if (input$selectSubject != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df)) #A tester
    }
    clean_output(output)
  })
  
  observeEvent(input$selectStim, {
    #browser()
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
    #browser()
    #cat("observe selectResp", "\n")
    if (length(input$selectResponse) >= 1){
      if (input$selectResponse[1] != ''){
        
        data$available_vars <- update_vars(input, possibilities = colnames(data$df))
        
        clean_output(output)
      }
    }
  })
  
  observeEvent(input$selectArm, {
    #browser()
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
    #browser()
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
    #browser()
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
    #browser()
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
  
  #callModule(module = mod_modelfit_server, id = "modelfit_ui_1",data = data,parent = parent,parentModule = session)
  return(input)
}
    
## To be copied in the UI
# mod_settings_pan_ui("settings_pan_ui_1")
    
## To be copied in the server
# callModule(mod_settings_pan_server, "settings_pan_ui_1")
 
