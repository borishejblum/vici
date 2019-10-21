#' @import shiny
app_ui <- function() {
  fluidPage(
    titlePanel("VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling",
               windowTitle = "VICI"),
    h6("v0.5.3"),
    h5(),

    shiny::actionLink(inputId='github_code', label="Source code",
                      icon = icon("github"),
                      onclick ="window.open('https://github.com/borishejblum/vici', '_blank')"),
    shiny::actionLink(inputId='github_code', label="CRAN package",
                      icon = icon("r-project"),
                      onclick ="window.open('https://CRAN.R-project.org/package=vici', '_blank')",
                      style='padding:10px;'),
    h5(),


    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        h3("Data input"),
        fileInput("datafile", label = "Choose a CSV/TXT file to import",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
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
        selectInput("selectModel", label = "Model choice",
                    choices = list("inter-arm" = 1, "intra-arm" = 2),
                    selected = 1),

        h4("Variable specification"),
        selectizeInput("selectSubject", label = "Select the column that identifies the subject ID",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        ),

        selectizeInput("selectResponse", label = "Select the column(s) that identify ICS response",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below'),
                       multiple = TRUE
        ),

        selectizeInput("selectStim", label = "Select the column that identifies the stimulation",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        ),
        conditionalPanel(
          condition = "input.selectStim != '' & !output.stimisfactor",
          verbatimTextOutput("warningstimisfactor")
        ),
        conditionalPanel(
          condition = "input.selectStim != '' & output.stimisfactor",
          selectizeInput("selectRefStim", label = "Select the value that identifies background samples",
                         choices =c(Choose = "", NULL))
        ),

        conditionalPanel(
          condition = "input.selectModel == 1",
          selectizeInput("selectArm", label = "Select the column that identifies the arm",
                         choices = c(Choose = "", NULL),
                         options = list(placeholder = 'Please select a column name below')
          )
        ),
        conditionalPanel(
          condition = "input.selectModel == 1 & input.selectArm != '' & !output.armisfactor & output.warningarmisfactor != null",
          verbatimTextOutput("warningarmisfactor")
        ),
        conditionalPanel(
          condition = "input.selectModel == 1 & input.selectArm != '' & output.armisfactor",
          selectizeInput("selectRefArm", label = "Select the value that identifies the reference arm",
                         choices =c(Choose = "", NULL))
        ),

        conditionalPanel(
          condition = "input.selectModel == 1",
          selectizeInput("selectTime2", label = "If several time-points (optional), please select the column that identifies the observation's time-point",
                         choices = c(Choose = "", NULL),
                         options = list(placeholder = 'Please select a column name below')
          )
        ),
        conditionalPanel(
          condition = "input.selectModel == 1 & input.selectTime2 != '' ",
          selectizeInput("selectRefTime2", label = "Select the time-point to analyze",
                         choices =c(Choose = "", NULL))
        ),

        conditionalPanel(
          condition = "input.selectModel == 2",
          selectizeInput("selectTime", label = "Select the column that identifies the time-points",
                         choices = c(Choose = "", NULL),
                         options = list(placeholder = 'Please select a column name below')
          )
        ),
        conditionalPanel(
          condition = "input.selectModel == 2 & input.selectTime != ''",
          selectizeInput("selectRefTime", label = "Select the value that identifies the reference time-point",
                         choices =c(Choose = "", NULL))
        ),

        conditionalPanel(
          condition = "input.selectModel == 2",
          selectizeInput("selectArm2", label = "If several arms (optional) please select the column that identifies the observation's arm",
                         choices = c(Choose = "", NULL),
                         options = list(placeholder = 'Please select a column name below')
          )
        ),
        conditionalPanel(
          condition = "input.selectModel == 2 & input.selectArm2 != '' & !output.armisfactor2 & output.warningarm2isfactor != null",
          verbatimTextOutput("warningarm2isfactor")
        ),
        conditionalPanel(
          condition = "input.selectModel == 2 & input.selectArm2 != '' & output.arm2isfactor",
          selectizeInput("selectRefArm2", label = "Select the arm to analyze",
                         choices =c(Choose = "", NULL))
        ),


        tags$hr(),
        h3("Run analysis"),
        actionButton("modelfit", label = "Fit model",
                     class = "btn-primary"),

        h3(),
        tags$hr(),
        h3("Example data"),
        fluidRow(
          actionButton("loadExample", label = "load example data")
        ),
        fluidRow(
          downloadButton("downloadExData", label = "download example data")
        )
      ),

      mainPanel(
        tabsetPanel(type = "tabs", id="inTabset",
                    tabPanel("Results", value="resTab",
                             # fluidRow(
                             #   column(8,
                             #          withMathJax(),
                             #          h3("Model"),
                             #          uiOutput('mod')),
                             # ),
                             conditionalPanel(
                               condition = "output.heatmap != null | output.res_error != null",
                               tags$hr(),
                               h3("Analysis results"),
                               
                               conditionalPanel(
                                 condition = "output.res_error != null",
                                 verbatimTextOutput("res_error")
                               ),
                               conditionalPanel(
                                 condition = "output.res_lik != null",
                                 #wellPanel(textOutput("res_sentence")),
                                 #h3(""),
                                 wellPanel(
                                   fluidRow(
                                     #column(6,
                                     plotOutput("heatmap"),
                                     h6(""),
                                     downloadButton("downloadHM", label = "Download heatmap [PNG]",
                                                    class = "btn-primary")
                                     #),
                                     # column(6, plotOutput("boxplot"),
                                     #        h6(""),
                                     #        downloadButton("downloadBP", label = "Download boxplot [PNG]",
                                     #                       class = "btn-primary")
                                     # )
                                   )
                                 ),
                                 h2(""),
                                 uiOutput('boxplotsAndTabs'),
                                 downloadButton("downloadRes", label = "Download Results", class = "btn-primary")
                               )
                             )
                    ),

                    # Output: Data file ----
                    tabPanel("Data view", value="dataTab",
                             conditionalPanel(
                               #tags$hr(),
                               condition = "output.table2render == null",
                               #h3("Data view")
                               helpText("Please input some data")
                             ),
                             h5(""),
                             DT::dataTableOutput("table2render")
                    ),
                    tabPanel("Additional Information", value="infoTab",
                             conditionalPanel(
                               condition = "output.mod_display",
                               withMathJax(),
                               h3("Statistical model fitted for each ICS response:"),
                               uiOutput('mod')
                               #)
                             ),
                             h2(),
                             tags$hr(),

                             conditionalPanel(
                               condition = "output.res_lik != null",
                               h3("Additional estimates:"),
                               wellPanel(htmlOutput("res_nparam")),
                               h5(""),
                               wellPanel(tableOutput("res_lik")),
                               h5(""),
                               wellPanel(h4("Estimated variances"),
                                         tableOutput("res_var"))
                             )
                    )
        )
      )
    )
  )
}
