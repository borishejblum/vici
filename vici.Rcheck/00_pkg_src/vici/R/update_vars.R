update_vars <- function(input, possibilities){
  setdiff(possibilities, union(union(union(union(union(union(input$selectSubject,
                                                             input$selectStim),
                                                       input$selectResponse),
                                                 input$selectArm),
                                           input$selectArm2),
                                     input$selectTime),
                               input$selectTime2))
}
