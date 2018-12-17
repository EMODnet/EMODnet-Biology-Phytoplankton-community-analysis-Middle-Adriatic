library(shiny)

source("helper.R")

shinyServer(function(input, output,session) {

# the data used - updated whenever a new selection
        outVar1 = reactive({
            mydata = get(input$set1)
            names(mydata)[4:(length(mydata[1,]))]
            })
        observe({
            updateSelectInput(session, "spec.set1",choices = outVar1()
            )})
    output$plot1 <- renderPlot({
        plotspecs(input$set1,input$spec.set1,input$transform)
    }) 
  output$plot4 <- renderPlot({
    plotseas(input$set1,input$spec.set1,input$transform)
  }) 
  output$plM1 <- renderPlot({
    plotmultv(input$set1,input$spec.set1)
  })
    output$about <- renderUI({
      doc 
      })
})
