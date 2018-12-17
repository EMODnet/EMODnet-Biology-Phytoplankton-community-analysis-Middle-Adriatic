library(shiny)
shinyUI(fluidPage(
  titlePanel("Middle Adriatic plankton time series"),
  fluidRow(
    column(4,
           wellPanel(
             selectInput(inputId = "set1", label = "",
                         choices = c("Phytoplankton"),
                         selected = "Phytoplankton"),
             selectInput(inputId = "spec.set1", label = "Choose a species (group)",
                         choices="",selected = "Bacillariophyceae"),
             br(),
             checkboxInput(inputId = "transform",
                           label = strong("value double sqrt transformed"),
                           value = TRUE)
             )
    ),
    column(8,
           tabsetPanel(
             tabPanel("Observations", plotOutput("plot1", height ="375px", width = "100%"),
                                plotOutput("plot4", height ="375px", width = "100%")),
             tabPanel("Multiv 1",plotOutput("plM1", height ="700px", width = "100%")),
             tabPanel("about",htmlOutput("about"))

           )

    )
  )
))

