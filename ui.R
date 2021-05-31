
library(shiny)


shinyUI(fluidPage(
    titlePanel("Tuition Cost"),
    sidebarLayout(
        sidebarPanel(
          uiOutput("typeSchool"),
          uiOutput("degreeLength")
          
        ),
      
       mainPanel(
            plotOutput("map"),
            plotOutput("secondMap")
    )
    )
    
))


