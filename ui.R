
library(shiny)


shinyUI(fluidPage(

    tabsetPanel(
      titlePanel("Tuition Cost"),
      tabPanel("Tuition Cost", 
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
      )),  

      tabPanel("Correlation",
               sidebarLayout(
                 sidebarPanel(
                   
                 ),
                mainPanel(
                  
                )
               ))

    
    
))


