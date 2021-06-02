
library(shiny)


shinyUI(fluidPage(
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"),
    tabsetPanel(
      tabPanel("Tuition Cost", 
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
      ),  

      tabPanel("Correlation",
          titlePanel("Correlation between tuition and career pay"),
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("nameText"),
                   uiOutput("chosenState")
                 ),
                mainPanel(
                  plotOutput("scatterplot")
                )
                
               )
      ),
      
      tabPanel("Diversity",
               titlePanel("Diversity rate by race in Universities"),
               sidebarLayout(
                 sidebarPanel(
                   
                 ),
                 mainPanel(
                   plotOutput("barPlot")
                 )
                 
               )
      )
    )
))


