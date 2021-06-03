
# Calling shiny library to enable shiny app function
library(shiny)


# shinyUI that draws a widget and panels on the scree of the app.
shinyUI(fluidPage(
    tabsetPanel(
      #Introduction tab
      tabPanel("Introduction",
               titlePanel("Introduction"),
               mainPanel(
                 br(),
                 img(src = "collegestudents.jpeg", height = 167, width = 500),
                 br(),
                 br(),
                 p(strong("Hoon Yu, Henna Lakha, Albert Yun")),
                 br(),
                 p("Our goal with this project was to give parents/guardians the information
                   necessary to make an informed decision about where to send their child for
                   higher education. We sought out to answer the following main questions for
                   our audience:"),p(strong("How do tuition costs and fees vary across the United States?
                   How does tuition cost relate to early career salary? And, finally, how does
                   diversity in these higher education institutions vary across the United States?")),
                 p("Our first dataset outlines 2018-2019 tuition costs and fees for colleges and
                   universities across the United States. Our second dataset outlines estimated
                   early career pay by college or university. Our third dataset outlines diversity
                   by college or university in 2014. Our first and third datasets come from the
                   Chronicle of Higher Education and the second dataset comes from payscale.com,
                   but all three were initially collected by the U.S. Department of Education.
                   We accessed these datasets from kaggle.com.")
               )),
      #Tuition tab.
      tabPanel("Tuition Cost", 
               titlePanel("Tuition Cost from 2018-2019"),
                   sidebarLayout(
                        sidebarPanel(
                          uiOutput("typeSchool"),
                          uiOutput("degreeLength"),
                          br(),
                          p("The two maps attempt to compare in/out of state tuition cost by state.
                            Users can click on the buttons on widgets in order to customize the 
                            type of school or degree length. Then two maps changes accordingly.")
                        ),
      
                        mainPanel(
                          plotOutput("map"),
                          plotOutput("secondMap")
                        )
                    )
      ),  
      
      #Correlation tab.
      tabPanel("Correlation",
          titlePanel("Correlation between tuition and career pay"),
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("chosenState")
                 ),
                mainPanel(
                  plotOutput("scatterplot"),
                  br(),
                  p("This graph illustrates the positive correlation between out-of-state tuition
                    and early career pay. Users can notice the general trend throughout the
                    graph. They can also choose the state they wish to focus on by interacting
                    with the widget.")
                )
               )
      ),
      #Diversity tab.
      tabPanel("Diversity",
               titlePanel("Diversity rate by race in Universities in 2014"),
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("diversityState")
                 ),
                 mainPanel(
                   plotOutput("barPlot"),
                   br(),
                   p("This bar graph demonstrates the diversity rate in 
                     Universities/Colleges in each state.
                     Users can specifically choose which state they would like to 
                     focus on.")
                 )
               )
      )
    )
))


