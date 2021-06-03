
# Calling shiny library to enable shiny app function
library(shiny)


# shinyUI that draws a widget and panels on the scree of the app.
shinyUI(fluidPage(
    tabsetPanel(
      #Introduction tab
      tabPanel("Introduction",
               titlePanel("Choosing the Right State for College/University"),
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
          titlePanel("Correlation Between Tuition and Career Pay"),
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
               titlePanel("Diversity Rate by Race in Universities in 2014"),
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
        
      ),
      
      #Conclusion Tab
      tabPanel("Conclusion",
               titlePanel("Conclusion"),
               mainPanel(
                 br(),
                 br(),
                 p("The most notable insight discovered in our project was the revelation of 
                 a", strong("positive"), "correlation between out-of-state tuition and estimated early career pay 
                 for colleges and universities in the United States. The trend appears to be that 
                 the more expensive out-of-state tuition is, the higher the estimated early career earnings. 
                 We believe that this information will be useful for our target audience because 
                 it allows them to see the value in their investment of tuition. 
                 This also implies that people with lower financial status may end up 
                 having a lower career pay compared to those who have a higher financial status 
                 before entering college/university. 
                 This systematic defect in higher education in the U.S. must be addressed. 
                 The graph with the trendline below illustrates the positive correlation."),
                 br(),
                 img(src = "correlation.jpeg", height = 300, width = 500),
                 br(),
                 br(),
                 p("While our datasets were sufficient for our analysis, 
                 we were not able to form a complete picture because our datasets 
                 did not include every single college and university in the United States. 
                 Our tuition data included the most schools, 
                 but our early career pay dataset and diversity dataset had fewer. 
                 It is possible that the addition of every single college and university 
                 in the United States would have skewed our results, 
                 but this is unlikely given the amount of data we did have to analyze. 
                 In the future, this project could be advanced by using datasets that 
                 cover all of the schools in the United States. 
                 We could also provide additional useful information by determining 
                 if there is a correlation between in-state tuition and early career earnings."
                 )
               ))
    )
))


