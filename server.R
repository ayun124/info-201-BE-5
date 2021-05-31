
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)


tuitionData <- read.csv("tuition_cost.csv")
diversityData <- read.csv("diversity_school.csv") 

stateMaps <- map_data("state")



tuition <- tuitionData %>% 
    select(name, state, type, degree_length, in_state_tuition, out_of_state_tuition) %>% 
    mutate(state = tolower(state))



shinyServer(function(input, output) {
   
    tuitionMap <- left_join(stateMaps, 
                             tuition,
                             by=c(region = "state"))
    

    sample <- reactive({
        if(is.null(input$degree)) {
            tuitionMap %>% 
                filter(type %in% input$type)
        } else {
            tuitionMap %>% 
                filter(degree_length %in% input$degree) %>% 
                filter(type %in% input$type)
        }
    })
    
    
    
    
    
    output$degreeLength <- renderUI({
        checkboxGroupInput(inputId = "degree", label = "Which Degree Length?",
                           choices = list("2 Years" = "2 Year", "4 Years" = "4 Year"),
                           selected = "2 Year") 
    })
    
   
    output$typeSchool <- renderUI({
        selectInput(inputId = "type", label = "Which type of School?",
                    choices = list("Public" = "Public", 
                                   "Private" = "Private",
                                   "For Profit" = "For Profit"),
                    selected = "Public")
    })
    
    
    
    
    
    output$map <- renderPlot({
        ggplot(sample(), aes(long, lat)) +
            geom_polygon(aes(fill= in_state_tuition, group = group), col = "black") +
            coord_quickmap() +
            labs(title = "InState Tuition Cost")+
            scale_fill_continuous(limits = c(480,60000), low = "white", high = "blue")
            
            
    })
    
    
    output$secondMap <- renderPlot({
        ggplot(sample(), aes(long, lat)) +
            geom_polygon(aes(fill= out_of_state_tuition, group = group), col = "black") +
            coord_quickmap() +
            labs(title = "Out of State Tuition Cost")+
            scale_fill_continuous(limits = c(480,60000), low = "white", high = "blue")
            

    })
    
    
    
})


tuitionMap %>% 
    select(out_of_state_tuition) %>% 
    arrange(out_of_state_tuition) %>% 
    head(5)
 