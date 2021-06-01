
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)


tuitionData <- read.csv("tuition_cost.csv")
diversityData <- read.csv("diversity_school.csv") 
salaryData <- read.csv("salary_potential.csv")


stateMaps <- map_data("state")



tuition <- tuitionData %>% 
    select(name, state, type, degree_length, in_state_tuition, out_of_state_tuition) %>% 
    mutate(state = tolower(state))


tuitionReform <- tuition %>% 
    select(name,state,in_state_tuition, out_of_state_tuition)


salaryReform <- salaryData %>% 
    select(name,state_name,early_career_pay, mid_career_pay) %>% 
    rename(state = state_name) %>% 
    mutate(state = tolower(state))



correlationData <- left_join(tuitionReform, salaryReform, by=c(name = "name", state = "state"), na.rm = TRUE)


    


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





 