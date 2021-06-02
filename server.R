
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
    
    
    
    
    
    
    
    
    
    correlationData <- left_join(salaryReform, tuitionReform, 
                                 by=c(name = "name", state = "state"))

    
    chosenData <- reactive({
        if(is.null(input$text)) {
            correlationData %>% 
                filter(state %in% input$state)
        } else {
            correlationData %>% 
                filter(name %in% input$text) %>% 
                filter(state %in% input$state)
        }
    })
    
    output$scatterplot <- renderPlot({
        ggplot(data = chosenData())+
            geom_point(mapping=aes(x= out_of_state_tuition, y = early_career_pay, col = state))+
        labs(x = "Out of State Tuition", y = "Early Career Pay", col = "State", 
             title = "Tuition versus Early Career Pay in U.S. Colleges/Universities") 
    })
    
    
    
    output$nameText <- renderUI({
        textInput(inputId = "text", label = ("Search the school by name"), value = "Enter text...")
    })
    
    output$chosenState <- renderUI({
        checkboxGroupInput(inputId = "state", label = "Which State?",
                           choices = list("Alabama" = "alabama", "Alaska" = "alaksa",
                                          "Arizona" = "arizona", "Arkansas" = "arkansas",
                                          "California" = "california", "Colorado" = "colorado",
                                          "Connecticut" = "connecticut", "Delaware" = "delaware",
                                          "Florida" = "florida", "Georgia" = "georgia",
                                          "Hawaii" = "hawaii", "Iadho" = "idaho",
                                          "Illinois" = "illinois", "Indiana" = "indiana",
                                          "Iowa" = "iowa", "Kansas" = "kansas",
                                          "Kentucky" = "kentucky", "Louisiana" = "louisiana",
                                          "Maine"  = "maine", "Marylane"= "maryland",
                                          "Massachusetts" = "massachusetts",
                                          "Michigan" = "michigan", "Minnesota" = "minnesota",
                                          "Mississippi" = "mississippi", "Missouri" = "missouri",
                                          "Montana" = "montana", "Nebraska" = "nebraska", 
                                          "Nevada" = "nevada", "New Hampshire" = "new-hampshire",
                                          "New Jersey" = "new-jersey", "New Mexico" = "new-mexico",
                                          "New York" = "new-york", "North Carolina"="north-carolina",
                                          "North Dakota" = "north-dakota", "Ohio" = "ohio",
                                          "Oklahoma" = "oklahoma", "Oregon" = "oregon",
                                          "Pennsylvania" = "pennsylvania", "Rhode-island" = "rhode-island",
                                          "South Carolina" = "south-carolina", "South Dakota" = "south-dakota", 
                                          "Tennessee" = "tennessee", "Texas" = "texas", 
                                          "Utah" = "utah", "Vermont" = "vermont", 
                                          "Virginia" = "virginia", "Washington" = "washington", 
                                          "West Virginia" = "west-virginia", "Wisconsin" = "wisconsin", 
                                          "Wyoming" = "wyoming"), selected = "washington")
    
    })
})




#1. state disappearing in map in tab 1
#2. pivot wider issue 
#3. scatter plot not showing issue 
#4. 
 