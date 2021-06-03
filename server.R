
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)


tuitionData <- read.csv("tuition_cost.csv")
diversityData <- read.csv("diversity_school.csv") 
salaryData <- read.csv("salary_potential.csv")


stateMaps <- map_data("state")


#diversity <- diversityData %>% 
    #pivot_wider(
               # names_from = category, 
               # values_from = enrollment, values_fn = list(enrollment = mean)) %>% 
    #mutate(Women = Women/total_enrollment*100,
            #`American Indian / Alaska Native` = `American Indian / Alaska Native` / total_enrollment * 100,
          # Asian = Asian/total_enrollment * 100,
          # Black = Black/total_enrollment * 100,
          # Hispanic = Hispanic/total_enrollment * 100,
          # `Native Hawaiian / Pacific Islander` =  `Native Hawaiian / Pacific Islander` / total_enrollment * 100,
          # White = White/total_enrollment * 100,
          # `Two Or More Races` = `Two Or More Races` / total_enrollment * 100,
          # Unknown = Unknown/total_enrollment * 100,
          # `Non-Resident Foreign` = `Non-Resident Foreign`/total_enrollment * 100,
          # `Total Minority` = `Total Minority`/total_enrollment*100)
    


diversityData <- diversityData %>% 
   mutate(enrollment = enrollment/total_enrollment * 100)




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
   
    
    sample <- reactive({
        if(is.null(input$degree)) {
            tuitionSelected<- tuition %>% 
                filter(type %in% input$type)
        } else {
            tuitionSelected <- tuition %>% 
                filter(degree_length %in% input$degree) %>% 
                filter(type %in% input$type)
        }
        
         left_join(stateMaps, tuitionSelected,by=c(region = "state"))
        
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
            geom_polygon(aes(fill= in_state_tuition, group = group), col = "black", na.value = "black") +
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
        if(input$state == "all"){
            correlationData
        } else {
          correlationData %>% 
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
                                          "Wyoming" = "wyoming", "All" = "all"), selected = "all")
    
    })
    
    
    
    
    
    
    
    
    output$diversityState <- renderUI({
        radioButtons("diversity", label = ("Choose the State"),
                 choices = list("Alabama" = "Alabama", "Alaska" = "Alaska",
                                "Arizona" = "Arizona", "Arkansas" = "Arkansas",
                                "California" = "California", "Colorado" = "Colorado",
                                "Connecticut" = "Connecticut", "Delaware" = "Delaware",
                                "Florida" = "Florida", "Georgia" = "Georgia",
                                "Hawaii" = "Hawaii", "Iadho" = "Idaho",
                                "Illinois" = "Illinois", "Indiana" = "Indiana",
                                "Iowa" = "Iowa", "Kansas" = "Kansas",
                                "Kentucky" = "Kentucky", "Louisiana" = "Louisiana",
                                "Maine"  = "Maine", "Marylane"= "Maryland",
                                "Massachusetts" = "Massachusetts",
                                "Michigan" = "Michigan", "Minnesota" = "Minnesota",
                                "Mississippi" = "Mississippi", "Missouri" = "Missouri",
                                "Montana" = "Montana", "Nebraska" = "Nebraska", 
                                "Nevada" = "Nevada", "New Hampshire" = "New Hampshire",
                                "New Jersey" = "New Jersey", "New Mexico" = "New Mexico",
                                "New York" = "New York", "North Carolina"="North Carolina",
                                "North Dakota" = "North Dakota", "Ohio" = "Ohio",
                                "Oklahoma" = "Oklahoma", "Oregon" = "Oregon",
                                "Pennsylvania" = "Pennsylvania", "Rhode-island" = "Rhode Island",
                                "South Carolina" = "South Carolina", "South Dakota" = "South dakota", 
                                "Tennessee" = "Tennessee", "Texas" = "Texas", 
                                "Utah" = "Utah", "Vermont" = "Vermont", 
                                "Virginia" = "Virginia", "Washington" = "Washington", 
                                "West Virginia" = "West Virginia", "Wisconsin" = "Wisconsin", 
                                "Wyoming" = "Wyoming"), selected = "Washington")
    })
    

    
    diData <- reactive({
        diversityData %>% 
            filter(state %in% input$diversity) %>% 
            group_by(category) %>% 
            summarize(enrollment = mean(enrollment)) 
    })
    
    
  

    
    output$barPlot <- renderPlot({
        ggplot(data = diData())+
            geom_bar(mapping = aes(x= category, y = enrollment), stat = "identity")+
            labs(x = "Races", y = "Percentage of Enrollment", col = "blue", 
                 title = "Percentage of Enrollment for each races") 
    })
    
    
    
    
    
    
    
    
    
    
    
})









 