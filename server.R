

# Calling Libraries to bring installed functions
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)



# Reading three csv files data
tuitionData <- read.csv("tuition_cost.csv")
diversityData <- read.csv("diversity_school.csv") 
salaryData <- read.csv("salary_potential.csv")



# Data preparation -------------------------------------------------------------
# Creating a stateMap data that will be used when 
# joining with other data to create a U.S. map.
stateMaps <- map_data("state")


# Mutating diversityData to have a percentage of enrollment
# for each race, not the number of enrollment.
diversityData <- diversityData %>% 
   mutate(enrollment = enrollment/total_enrollment * 100)


# Creating a tuition data by filter out only the data needed
# from tuitionData data. 
tuition <- tuitionData %>% 
    select(name, state, type, degree_length, in_state_tuition, out_of_state_tuition) %>% 
    mutate(state = tolower(state))


# Reforming the tuition data that will be used for correlation data.
tuitionReform <- tuition %>% 
    select(name,state,in_state_tuition, out_of_state_tuition)

# Reforming the salary data that will be used for correlation data
salaryReform <- salaryData %>% 
    select(name,state_name,early_career_pay, mid_career_pay) %>% 
    rename(state = state_name) %>% 
    mutate(state = tolower(state))




# Changing the name of several race in the diversityData data
# Since it is too long. 
rownumbers <- which(diversityData$category == "Non-Resident Foreign")
diversityData$category[rownumbers] <- "Foreign"

rownumbers1 <- which(diversityData$category == "American Indian / Alaska Native")
diversityData$category[rownumbers1] <- "Natives"

rownumbers2 <- which(diversityData$category == "Native Hawaiian / Pacific Islander")
diversityData$category[rownumbers2] <- "Islanders"





#-------------------------------------------------------------------------------

# Start of the Shinny server 
shinyServer(function(input, output) {
   
# Tuition Panel Section --------------------------------------------------------
    
    # Creating a sample data that is reactive to the widgets
    sample <- reactive({
        if(is.null(input$degree)) {
            tuitionSelected<- tuition %>% 
                filter(type %in% input$type) %>% 
                group_by(state) %>% 
                summarize(in_state_tuition = mean(in_state_tuition),
                          out_of_state_tuition = mean(out_of_state_tuition))

        } else {
            tuitionSelected <- tuition %>% 
                filter(degree_length %in% input$degree) %>% 
                filter(type %in% input$type) %>% 
                group_by(state) %>% 
                summarize(in_state_tuition = mean(in_state_tuition),
                          out_of_state_tuition = mean(out_of_state_tuition))
        }
         left_join(stateMaps, tuitionSelected, by=c(region = "state"))
    })
    
    
    # UI widget code for check box - Degree Length
    output$degreeLength <- renderUI({
        checkboxGroupInput(inputId = "degree", label = "Which Degree Length?",
                           choices = list("2 Years" = "2 Year", "4 Years" = "4 Year"),
                           selected = "2 Year") 
    })
    
    # UI widget code for drop down menu - Type of School
    output$typeSchool <- renderUI({
        selectInput(inputId = "type", label = "Which type of School?",
                    choices = list("Public" = "Public", 
                                   "Private" = "Private",
                                   "For Profit" = "For Profit"),
                    selected = "Public")
    })
    
    # Map code for creating a U.S. map. 
    
    # In-state tuition map.
    output$map <- renderPlot({
        ggplot(sample(), aes(long, lat)) +
            geom_polygon(aes(fill= in_state_tuition, group = group), 
                         col = "black", na.value = "black") +
            coord_quickmap() +
            labs(title = "InState Tuition Cost")+
            scale_fill_continuous(limits = c(480,60000),
                                  low = "white", high = "blue")
    })
    
    #Out-of-state tuition map.
    output$secondMap <- renderPlot({
        ggplot(sample(), aes(long, lat)) +
            geom_polygon(aes(fill= out_of_state_tuition, group = group), 
                         col = "black") +
            coord_quickmap() +
            labs(title = "Out of State Tuition Cost")+
            scale_fill_continuous(limits = c(480,60000), 
                                  low = "white", high = "blue")
    })
    
    
 
    
    
#Correlation Panel Section -------------------------------------------------------------
    
    # Creating a correlation Data by combining salary and tuition data that are 
    # reformed for joining. 
    correlationData <- left_join(salaryReform, tuitionReform, 
                                 by=c(name = "name", state = "state"))
    
    
    # Creating a reactive data that is reactive to the state chosen
    chosenData <- reactive({
        if(any(input$state == "all")){
           selectedCorrelation <-  correlationData
        } else {
            selectedCorrelation <- correlationData %>% 
                                     filter(state %in% input$state)
        }
    })
    
    
    # Drawing a scatterplot that shows the general trend of tuition vs career pay.
    output$scatterplot <- renderPlot({
        ggplot(data = chosenData())+
            geom_point(mapping=aes(x = out_of_state_tuition, y = early_career_pay, col = state), 
                       na.rm = TRUE)+
        labs(x = "Out of State Tuition", y = "Early Career Pay", col = "State", 
             title = "Tuition versus Early Career Pay in U.S. Colleges/Universities") 
    })
    
    
    # UI widget for letting users check boxes for the state they would like to see.
    output$chosenState <- renderUI({
        checkboxGroupInput(inputId = "state", label = "Which State?",
                 choices = list("All" = "all", "Alabama" = "alabama", "Alaska" = "alaksa",
                                "Arizona" = "arizona", "Arkansas" = "arkansas",
                                "California" = "california", "Colorado" = "colorado",
                                "Connecticut" = "connecticut", "Delaware" = "delaware",
                                "Florida" = "florida", "Georgia" = "georgia",
                                "Hawaii" = "hawaii", "Iadho" = "idaho",
                                "Illinois" = "illinois", "Indiana" = "indiana",
                                "Iowa" = "iowa", "Kansas" = "kansas",
                                "Kentucky" = "kentucky", "Louisiana" = "louisiana",
                                "Maine"  = "maine", "Maryland"= "maryland",
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
                                "Wyoming" = "wyoming"), selected = "all")
    
    })
    
    
#Diversity Panel ---------------------------------------------------------------------------------------
    
    # UI Widget for radio buttons.
    # Users can choose the state to see the rate of race diversity in universities in average.
    output$diversityState <- renderUI({
        radioButtons("diversity", label = ("Choose the State"),
                 choices = list("Alabama" = "Alabama", "Alaska" = "Alaska",
                                "Arizona" = "Arizona", "Arkansas" = "Arkansas",
                                "California" = "California", "Colorado" = "Colorado",
                                "Connecticut" = "Connecticut", "Delaware" = "Delaware",
                                "Florida" = "Florida", "Georgia" = "Georgia",
                                "Hawaii" = "Hawaii", "Idaho" = "Idaho",
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
                                "South Carolina" = "South Carolina", "South Dakota" = "South Dakota", 
                                "Tennessee" = "Tennessee", "Texas" = "Texas", 
                                "Utah" = "Utah", "Vermont" = "Vermont", 
                                "Virginia" = "Virginia", "Washington" = "Washington", 
                                "West Virginia" = "West Virginia", "Wisconsin" = "Wisconsin", 
                                "Wyoming" = "Wyoming"), selected = "Washington")
    })
    
    
    # Creating a reactive data that reacts to the radio button widget.
    # Therefore, changing the plot simultaneously, making the plot interactive.
    diData <- reactive({
        diversityData %>% 
            filter(state %in% input$diversity) %>% 
            group_by(category) %>% 
            summarize(enrollment = mean(enrollment)) 
    })
    
    
    # Creating a bar graph that shows the percentage of enrollment for each race type.
    output$barPlot <- renderPlot({
        ggplot(data = diData())+
            geom_bar(mapping = aes(x= category, y = enrollment), stat = "identity")+
            labs(x = "Races", y = "Percentage of Enrollment", col = "blue", 
                 title = "Diversity Rate in Colleges/Universities by state") 
    })
})




 