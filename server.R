
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)


tuitionData <- read.csv("tuition_cost.csv")
diversityData <- read.csv("diversity_school.csv") 

stateMaps <- map_data("state")



tuition <- tuitionData %>% 
    select(name, state, type, degree_length, in_state_tuition, out_of_state_tuition)




shinyServer(function(input, output) {
   
    tuitionMap <- left_join(stateMaps, 
                             tuition,
                             by=c(region = "state"))
    
    output$map <- renderPlot({
        ggplot(tuitionMap, aes(long, lat)) +
            geom_polygon(aes(fill= in_state_tuition, group = group), col = "black") +
            coord_quickmap()
    })
})
