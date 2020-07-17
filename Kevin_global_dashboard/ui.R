
# Step 1: define a UI
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("My COVID-19 Global Dashboard"),
    
    # Sidebar with a slider input for number of bins
    sidebarPanel(
        
        # Step 4: Add to server UI the total number of cases worldwide
        h3("Total number of COVID-19 cases worldwide:"),
        span(textOutput("TotalWorldwide"), 
                   style="font-size: 20px"), # can add html text options
                    # font-weight: bold"), # can also add bold text
        
        # Step 5: begin modifying the slider input; 
        # add two values, change 'bins' to 'max_deaths',
        # change label
        sliderInput("log10_deaths",
                    label = h4("y-axis range (log10-scale):"),
                    min = 0,
                    max = 6,
                    step = 0.1,
                    value = c(0,5.1)) # go up to 1 million deaths
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot")
    )
))
