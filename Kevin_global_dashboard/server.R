
## Step 2: load in the data on the server
library(shiny)
library(tidyverse)
source("functions.R") # may have to set working directory!!
# Define a file name (to save the dataset as) that changes with date:
file.name <- paste0("global_data_", as.Date(Sys.time(), tz="Australia/Queensland"), ".csv")
# Define an if-else to either read in existing data, or 
# download and then read in data using the get_data from the functions.R script
if(file.exists(file.name)) {
    global_data <- read.csv(file.name)
} else {
    get_data(file.name) # download new copy of data
    global_data <- read.csv(file.name) # read data into R
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    # Step 3: Define the total number of cases worldwide!
	TotalWorldwide <- 
		global_data %>%
		filter(date == last_date) %>% # subset the data based on last available date
		pull(cumulative_confirmed) %>% # get only the confirmed cases col
		sum() # sum the cases together
	
    # Add to server output: Total COVID-19 worldwide
    output$TotalWorldwide <- renderText({ TotalWorldwide })
    
    
    
    output$distPlot <- renderPlot({
        
        # Step 6: begin to make a ggplot of the data input
        # Aiming to make a graph like this one:
        # https://ourworldindata.org/grapher/covid-confirmed-deaths-since-5th-death
        
        # calculate cumulative number per capita
        global_data <- global_data %>%
            mutate(cumulative_deaths_pc = 1e6*cumulative_deaths/pop_total)
        
        
        
        ggplt <- 
            global_data %>%
            ggplot(aes(x=days_since_first, y=cumulative_deaths,
                       color=country)) +
            theme_light() +
            geom_line() +
            theme(legend.position='none') +
            scale_y_log10(breaks = 10^(0:6),
                          limits = 10^input$log10_deaths) +
            labs(x = "Days since first reported death",
                 y = "Cumulative number of deaths")
        # ggplt
        
        # Extra:
        require(ggrepel)
        text_position <- global_data %>% 
            mutate(last_day = ifelse(date == last_date, 1, 0)) %>%
            filter(last_day == 1)
        # text_position <- text_position %>%
        #     filter(cumulative_deaths > quantile(text_position$cumulative_deaths, 0.75))
        text_position <- text_position %>%
            filter(days_since_first > quantile(text_position$days_since_first, 0.5)) %>%
            mutate(days_since_first = days_since_first + 1)
        
        ggplt + 
            ggrepel::geom_text_repel(
                data=text_position,
                aes(x=days_since_first, y = cumulative_deaths, color=country, label = country))
        # Step 7: add in slider input (^above)
        
        # Step 8: do the same plot, but using ggplotly (inefficient on server...)
        # no need for slider input now!
        # ggplotly(ggplt, tiptool = c("country", "cumulative_deaths", "date"),
        #          dynamicTicks=T) %>%
        #     layout(yaxis = list(type = "log"))
        
        ## comment this old stuff off when complete:
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$case.range + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
})
