library(shiny)
library(lubridate)
library(tidyverse)

theme_set(theme_minimal())

ridership = read_csv("CTA_Ridership.csv") %>% 
    mutate(service_date = mdy(service_date))


ui <- fluidPage(

    titlePanel("CTA Ridership Data"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("year", "Pick a year:", min = 2001, max = 2020, value = 2020)
        ),
        mainPanel(
            plotOutput("RidershipOverTime", hover = hoverOpts("year_hover")),
            "Number of Riders: ",
            textOutput("yearHoverOutput"),
        )
    )
)

server <- function(input, output) {
    
    ridership_by_month_year = ridership %>%
        mutate(service_month = month(service_date),
               service_year = factor(year(service_date))) %>% 
        group_by(service_month, service_year) %>% 
        summarise(total_rides = mean(total_rides))
    
    output$yearHoverOutput = renderText({
        if(is.null(input$year_hover$y)) {
            return(NULL)
        } else {
            return(as.character(round(input$year_hover$y)))
        }

    })
    
    output$RidershipOverTime = renderPlot({
        annotate_data = ridership_by_month_year %>% 
            filter(service_year == input$year)
        
        ggplot(ridership_by_month_year, aes(x = service_month, y = total_rides, group=service_year)) +
            geom_line(color="#CCCCCC") +
            geom_line(data = annotate_data, color = "firebrick2", size=2) +
            guides(color = FALSE) +
            ylim(0, 2000000)
    })
}

shinyApp(ui = ui, server = server)
