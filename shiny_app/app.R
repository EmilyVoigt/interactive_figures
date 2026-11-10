library(shiny)
library(bslib)
library(readxl)
library(dygraphs)
library(ggplot2) 
library(tidyr)
library(dplyr)
library(ggiraph)
library(plotly)

setwd("C:/Users/evoig/Desktop/Grad School!!/Spring 2026/GRA/Caribbean Hydro-SMR/Interactive Figures/Shiny_app")
data <- read_xlsx("Consumption.xlsx", sheet = "plot")
long_data <- pivot_longer(data, cols=2:18, names_to = "Country", values_to = "consumption")
long_data[long_data == 0] <- NA
long_data <- na.omit(long_data)
long_data

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  title = "Technical Brief interactive Figures",
  
  sidebar = sidebar(
    
    checkboxGroupInput("countries",
                       label = "Select Countries to Display on Graphs", 
                       choices = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"), 
                       selected = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"),
                       inline = FALSE
                      ),
  
  ),
  card(
    card_header("Introduction"),
    "The following interactive graphs correspond to Figures X to X in the article X. To view data in more detail, please select the countries of interest from the menu on the left. "
  ),
  card(
    
    card_header("Consumption"),
    "Consumption plot", 
    
    plotlyOutput("consumption_plot", width = "90vw", height = "90vh"),
    
  ),
  card(
    card_header("Access"),
    "Access plot"
  ),
  card(
    card_header("Availability"),
    "Availability plot"
  ),
  card(
    card_header("Water Stress"),
    "Water Stress plot"
  )
  

)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  
  output$consumption_plot <-renderPlotly({
    filtered_data <- filter(long_data, Country %in% c(input$countries))
    year_min <- min(filtered_data$Year, na.rm =TRUE)
    year_max <- max(filtered_data$Year, na.rm=TRUE)
    #print(filtered_data, n=1000)
    print(year_min)
    #print("Year min is: ", year_min)
    #print("Year max is: ", year_max)
    consumption_plot<- ggplot(filtered_data, aes(x=Year, y=consumption, group = Country, color = Country, text = paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Consumption: ", sprintf("%.2f",consumption))))+
     geom_line()+
     labs(
       title = "Water Consumption in m^3 per capita", 
       x = "Year", 
       y = "Consumption (m3/capita)"
     )+
      xlim(min(filtered_data$Year, na.rm =TRUE), max(filtered_data$Year, na.rm =TRUE))+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
        
      theme(
        legend.position = "bottom"
        
      )
  
    ggplotly(consumption_plot, height = 500, width = 800, tooltip = "text")
   
     
   
  })
    
}

shinyApp(ui = ui, server = server) 


