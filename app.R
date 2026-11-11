#install.packages('dygraph')
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")


library(shiny)
library(bslib)
library(readxl)
#library(dygraphs)
library(ggplot2) 
library(tidyr)
#library(dplyr)
#library(ggiraph)
library(plotly)


#load availability data 
availability_data <- read_xlsx("data/data.xlsx", sheet = "availability")
long_availability_data <- pivot_longer(availability_data, cols=2:18, names_to = "Country", values_to = "availability")
long_availability_data[long_availability_data == 0] <- NA
long_availability_data <- na.omit(long_availability_data)
# # 
# #load consumption data 
consumption_data <- read_xlsx("data.xlsx", sheet = "consumption")
long_consumption_data <- pivot_longer(consumption_data, cols=2:18, names_to = "Country", values_to = "consumption")
long_consumption_data[long_consumption_data == 0] <- NA
long_consumption_data <- na.omit(long_consumption_data)
# 
# #load water stress data 
stress_data <- read_xlsx("data.xlsx", sheet = "stress")
long_stress_data <- pivot_longer(stress_data, cols=2:18, names_to = "Country", values_to = "stress")
long_stress_data[long_stress_data == 0] <- NA
long_stress_data <- na.omit(long_stress_data)
# 
# #load access data 
access_data <- read_xlsx("data.xlsx", sheet = "access")
long_access_data <- pivot_longer(access_data, cols=2:18, names_to = "Country", values_to = "access")
long_access_data[long_access_data == 0] <- NA
long_access_data <- na.omit(long_access_data)


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  title = "Water Security Under Climate Risk in Small Island Developing States (SIDS): Interactive Figures for Hydro-SMR Indicators",
  fillable = TRUE, 
  sidebar = sidebar(
    position = "right",
    
    checkboxGroupInput("countries",
                       label = tags$span(tags$strong("Select Countries to Display"), tags$em("\n")), 
                       choices = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"), 
                       selected = list("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana", "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago"),
                       inline = FALSE
    ),
    
  ),
  
  card(
    card_header("Introduction"),
    #height = "20vh",
    "The following interactive graphs correspond to Figures 3-6. To view data in more detail, please select the countries of interest from the menu on the right. All data is sourced from FAO AQUASTAT (2026).  "
  ),
  navset_card_underline(
    full_screen = TRUE,
    nav_panel(
      title = "Availability",
      plotlyOutput("availability_plot", width = "100%", height = "100%")
      
    ),
    nav_panel(
      title = "Consumption",
      plotlyOutput("consumption_plot", width = "100%", height = "100%")
      
    ),
    nav_panel(
      title = "Water Stress",
      plotlyOutput("water_stress_plot", width = "100%", height = "100%")
      
    ),
    
    nav_panel(
      title = "Access",
      plotlyOutput("access_plot", width = "100%", height = "100%")
    )
  )
  
  
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
 
  
  # #Configure availability plot 
  output$availability_plot <-renderPlotly({
    filtered_availability_data <- filter(long_availability_data, Country %in% c(input$countries))
    year_min <- min(filtered_availability_data$Year, na.rm =TRUE)
    year_max <- max(filtered_availability_data$Year, na.rm=TRUE)

    availability_plot<- ggplot(filtered_availability_data, aes(x=Year, y=availability, group = Country, color = Country, text = paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Availability: ", sprintf("%.2f",availability))))+
      geom_line()+
      labs(
        title = "Freshwater Availability in m<sup>3</sup>/capita",
        x = "Year",
        y = "Availability (m<sup>3</sup>/capita)",
        color = NULL
      )+
      xlim(min(filtered_availability_data$Year, na.rm =TRUE), max(filtered_availability_data$Year, na.rm =TRUE))+
      scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
      scale_y_continuous(expand = c(0, 0), n.breaks = 10) +

      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20, r = 20, l = 20),
        axis.title.x = element_text(margin = margin(b = 20)),
        plot.title = element_text(hjust = 0.5)
      )

    new_availability_plot <- plotly::ggplotly(availability_plot, tooltip = "text")
    new_availability_plot <-plotly::layout(
      new_availability_plot,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ),
      margin = list(b = 120)
    )



  })
  # 
  # #Configure consumption plot 
  output$consumption_plot <-renderPlotly({
    filtered_consumption_data <- filter(long_consumption_data, Country %in% c(input$countries))
    year_min <- min(filtered_consumption_data$Year, na.rm =TRUE)
    year_max <- max(filtered_consumption_data$Year, na.rm=TRUE)

    consumption_plot<- ggplot(filtered_consumption_data, aes(x=Year, y=consumption, group = Country, color = Country, text = paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Consumption: ", sprintf("%.2f",consumption))))+
      geom_line()+
      labs(
        title = "Water Consumption in m<sup>3</sup>/capita",
        x = "Year",
        y = "Consumption (m<sup>3</sup>/capita)",
        color = NULL
      )+
      xlim(min(filtered_consumption_data$Year, na.rm =TRUE), max(filtered_consumption_data$Year, na.rm =TRUE))+
      scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
      scale_y_continuous(expand = c(0, 0), n.breaks = 10) +

      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20, r = 20, l = 20),
        axis.title.x = element_text(margin = margin(b = 20)),
        plot.title = element_text(hjust = 0.5)
      )

    new_consumption_plot <- plotly::ggplotly(consumption_plot, tooltip = "text")
    new_consumption_plot <-plotly::layout(
      new_consumption_plot,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ),
      margin = list(b = 120)
    )



  })
  # #Configure water stress plot 
  output$water_stress_plot <-renderPlotly({
    filtered_stress_data <- filter(long_stress_data, Country %in% c(input$countries))
    year_min <- min(filtered_stress_data$Year, na.rm =TRUE)
    year_max <- max(filtered_stress_data$Year, na.rm=TRUE)

    stress_plot<- ggplot(filtered_stress_data, aes(x=Year, y=stress, group = Country, color = Country, text = paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Water Stress: ", sprintf("%.2f",stress))))+
      geom_line()+
      labs(
        title = "Water Stress",
        x = "Year",
        y = "Water Stress (%)",
        color = NULL
      )+
      xlim(min(filtered_stress_data$Year, na.rm =TRUE), max(filtered_stress_data$Year, na.rm =TRUE))+
      scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
      scale_y_continuous(expand = c(0, 0), n.breaks = 10) +

      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20, r = 20, l = 20),
        axis.title.x = element_text(margin = margin(b = 20)),
        plot.title = element_text(hjust = 0.5)
      )

    new_stress_plot <- plotly::ggplotly(stress_plot, tooltip = "text")
    new_stress_plot <-plotly::layout(
      new_stress_plot,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ),
      margin = list(b = 120)
    )



  })
  # 
  # #Configure water access plot 
  output$access_plot <-renderPlotly({
    filtered_access_data <- filter(long_access_data, Country %in% c(input$countries))
    year_min <- min(filtered_access_data$Year, na.rm =TRUE)
    year_max <- max(filtered_access_data$Year, na.rm=TRUE)

    access_plot<- ggplot(filtered_access_data, aes(x=Year, y=access, group = Country, color = Country, text = paste("Country: ", Country,"<br>", "Year: ", Year, "<br>",  "Access: ", sprintf("%.2f",access))))+
      geom_line()+
      labs(
        title = "Water Access",
        x = "Year",
        y = "Water Access (%)",
        color = NULL
      )+
      xlim(min(filtered_access_data$Year, na.rm =TRUE), max(filtered_access_data$Year, na.rm =TRUE))+
      scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
      scale_y_continuous(expand = c(0, 0), n.breaks = 10) +

      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20, r = 20, l = 20),
        axis.title.x = element_text(margin = margin(b = 20)),
        plot.title = element_text(hjust = 0.5)
      )

    new_access_plot <- plotly::ggplotly(access_plot, tooltip = "text")
    new_access_plot <-plotly::layout(
      new_access_plot,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ),
      margin = list(b = 120)
    )



  })

}

shinyApp(ui = ui, server = server) 


