#
# This is a KK demo Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)

location_DF <- read.csv('Dummy_Data.csv')
vendor_consol_DF <- subset(location_DF, Location_Type == 'Vendor' | Location_Type == 'Consol')


#vendor_consol_DF$Long <- ifelse(vendor_consol_DF$Long>0, vendor_consol_DF$Long*-1, vendor_consol_DF$Long)

consols_DF <- subset(location_DF,Location_Type == 'Consol')

consol_name <-  consols_DF$Name

vendors_filtered_DF <- vendor_consol_DF


# selected_consol <- subset(consols_DF,Name == input$selected_consol)
# # calculate distance (as the crow flies) between google lat/lon and TM lat/lon
# vendors_filtered_DF$distance <- crow_distance_between_nodes(selected_consol$Lat, selected_consol$Long, vendors_filtered_DF$Lat, vendors_filtered_DF$Long)



# shortcut (proxy) method for distance between locations
deg_2_rad <- function(deg) {
  # Faster than calling routing service 
  # input = degrees, output = radians
  return((deg * pi) / 180)
}

crow_distance_between_nodes <- function(start_lat, start_lon, end_lat, end_lon) {
  
  # find distance
  inner_p1 <- cos(deg_2_rad(start_lat))
  inner_p2 <- cos(deg_2_rad(end_lat))
  inner_p3 <- sin(deg_2_rad(start_lat))
  inner_p4 <- sin(deg_2_rad(end_lat)) 
  inner_p5 <- cos(deg_2_rad(end_lon-start_lon))
  
  inner <- (inner_p1 * inner_p2) + (inner_p3 * inner_p4) * inner_p5
  
  return(acos(inner) * 3958.786)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("They-pay GM Vendors nearby Target consolidators"),

   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(2, selectInput('selected_consol', 'Target GM Consolidators',consol_name,selected='SOCS_Consol_Ellenwood_GA',selectize = TRUE) 
    ),
      column (10, sidebarPanel(
         sliderInput("radius",
                     "Miles away from consolidator (Radius):",
                     min = 0,
                     max = 1000,
                     value = 300,
                     step = 10
                     )
      )),
    column(4,  downloadButton('downloadData', 'Download data'))),
       fluidRow(
      # Show a plot of the generated distribution
      mainPanel(
        br(),
        br(),
        br(),
        leafletOutput("mymap"),
        br(),
        br(),
        textOutput("no_of_vendors"),
        br(),
        br(),
        DT::dataTableOutput("vendor_data")
      )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  pal <- colorFactor(c("navy", "red"), domain = c("Consol", "Vendor"))
  
  output$mymap <- renderLeaflet({
    
    selected_consol <- subset(consols_DF,Name == input$selected_consol)
    vendors_filtered_DF$distance <- crow_distance_between_nodes(selected_consol$Lat, selected_consol$Long, vendors_filtered_DF$Lat, vendors_filtered_DF$Long)
    vendors_filtered_DF <- filter(vendors_filtered_DF, vendors_filtered_DF$distance < input$radius)
    
  leaflet(data=vendors_filtered_DF) %>% addTiles() %>%
    addCircleMarkers(
      color = ~pal(vendors_filtered_DF$Location_Type),
      radius = ~ifelse(vendors_filtered_DF$Name == input$selected_consol,input$radius/2,5),
      stroke = FALSE, fillOpacity = ~ifelse(vendors_filtered_DF$Location_Type == 'Consol',0.4,4)
    )
  })
  
  output$vendor_data = DT::renderDataTable({
    
    selected_consol <- subset(consols_DF,Name == input$selected_consol)
    vendors_filtered_DF$distance <- crow_distance_between_nodes(selected_consol$Lat, selected_consol$Long, vendors_filtered_DF$Lat, vendors_filtered_DF$Long)
    vendors_filtered_DF <- filter(vendors_filtered_DF, vendors_filtered_DF$distance < input$radius)

    vendors_filtered_DF$distance <- round(vendors_filtered_DF$distance,2)
    vendors_filtered_DF$Lat <- round(vendors_filtered_DF$Lat,2)
    vendors_filtered_DF$Long <- round(vendors_filtered_DF$Long,2)
    
    # sort data by longest error distance to shortest
    vendors_filtered_DF %>% arrange((distance))
    
  })
  
  output$no_of_vendors <- renderText({
    selected_consol <- subset(consols_DF,Name == input$selected_consol)
    vendors_filtered_DF$distance <- crow_distance_between_nodes(selected_consol$Lat, selected_consol$Long, vendors_filtered_DF$Lat, vendors_filtered_DF$Long)
    vendors_filtered_DF <- filter(vendors_filtered_DF, vendors_filtered_DF$distance < input$radius)
    c('Number of Vendors available at this radius: ',nrow(vendors_filtered_DF)-1)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      selected_consol <- subset(consols_DF,Name == input$selected_consol)
      vendors_filtered_DF$distance <- crow_distance_between_nodes(selected_consol$Lat, selected_consol$Long, vendors_filtered_DF$Lat, vendors_filtered_DF$Long)
      vendors_filtered_DF <- filter(vendors_filtered_DF, vendors_filtered_DF$distance < input$radius)
      write.csv(vendors_filtered_DF, file)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

