library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(zoo)

months<-readRDS("months.Rds")
ui <- dashboardPage(
  dashboardHeader(title = "Wesley and the Housing Market", titleWidth = "50%"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Map",
      tabName = "map",
      icon = icon("globe")
    ),
    menuItem(
      "Plots",
      tabName = "plots",
      icon = icon("globe")
    ),
    radioButtons(
      "Type",
      "Property Type",
      c(
        "Detatched" = "D",
        "Semidetatched" = "S",
        "Terraced" = "T",
        "Flats/Maisonettes" = "F",
        "Other" = "O"#,
       # "All (will be slow)" = "A"
      )
    ),
    radioButtons(
      "OldNew",
      "Age of Property",
      c(
        "Established" = "Y",
        "New Build" = "N"#,
       # "Any" = "A"
      )
    ),
    radioButtons(
      "Duration",
      "Ownership Duration",
      c(
        "Freehold" = "F",
        "Leasehold" = "L"#,
       # "Any" = "A"
      )
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "plots",
      sliderTextInput(
        inputId = "range", label = "Month", width = "100%",
        choices = months, 
        selected = months[c(4,50)]
      ),
      radioButtons(
        "Calc",
        "Select what to plot:",
        c(
          "mean" = "mean",
          "median" = "median",
          "number" = "number"
        )
      ),
      plotOutput("scatterPlot", height = 300)
    ),
   
     tabItem(
      tabName = "map",
      
      
      sliderTextInput(
        inputId = "test", label = "Month", width = "100%",
        choices = months, 
        selected = months[4]
      ),

     
    
      
      
      

      box(
        title = "sold prices",
        collapsible = TRUE,
        width = "100%",
        height = "100%",
        leafletOutput("map")
      )
    )
  ))
)



server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      
      addTiles()  %>%
      
      setView(lat = 52.1386394,
              lng = -0.4667782 ,
              zoom = 8) %>%
      addSearchOSM()
    
  })

  datamap1 <- reactive({
    Dates <- input$test
    datamap1 <- readRDS(paste(Dates, ".Rds", sep = ""))
    
    if (input$Type == "T") {
      datamap1 <- subset(datamap1, type == "T")
    } else if (input$Type == "D") {
      datamap1 <- subset(datamap1, type == "D")
    } else if (input$Type == "F") {
      datamap1 <- subset(datamap1, type == "F")
    } else if (input$Type == "S") {
      datamap1 <- subset(datamap1, type == "S")
    } else if (input$Type == "O") {
      datamap1 <- subset(datamap1, type == "O")
    } else if (input$Type == "A") {
      datamap1 <- datamap1
    }
    
    if (input$OldNew == "y") {
      datamap1 <- subset(datamap1, newbuild == "Y")
      
    } else if (input$OldNew == "M") {
      datamap1 <- subset(datamap1, newbuild == "N")
      
    } else if (input$OldNew == "N") {
      datamap1 <- datamap1
    }
    
    if (input$Duration == "L") {
      datamap1 <- subset(datamap1, duration == "L")
      
    } else if (input$Duration == "F") {
      datamap1 <- subset(datamap1, duration == "F")
      
    } else if (input$Duration == "A") {
      datamap1 <- datamap1
    }
  
    datamap1$price <- datamap1$price / 1000
    datamap1
  })
  
  Agg<-reactive({
    agg<-readRDS("AggregatedCalcData.Rds")
    agg<-subset(agg, type == input$Type)
    agg<-subset(agg, newbuild == input$OldNew)
    agg<-subset(agg, duration == input$Duration)
    agg
  })
  
  
  output$scatterPlot <- renderPlot({
    data<-Agg()
    data$date<-as.yearmon(data$date)
    data<-subset(data, date > input$range[1] & date < input$range[2])
    if (input$Calc=="mean"){
    y <- data$average
    } else if (input$Calc=="median"){
      y <-data$median
    } else if (input$Calc=="number"){
      y<-data$number
    }
    
    x <- data$date
    
   # x <- as.POSIXct(paste0(as.character(Agg()[,"date"]),"-01"), format = "%Y-%m-%d")
    plot(x, y)
  })
  
  observe({
    # Create a color palette with handmade bins.
    mybins = seq(min(datamap1()$price), max(datamap1()$price), by = 10000)
    mybins <- c(0, 150, 300, 450, 600, 750, 900, 1050, Inf)
    
    mypalette = colorBin(
      palette = "YlOrRd",
      domain = datamap1()$price,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepar the text for the tooltip:
    mytext = paste("price: ", datamap1()$price) %>%
      lapply(htmltools::HTML)
    leafletProxy("map", data = datamap1()) %>%
      clearShapes() %>%
      addCircles(
        ~ long,
        ~ lat,
        
        color = ~ mypalette(price),
        radius = 5,
        fillOpacity = 0.2,
        stroke = T,
        
        label = mytext,
        
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      
      clearControls() %>%
      
      addLegend(
        pal = mypalette,
        values =  ~ price,
        opacity = 0.9,
        title = "prices",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)