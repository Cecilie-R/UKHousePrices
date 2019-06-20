library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Wesley and the houses"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps", 
        tabName = "maps", 
        icon = icon("globe"),
        menuSubItem("Sold Prices", tabName = "sold", icon = icon("map"))
      #  menuSubItem("Population", tabName = "m_pop", icon = icon("map"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "sold",
        sliderInput("Dates",
                    "Dates:",
                    min = as.Date("1995-01-01","%Y-%m-%d"),
                    max = as.Date("2019-04-01","%Y-%m-%d"),
                    value=as.Date("2016-12-01"),timeFormat="%Y-%m-%d"),
        fluidRow(
        column(3,
        radioButtons("Type", "Property Type", c("Detatched"="D", 
                                                "Semidetatched" = "S",
                                                "Terraced" = "T",
                                                "Flats/Maisonettes" = "F",
                                                "Other" = "O",
                                                "All (will be slow)" = "A"))),
        column(3,
        radioButtons("OldNew", "Age of Property", c("Established" = "Y",
                                                    "New Build" = "N",
                                                    "Any" = "A"))),
        column(3,
        radioButtons("Duration", "Ownership Duration", c("Freehold" = "F",
                                                    "Leasehold" = "L",
<<<<<<< HEAD
                                                    "Any" = "A")))),
=======
                                                    "Any" = "A"))),
        column(3,
        radioButtons("SalesType", "Sales Type", c("Private Recidency" = "A",
                                                         "Non-Private (e.g. Buy to let)" = "B",
                                                         "Any" = "A")))),
>>>>>>> master
        
        
        
        
        box(
          title = "sold prices",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          leafletOutput("map")
        )
      )
)))

server <- function(input, output) {
<<<<<<< HEAD
  
  output$map<-renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles()  %>% 
  setView(lat=52.1386394, lng=-0.4667782 , zoom=8)
  })
  


    
    
    
    
    datamap1<-reactive({
      Dates<-format(input$Dates, "%Y-%m")
      
      datamap1<-readRDS(paste(Dates, ".Rds", sep=""))
    
    if(input$Type=="T"){
      datamap1<-subset(datamap1, type=="T")
    } else if (input$Type=="D"){
      datamap1<-subset(datamap1, type=="D")
    } else if(input$Type=="F"){
      datamap1<-subset(datamap1, type=="F")
    } else if(input$Type=="S"){
      datamap1<-subset(datamap1, type=="S")
    } else if(input$Type=="O"){
      datamap1<-subset(datamap1, type=="O")
    } else if(input$Type=="A"){
      datamap1<-datamap1
    }
    
    if(input$OldNew=="y"){
      datamap1<-subset(datamap1, newbuild=="Y")
    } else if (input$OldNew=="M"){
      datamap1<-subset(datamap1, newbuild=="N")
    } else if(input$OldNew=="N"){
      datamap1<-datamap1
    }
    
    if(input$Duration=="L"){
      datamap1<-subset(datamap1, duration=="L")
    } else if (input$Duration=="F"){
      datamap1<-subset(datamap1, duration=="F")
    } else if(input$Duration=="A"){
      datamap1<-datamap1
    }
    
    
    datamap1$price<-datamap1$price/1000
    datamap1
    })

    observe({
    # Create a color palette with handmade bins.
    mybins=seq(min(datamap1()$price), max(datamap1()$price), by=10000)
    mybins<-c(0,150,300,450,600,750,900,1050,Inf)
    mypalette = colorBin( palette="YlOrRd", domain=datamap1()$price, na.color="transparent", bins=mybins)
    
    # Prepar the text for the tooltip:
    mytext=paste("price: ", datamap1()$price) %>%
      lapply(htmltools::HTML)
    
    leafletProxy("map", data = datamap1()) %>%
      clearShapes() %>%
      addCircles(~long, ~lat, 
                 color = ~mypalette(price), radius=5, fillOpacity = 0.2, stroke=T,
                 label = mytext,
                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                   clearControls() %>% 
                   addLegend( pal=mypalette, values=~price, opacity=0.9, title = "prices", position = "bottomright" )
                 
        })
    
=======
  output$map <- renderLeaflet({
    
    Dates<-format(input$Dates, "%Y-%m")
    
    
    datamap1<-readRDS(paste(Dates, ".Rds", sep=""))
    datamap1$price<-datamap1$price/1000
    # load example data (Fiji Earthquakes) + keep only 100 first lines
    #data(quakes)
    #quakes =  head(quakes, 100)
    
    # Create a color palette with handmade bins.
    mybins=seq(min(datamap1$price), max(datamap1$price), by=10000)
    mybins<-c(0,150,300,450,600,750,900,1050,Inf)
    mypalette = colorBin( palette="YlOrRd", domain=datamap1$price, na.color="transparent", bins=mybins)
    
    # Prepar the text for the tooltip:
    mytext=paste("price: ", datamap1$price) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(datamap1,
          #  clusterOptions = markerClusterOptions(),
            options = leafletOptions(preferCanvas = TRUE)) %>% 
      addTiles()  %>% 
      setView( lat=52.1386394, lng=-0.4667782 , zoom=8) %>%
      #  addProviderTiles("Esri.WorldImagery") %>% #Esri.WorldGrayCanvas
      addCircles(~long, ~lat, 
                 color = ~mypalette(price), radius=13, fillOpacity = 0.2, stroke=T,
                 label = mytext,
                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=mypalette, values=~price, opacity=0.9, title = "Magnitude", position = "bottomright" )
    
    
  })
>>>>>>> master
}

shinyApp(ui, server)