

library(shiny)
library(sf)
library(leaflet)
library(leafem)
library(shinythemes)
library(DT)

lineas_rutas<- st_read("rutas.shp")
puntos_paradas<- st_read("paradas.shp")

pal <- colorFactor(
    palette = "viridis",
    domain = lineas_rutas$shape_id)

pal2 <- colorFactor(
    palette = "magma",
    domain = unique(puntos_paradas$stop_name))

ui <- bootstrapPage(

    theme = shinytheme("flatly"),
    navbarPage(
        title = "Assessment Map - Héctor",
        tabPanel("Map",
                 sidebarPanel(
                     h2("Filter Data"),
                     selectInput("rutas", label = "Select routes by ID",
                                 choices = unique(lineas_rutas$shape_id),
                                 selected = "110-1",
                                 multiple = TRUE,
                                 
                     ),
                     selectInput("paradas", label = "Select stops by name",
                                 choices = unique(puntos_paradas$stop_name),
                                 selected = "VILA NOVA MONTE CRISTO",
                                 multiple = TRUE,
                                 
                     ),
                     actionButton("filtrar", label = "Filter"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     h2("View all data"),
                     actionButton("todo", label="All data")
                 ),
                 mainPanel(
                     leafletOutput("mapa",  height = 550),
                     p("Data Sources: Assessment-Shapes.csv and Assesment-Stops.csv by WhereIsMyTransport;")
                 )
                 )
    )   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapa<- renderLeaflet({
        mapa<-leaflet()%>% addMiniMap()%>% addMouseCoordinates()%>% addScaleBar()%>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
            addLayersControl(
                baseGroups = c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap.Mapnik","Esri.WorldImagery")
            )%>%
            addPolylines(data=lineas_rutas, color = ~pal(lineas_rutas$shape_id), label = ~lineas_rutas$shape_id, labelOptions = labelOptions(direction = "left",textsize = "15px"), fillOpacity = 1)%>%
            addCircles(data= puntos_paradas, stroke = FALSE,color = ~pal2(puntos_paradas$stop_name), label = ~puntos_paradas$stop_name, labelOptions = labelOptions(direction = "left",textsize = "15px"), fillOpacity = 1)
    })
    
    map_proxy<-leaflet::leafletProxy("mapa")
    
    observeEvent(input$filtrar,{
        lineas_filtrados<-lineas_rutas[lineas_rutas$shape_id%in%input$rutas,]
        paradas_filtrados<-puntos_paradas[puntos_paradas$stop_name%in%input$paradas,]
        
        map_proxy%>%clearMarkers()%>%clearShapes()%>%addPolylines(data=lineas_filtrados, color = ~pal(lineas_filtrados$shape_id), label = ~lineas_filtrados$shape_id, labelOptions = labelOptions(direction = "left",textsize = "15px"),  fillOpacity = 1)%>%
            addCircles(data= paradas_filtrados, stroke = FALSE,color = ~pal2(paradas_filtrados$stop_name), label = ~paradas_filtrados$stop_name, labelOptions = labelOptions(direction = "left",textsize = "15px"), fillOpacity = 1)
            
    })
    
    observeEvent(input$todo,{
        map_proxy%>%clearMarkers()%>%clearShapes()%>%addPolylines(data=lineas_rutas, color = ~pal(lineas_rutas$shape_id), label = ~lineas_rutas$shape_id, labelOptions = labelOptions(direction = "left",textsize = "15px", fillOpacity = 1))%>%
            addCircles(data= puntos_paradas, stroke = FALSE,color = ~pal2(puntos_paradas$stop_name), label = ~puntos_paradas$stop_name, labelOptions = labelOptions(direction = "left",textsize = "15px"), fillOpacity = 1)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
