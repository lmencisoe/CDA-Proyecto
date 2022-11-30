
library(geosphere, include.only = 'distHaversine')
library(shiny)
library(leaflet)
library(shinydashboard)
library(openxlsx)
library(DT)
library(dplyr)


#setwd("C:/Apps_Shiny/Shiny_App_CDA")

data_1 <- read.xlsx('data_total.xlsx') %>% 
  mutate(Categoria = as.character(ntile(precio, 5)))
str(data_1)

ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("Proyecto CDA (Baquero  - Enciso - Menco) - Universidad de los Andes",
                                             style = "color: white; font-size: 14px"),
                                titleWidth = 400),
                dashboardSidebar(
                  tags$head(tags$style(HTML('
                               /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2D5FCC;
                                }
                              '))),
                  
                  selectInput(inputId = 'trim',
                              label= "Seleccione ID de inmueble",
                              choices = 1:nrow(data_1),
                              selected = NA,
                              multiple = F),
                  actionButton(inputId = "Clic", label = 'Calcule')
                ),
                dashboardBody(
                  fluidRow(
                    valueBoxOutput("resu1"),
                    valueBoxOutput("resu2"),
                    valueBoxOutput("resu3")
                  ),
                  fluidRow(
                    column(
                      width = 10,
                      offset = 0, style='padding-top:100px;padding-bottom:70px; padding-left:0px',
                      DT::dataTableOutput(outputId = 'data_filtered', width = "100%")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      leafletOutput("leafletmap", width = "1500px")
                    )
                  ),
                  p(),
                  tags$footer('El modelo se actualiza cada mes, recalibrando hiperparámetros',
                              tags$br(),
                              'Las predicciones se actualizan de forma diaria y el usuario puede acceder a ver sus predicciones y vecinos cercanos',
                              tags$br()
                  )
                )
  )
)


server <- shinyServer(function(input, output, session){
  
  reactivo <- eventReactive(input$Clic,
                            {
                              kk <- input$trim
                              
                              data_filtrada <- data_1 %>% 
                                filter(ID == kk)
                              coordinates_now <- data_filtrada[c('longitud', 'latitud')]
                              
                              data_no <- data_1 %>%
                                filter(ID != kk)
                              
                              distancias <- distHaversine(data_no[c('longitud', 'latitud')], coordinates_now)
                              
                              data_no <- data_no %>%
                                mutate(distancia = distancias) %>%
                                arrange(distancia)
                              
                              other_random <- sample(2:ncol(data_filtrada), 10)
                              
                              
                              data_no <- data_no[1:25,]
                              
                              data_mostrar <- data_filtrada[,c(1, other_random)]
                              
                              Lista <- list()
                              Lista[['data_f']] <- data_filtrada
                              Lista[['data_cerc']] <- data_no
                              Lista[['data_mostrar']] <- data_mostrar
                              
                              return(list(Lista = Lista))
                            })
  
  
  
  output$data_filtered <- DT::renderDataTable({
    req(reactivo())
    Lista <- reactivo()$Lista
    
    selTable <- Lista$data_mostrar
    
    DT::datatable(data = selTable,
                  escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip', 
                                 lengthChange = FALSE,
                                 dom = 'l',
                                 searchable = FALSE))
    
    
  })
  
  
  output$resu1 <- renderValueBox({
    valueBox(value = tags$p("87%", style = "font-size: 100%;"), 
             "Accuracy total del modelo", icon = icon("bell"), color = "navy")
  })
  
  output$resu2 <- renderValueBox({
    req(reactivo())
    Lista <- reactivo()$Lista
    
    selTable <- Lista$data_f
    acc <- min(selTable$Pred, selTable$precio)/max(selTable$Pred, selTable$precio)
    valueBox(value = tags$p(paste(round(acc,4)*100, "%"),
                            style = "font-size: 100%;"),
             "Accuracy dato ingresado",
             icon = icon("check-circle"), 
             color = "blue")
  })
  
  output$resu3 <- renderValueBox({
    req(reactivo())
    Lista <- reactivo()$Lista
    
    selTable <- Lista$data_f
    valueBox(value = tags$p(selTable$Pred[1], style = "font-size: 100%;"), 
             "Valor predicho venta", 
             icon = icon("chart-pie"), color = "light-blue")
  })
  
  
  
  output$leafletmap <- renderLeaflet({
    req(reactivo())
    Lista <- reactivo()$Lista
    selTable1 <- Lista$data_f  %>% 
      mutate(Categoria = '0')# Add this
    selTable2 <- Lista$data_cerc
    
    data_total <- selTable1 %>% 
      bind_rows(selTable2)
    
    pal <- colorFactor(
      palette = c('black', '#F04D00', '#F79D0C', '#E0BB00', '#D7FA2F', '#37F026'),
      domain = data_total$Categoria
    )
    
    
    data_total %>% leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        ~ longitud ,  # Removed `data = data_map()`
        ~ latitud ,
        popup =  10,
        radius = ~ Pred/10000000 ,
        color = ~pal(Categoria) ,
        stroke = FALSE,
        fillOpacity = 0.8,
        popupOptions = popupOptions(closeButton = FALSE)
      ) %>% 
      addLegend("bottomright", pal = pal, values = ~Categoria,
                title = "Valores de vivienda",
                labFormat = labelFormat(prefix = "Grupo "),
                opacity = 1
      )
  })
  
  
})

shinyApp(ui, server)