library(shiny)
library(shinybusy)
library(leaflet) # Necesario para el output del mapa
library(terra)
library(raster)
library(sf)
library(sp)

source("Códigos/raster_base.R")
source("Códigos/leer_geojsons_c_extract.R")
source("Códigos/leer_rasters_generados_en_r.R")
#rsconnect::writeManifest()
origin(rasters[[1]])=origin(rasters[[2]])
extent(rasters[[1]])=extent(rasters[[2]])
rasters[[8]]=min(rasters[[8]],rasters[[9]],na.rm = T)
rasters=rasters[c(1:8,10)]
rasters_list_names[8]='Distancia a localidades con bajo acceso a agua entubada o drenaje sanitario'
rasters_list_names=rasters_list_names[c(1:8,10)]
weights=c(9,3,0,0,5,8,7,0,1)
weights=weights/sum(weights)
weights[1]=-weights[1]
weights=-weights
for(i in 1:length(rasters)){
  raster_vals <- values(rasters[[i]])
  raster_vals <- raster_vals[!is.na(raster_vals)]
  
  q1 <- quantile(raster_vals, 0.25)
  q3 <- quantile(raster_vals, 0.75)
  iqr <- q3 - q1
  
  upper_limit <- q3 + 1.5 * iqr
  lower_limit <- q1 - 1.5 * iqr
  
  rasters[[i]] <- clamp(rasters[[i]], 
                        lower = lower_limit, 
                        upper = upper_limit, 
                        useValues=TRUE)
}
rasters=rasters |> lapply(scale)
ui <- fluidPage(
  # Enlace a Tailwind CSS para un estilo moderno y responsivo
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f3f4f6; /* Un gris claro de fondo */
      }
      .panel {
        background-color: #ffffff;
        border-radius: 0.75rem; /* Bordes redondeados */
        padding: 1.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
      }
      .title-text {
        color: #1f2937; /* Gris oscuro para el texto del título */
        font-weight: 700;
      }
      .subtitle-text {
        color: #4b5563; /* Gris medio para el subtítulo */
      }
      .input-label {
        font-weight: 500;
        color: #374151;
        margin-bottom: 0.5rem;
        display: block;
      }
      .btn-primary {
        background-color: #4f46e5; /* Índigo */
        color: white;
        padding: 0.75rem 1.5rem;
        border-radius: 0.5rem;
        transition: background-color 0.2s;
      }
      .btn-primary:hover {
        background-color: #4338ca; /* Índigo más oscuro al pasar el ratón */
      }
      .shiny-input-container:not(.shiny-input-container-inline) {
        margin-bottom: 1.5rem;
      }
      .leaflet-container {
        border-radius: 0.75rem; /* Bordes redondeados para el mapa */
      }
    "))
  ),
  
  # Contenedor principal para toda la página, centrado y con ancho limitado
  div(class = "container mx-auto p-4 md:p-8 max-w-12xl",
      
      # Título de la aplicación
      div(class = "text-center mb-8",
          h1(class = "text-4xl lg:text-5xl font-extrabold title-text mb-2", "Análisis de Jerarquías"),
          p(class = "text-lg subtitle-text", "Ajusta los pesos para crear un nuevo raster y visualízalo en el mapa.")
      ),
      
      # Layout principal con panel lateral para controles y panel principal para el mapa
      div(class = "grid grid-cols-1 lg:grid-cols-3 gap-8",
          
          # Panel lateral para los controles (pesos de los rasters)
          div(class = "lg:col-span-1 panel",
              h2(class = "text-2xl font-semibold mb-6 text-gray-800", "Ajustar Pesos de Rasters"),
              
              p(class = "text-sm text-gray-600 mb-6", 
                "Define la influencia de cada uno de los 9 rasters estandarizados. Los valores pueden estar entre -1 y 1."),
              
              # Sliders para los 9 rasters
              lapply(1:9, function(i) {
                div(class = "mb-4",
                    p(class = "input-label", rasters_list_names[[i]]),
                    sliderInput(
                      inputId = paste0("raster_weight_", i),
                      label = NULL, # El label se maneja con el 'p' de arriba
                      min = -1,
                      max = 1,
                      value = weights[i],
                      step = 0.01,
                      width = "100%"
                    )
                )
              }),
              
              # Botón para generar el mapa
              div(class = "mt-8 text-center",
                  actionButton(
                    inputId = "generate_map_button",
                    label = "Generar Índice",
                    class = "btn-primary hover:scale-105 transform transition-all duration-200"
                  )
              )
          ),
          
          # Panel principal para el mapa Leaflet
          div(class = "lg:col-span-2 panel flex flex-col items-center justify-center min-h-[500px]",
              h2(class = "text-2xl font-semibold mb-4 text-gray-800", "Índice de pertinencia"),
              # Placeholder para el mapa Leaflet
              leafletOutput("result_map", height = "600px"),
              add_busy_spinner(spin = "cube-grid")
          )
      )
  )
)
server <- function(input, output, session) {
  dummy_raster_data <- eventReactive(input$generate_map_button, {
    current_rasters <- rasters
    
    # Verifica que 'current_rasters' tenga exactamente 9 elementos
    if (length(current_rasters) != 9) {
      stop("La lista 'rasters' debe contener exactamente 9 objetos raster.")
    }
    
    weights <- numeric(9)
    for (i in 1:9) {
      weights[i] <- input[[paste0("raster_weight_", i)]]
    }
    #print("Haciendo la suma")
    combined_raster <- Reduce(`+`, Map(`*`, current_rasters, weights))
    #print("suma lista")
    return(combined_raster |>aggregate( fact = 3, fun = "mean"))
  })
  
  output$result_map <- renderLeaflet({
    # The initial leaflet map is rendered here without the raster image.
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = lineas_c_extract, label = lineas_labels, group = 'Obras (líneas)',
                   ) %>%
      addMarkers(data = puntos_c_extract, label = puntos_labels, group = 'Obras (puntos)'
                 ) %>%
      addLayersControl(overlayGroups = c("Pertinencia", 'Obras (líneas)', 'Obras (puntos)'
                                         )) 
    # %>%
    #   htmlwidgets::onRender(
    #     "function(el, x) {
    #       this.getPane('tooltipPane').style.maxWidth = '800px';
    #       var style = document.createElement('style');
    #       style.innerHTML = '.leaflet-tooltip { max-width: 800px; white-space: normal; }';
    #       document.head.appendChild(style);
    #     }"
    #   )
  })
  
  observeEvent(input$generate_map_button, {
    req(dummy_raster_data())
    
    # This observeEvent handles clearing and redrawing the raster
    # and legend only when the button is clicked.
    #print("Haciendo el dibujo")
    leafletProxy("result_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(dummy_raster_data(), colors = "Spectral", opacity = 0.8, group = "Pertinencia") %>%
      addLegendNumeric( pal = colorNumeric('Spectral', 1:100) , values = 1:100, position = 'bottomright', title = 'Pertinencia', orientation = 'horizontal', shape = 'rect', decreasing = FALSE, height = 20, width = 100,labels = c('Baja', "Alta"),tickLength = 0) 
    #print("dibujo listo")
  })
}

shinyApp(ui, server)

