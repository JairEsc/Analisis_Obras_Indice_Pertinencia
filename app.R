library(shiny)
library(leaflet) # Necesario para el output del mapa

source("Códigos/raster_base.R")
source("Códigos/leer_obras_sipdus_carretera_y_separar_por_tipo_geometria.R")
source("Códigos/leer_rasters_generados_en_r.R")
#rsconnect::writeManifest()

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
                      value = 0,
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
              leafletOutput("result_map", height = "600px")
          )
      )
  )
)
server <- function(input, output, session) {
  # Asegúrate de que tu lista 'rasters' esté disponible en el entorno global
  # o se cargue aquí antes de ser referenciada.
  # Por ejemplo, si 'rasters' es un objeto global, no necesitas cargarlo aquí.
  # Si necesitas cargarlo, descomenta y ajusta esta sección:
  # rasters_global <- list()
  # for (i in 1:9) {
  #   r <- raster(ncols=50, nrows=50, xmn=-120, xmx=-80, ymn=15, ymx=35)
  #   values(r) <- runif(ncell(r), min = -1, max = 1)
  #   rasters_global[[i]] <- r
  # }
  
  rasters_reactive <- reactive({
    # Aquí se asume que 'rasters' es un objeto global o ya cargado
    # que contiene tu lista de 9 objetos raster.
    # Si 'rasters' no está definido en este scope, esto causará un error.
    # Podrías querer renombrar tu lista global si hay un conflicto de nombres.
    return(rasters) 
  })
  
  dummy_raster_data <- eventReactive(input$generate_map_button, {
    req(rasters_reactive())
    current_rasters <- rasters_reactive() |> sapply(scale)
    
    # Verifica que 'current_rasters' tenga exactamente 9 elementos
    if (length(current_rasters) != 9) {
      stop("La lista 'rasters' debe contener exactamente 9 objetos raster.")
    }
    
    weights <- numeric(9)
    for (i in 1:9) {
      weights[i] <- input[[paste0("raster_weight_", i)]]
    }
    
    combined_raster <- current_rasters[[1]]*weights[1]+current_rasters[[2]]*weights[2]+
      current_rasters[[3]]*weights[3]+current_rasters[[4]]*weights[4]+
      current_rasters[[5]]*weights[5]+current_rasters[[6]]*weights[6]+
      current_rasters[[7]]*weights[7]+current_rasters[[8]]*weights[8]+
      current_rasters[[9]]*weights[9]
    return(combined_raster)
  })
  
  output$result_map <- renderLeaflet({
    req(dummy_raster_data())
    leaflet() %>%
      addTiles() %>%
      addRasterImage(dummy_raster_data(), colors = "Spectral", opacity = 0.8) %>%
      addLegend(pal = colorNumeric("Spectral", values(dummy_raster_data()), na.color = "transparent"),
                values = values(dummy_raster_data()),
                title = "Pertinencia")
  })
} 

shinyApp(ui, server)

