library(shiny)
library(shinybusy)
library(leaflet) # Necesario para el output del mapa
library(terra)
library(raster)
library(sf)
library(sp)
library(leaflegend)
source("Códigos/raster_base.R")
source("Códigos/leer_geojsons_c_extract.R")
source("Códigos/leer_rasters_generados_en_r.R")
#rsconnect::writeManifest()
####Requerimientos previos. 
##Modificar el raster 1 (Accesibilidad)
origin(rasters[[1]])=origin(rasters[[2]])
extent(rasters[[1]])=extent(rasters[[2]])
##Unir servicios
rasters[[8]]=min(rasters[[8]],rasters[[9]],na.rm = T)
rasters=rasters[c(1:8,10)]
rasters_list_names[8]='Distancia a localidades con bajo acceso a agua entubada o drenaje sanitario'
rasters_list_names=rasters_list_names[c(1:8,10)]
##Definir pesos por default
weights=c(9,3,0,0,5,8,7,0,1)
weights=weights/sum(weights)
##Darles interpretación como en la documentación.
weights[1]=-weights[1]
weights=-weights
##Escalar y eliminar outliers
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

###UI

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f3f4f6; 
      }
      .panel {
        background-color: #ffffff;
        border-radius: 0.75rem; 
        padding: 1.5rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
      }
      .title-text {
        color: #1f2937; 
        font-weight: 700;
      }
      .subtitle-text {
        color: #4b5563; 
      }
      .input-label {
        font-weight: 500;
        color: #374151;
        margin-bottom: 0.5rem;
        display: block;
      }
      .btn-primary {
        background-color: #4f46e5; 
        color: white;
        padding: 0.75rem 1.5rem;
        border-radius: 0.5rem;
        transition: background-color 0.2s;
      }
      .btn-primary:hover {
        background-color: #4338ca;
      }
      .shiny-input-container:not(.shiny-input-container-inline) {
        margin-bottom: 1.5rem;
      }
      .leaflet-container {
        border-radius: 0.75rem;
      }
    "))
  ),
  
  # Contenedor principal para toda la página, centrado y con ancho limitado
  div(class = "container mx-auto p-4 md:p-8 max-w-12xl",
      # Layout principal
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
              leafletOutput("result_map", height = "80vh"),
              ##Loading state cuando se de click a actualizar
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
      stop("Este es un stop para asegurarme que uní servicios en un solo raster")
    }
    
    weights <- numeric(9)
    for (i in 1:9) {
      weights[i] <- input[[paste0("raster_weight_", i)]]
    }
    print("Haciendo la suma")
    combined_raster = Reduce(`+`, Map(`*`, current_rasters, weights))|>
      aggregate( fact = 3, fun = "mean")##Para que se tarde un poco menos en pintarlo con leaflet
    print("suma lista")
    return(combined_raster )
  })
  
  output$result_map <- renderLeaflet({
    # Mapa inicial con obrasy tiles
    leaflet() |> 
      addTiles()  |> 
      addPolylines(data = lineas_c_extract, label = lineas_labels, group = 'Obras (líneas)') |> 
      addMarkers(data = puntos_c_extract, label = puntos_labels, group = 'Obras (puntos)') |> 
      addLayersControl(overlayGroups = c("Pertinencia", 'Obras (líneas)', 'Obras (puntos)')) 
  })
  
  observeEvent(input$generate_map_button, {
    req(dummy_raster_data())
    print("Inicia el extract")
    dummy_raster_data() |> raster::extract(lineas_c_extract |> st_transform(st_crs("EPSG:32614")), method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,
                                   fun=mean, na.rm=TRUE)->z
    z[z |> is.na()]=0
    lineas_c_extract$extract=z
    dummy_raster_data() |> raster::extract(puntos_c_extract|> st_transform(st_crs("EPSG:32614")), method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,
                                   fun=mean, na.rm=TRUE)->z
    puntos_c_extract$extract=z
    z[z |> is.na()]=0
    print("termina el extract")
    # Aquí se dibuja el raster
    print("Haciendo el dibujo")
    leafletProxy("result_map") |> 
      clearImages() |> 
      clearControls()  |> 
      addRasterImage(dummy_raster_data(), colors = "Spectral", opacity = 0.8, group = "Pertinencia") |> 
      addLegendNumeric( pal = colorNumeric('Spectral', 1:100) , values = 1:100, position = 'bottomright', title = 'Pertinencia', orientation = 'horizontal', shape = 'rect', decreasing = FALSE, height = 20, width = 100,labels = c('Baja', "Alta"),tickLength = 0) 
    print("dibujo listo")
  })
}

shinyApp(ui, server)

