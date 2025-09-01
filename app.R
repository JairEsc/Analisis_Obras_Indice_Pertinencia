library(shiny)
library(shinybusy)
library(leaflet) # Necesario para el output del mapa
library(terra)
library(raster)
library(sf)
library(sp)
library(leaflegend)
library(data.table)
source("Códigos/raster_base.R")##Regresa municipios y base
source("Códigos/leer_geojsons_c_extract.R") #Regresa funciones generate_labels, update_labels
                                            #También regresa obras tipo linea y punto de cada tipo de obra
source("Códigos/leer_rasters_generados_en_r.R") ##Regresa rasters, rasters_list_names
#rsconnect::writeManifest()
####Requerimientos previos. 

rasters_list_names[8]='Distancia a localidades con bajo acceso a agua entubada o drenaje sanitario'
rasters_list_names=rasters_list_names[c(1:8,10)]
##Definir pesos por default

weights= c(0.15,0.13,0,0,0.13,0.17,0.15,0,0.19,0.17)
  #c(9,6,0,0,6,8,7,0,0,11)#c(2*c(9,3,0,0,5,8,7,0,1),0,0,0,0,0)


rasters_list_names[[10]]="Percepción infraestructura vial"##Caso por default
rasters=list.files("Inputs/Rasters_Generados_en_R/rasters_app/",full.names = T) |> lapply(raster)##Definimos rasters del caso base
rasters_agua=list.files("Inputs/Rasters_Generados_en_R/rasters_app_agua/",full.names = T) |> lapply(raster)#Otro tema
rasters_drenaje=list.files("Inputs/Rasters_Generados_en_R/rasters_app_drenaje/",full.names = T) |> lapply(raster)#Otro tema

###UI
descripciones_minimas=c(
  "Se mide en distancia. Mayor valor significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a centros de trabajo significa más pertinente la obra",
  "Se mide en distancia. Mayor distancia una ANP significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a escuelas significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a hospitales significa más pertinente la obra",
  "Se mide en log-distancia. Menor distancia a hospitales localidades marginadas significa más pertinente la obra",
  "Se mide en log-distancia. Menor valor significa obra específica de una ZAP. I.e. más pertinente la obra",
  "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin servicios públicos I.e. más pertinente la obra",
  "Se mide en porcentaje de votos. Mayor valor significa más votos al partido",
  "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"
                        )


ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$style(HTML({"
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
      .sliders-responsive{
        height: 90vh;
        overflow-y:scroll
      }
      .leaflet-map-container-custom{
        height: 90vh;
      }
    "}))
  ),
  # Contenedor principal para toda la página, centrado y con ancho limitado
  div(class = "container mx-auto p-4 md:p-8 max-w-12xl",
      # Layout principal
      div(class = "grid grid-cols-1 lg:grid-cols-3 gap-8",
          # Panel lateral para los controles (pesos de los rasters)
          div(class = "lg:col-span-1 panel sliders-responsive",
              div(class='flex',
              h1(class = "text-2xl font-semibold mb-6 text-gray-800", "Análisis de obras:"),
              selectInput(inputId = "select_obras",label="",choices = c("Infraestructura vial","Infraestructura Suministro de Agua","Infraestructura Drenaje")) 
              ),uiOutput("second_input_infra_vial"),
              h2(class = "text-2xl font-semibold mb-6 text-gray-800", "Ajustar Pesos de Rasters"),
              
              p(class = "text-sm text-gray-600 mb-6", 
                "Define la influencia de cada uno de los rasters (estandarizados)"),
              
              # Sliders para los n rasters
              uiOutput("dynamic_sliders"),

              # Botón para generar el mapa
              div(class = "mt-8 text-center",
                  actionButton(
                    inputId = "generate_map_button",
                    label = "Generar Índice",
                    class = "btn-primary hover:scale-105 transform transition-all duration-200"
                  ),
                  downloadButton(
                    outputId = "download_excel",
                    label = "Descargar listado de obras",
                    class = "btn-primary hover:scale-105 transform transition-all duration-200"
                  )
              )
          ),
          
          # Panel principal para el mapa Leaflet
          div(class = "lg:col-span-2 panel flex flex-col items-center justify-center min-h-[500px] leaflet-map-container-custom",
              h2(class = "text-2xl font-semibold mb-4 text-gray-800", "Índice de pertinencia"),
              # Placeholder para el mapa Leaflet
              leafletOutput("result_map", height = "80vh"),
              ##Loading state cuando se de click a actualizar
              add_busy_spinner(spin = "cube-grid")
          )
      )
  )
)
##rasters es una variable global
rasters_segun_eleccion=rasters
server <- function(input, output, session) {
  output$second_input_infra_vial=renderUI({
    if(input$select_obras == 'Infraestructura vial'){
      selectInput(inputId = "select_nuevas_o_reconstrucciones",label="Elegir tipo de obra vial",choices = c("Construcción","Mejoramiento")) 
    }
    else{div()}
  })
  output$dynamic_sliders <- renderUI({
    
    titulos_para_sliders <- if (input$select_obras == 'Infraestructura vial' & input$select_nuevas_o_reconstrucciones=='Construcción') {
      rasters_list_names
    }
    else {
      if(input$select_obras == 'Infraestructura vial' & input$select_nuevas_o_reconstrucciones=='Mejoramiento'){
        c(rasters_list_names[1],"Nivel de uso")
      }
      if(input$select_obras == "Infraestructura Suministro de Agua"){
      nombres_agua <- c(
        rasters_list_names[1:7],
        "Distancia a localidades con bajo acceso a agua entubada",
        rasters_list_names[9],
        "Percepción suministro de agua"
      )}
      else{nombres_agua <- c(
        rasters_list_names[1:7],
        "Distancia a localidades con bajo acceso a drenaje sanitario",
        rasters_list_names[9],
        "Percepción infraestructura de drenaje"
      )}
      nombres_agua
    }
    descripciones_para_sliders <- if (input$select_obras == 'Infraestructura vial') {
      descripciones_minimas
    } else {
      if(input$select_obras == "Infraestructura Suministro de Agua"){
        nombres_agua <- c(
          descripciones_minimas[1:7],
          "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin acceso a agua entubada. I.e. más pertinente la obra",
          descripciones_minimas[9],
          "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"
        )
      }else{
        nombres_agua <- c(
          descripciones_minimas[1:7],
          "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin acceso a drenaje sanitario. I.e. más pertinente la obra",
          descripciones_minimas[9],
          "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"
        )
      }
      
      nombres_agua
    }
    
    lapply(1:10, function(i) {
      div(class = "mb-4",
          p(class = "input-label", titulos_para_sliders[[i]]),
          sliderInput(
            inputId = paste0("raster_weight_", i),
            label = descripciones_para_sliders[i],
            min = 0,
            max = 1,
            value = weights[i],
            step = 0.01,
            width = "100%"
          )
      )
    })
  })
  
  rasters_seleccionados <- reactive({
    print(paste0("Estamos usando: ", input$select_obras))
    
    if (input$select_obras == 'Infraestructura vial') {
      rasters_a_usar <- rasters
    } else {
      if(input$select_obras == "Infraestructura Suministro de Agua"){
      rasters_a_usar <- c(rasters[1:7], rasters_agua[[1]], rasters[[9]], rasters_agua[[2]])}
      else{
        rasters_a_usar <- c(rasters[1:7], rasters_drenaje[[1]], rasters[[9]], rasters_drenaje[[2]])
      }
    }
    return(rasters_a_usar)
  })
  
  shapes_seleccionados <- reactive({
    if (input$select_obras == 'Infraestructura vial') {
      shapes_a_usar <- list(lineas_c_extract, puntos_c_extract)
    } else {
      if(input$select_obras == "Infraestructura Suministro de Agua"){
      shapes_a_usar <- list(lineas_agua, puntos_agua)
      }
      else{
        shapes_a_usar <- list(lineas_drenaje, puntos_drenaje)
      }
    }
    return(shapes_a_usar)
  })
  
  dummy_raster_data <- eventReactive(input$generate_map_button, {
    current_rasters <- rasters_seleccionados()
    current_rasters[[1]] <- -current_rasters[[1]]
    if (length(current_rasters) != 10) {
      stop("Este es un stop para asegurarme que uní servicios en un solo raster")
    }
    weights <- numeric(10)
    for (i in 1:10) {
      weights[i] <- input[[paste0("raster_weight_", i)]]
    }
    print(weights)
    print("Haciendo la suma")
    combined_raster <- Reduce(`+`, Map(`*`, current_rasters , -weights))
    combined_raster <- combined_raster
    return(combined_raster)
  })
  
  output$result_map <- renderLeaflet({
    req(shapes_seleccionados())
    lineas_seleccionadas <- shapes_seleccionados()[[1]]
    puntos_seleccionadas <- shapes_seleccionados()[[2]]
    labels_para_usar <- generate_labels(lineas_seleccionadas, puntos_seleccionadas)
    leaflet() |>
      addTiles() |>
      addPolylines(data = lineas_seleccionadas, label = labels_para_usar[[1]], group = 'Obras (líneas)',layerId = 1:nrow(lineas_seleccionadas)) |>
      addMarkers(data = puntos_seleccionadas, label = labels_para_usar[[2]], group = 'Obras (puntos)',layerId = (1+nrow(lineas_seleccionadas)):(nrow(lineas_seleccionadas)+nrow(puntos_seleccionadas))) |>
      addLayersControl(overlayGroups = c("Pertinencia", 'Obras (líneas)', 'Obras (puntos)'))
  })
  
  observeEvent(input$result_map_marker_click,{
    req(dummy_raster_data())
    req(shapes_seleccionados())
    puntos_seleccionadas <- shapes_seleccionados()[[2]]
    obra_sel <- puntos_seleccionadas[as.numeric(input$result_map_marker_click$id) - nrow(shapes_seleccionados()[[1]]), ]
    
    z <- raster::extract(dummy_raster_data(), obra_sel |> st_transform(st_crs("EPSG:32614")), method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,
                         fun=mean, na.rm=TRUE)
    
    leafletProxy(mapId = "result_map") %>%
      clearPopups() %>%
      addPopups(dat = input$result_map_marker_click, lat = ~lat, lng = ~lng, update_labels(obra_sel,z))
  })
  
  observeEvent(input$result_map_shape_click,{
    req(dummy_raster_data())
    req(shapes_seleccionados())
    lineas_seleccionadas <- shapes_seleccionados()[[1]]
    obra_sel <- as.numeric(input$result_map_shape_click$id)
    obra_sel <- lineas_seleccionadas[obra_sel,]
    z <- raster::extract(dummy_raster_data(), obra_sel |> st_transform(st_crs("EPSG:32614")), method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE,
                         fun=mean, na.rm=TRUE)
    
    leafletProxy(mapId = "result_map") %>%
      clearPopups() %>%
      addPopups(dat = input$result_map_shape_click, lat = ~lat, lng = ~lng, update_labels(obra_sel,z))
  })
  
  observeEvent(input$generate_map_button, {
    req(dummy_raster_data())
    min_raster <- raster::minValue(dummy_raster_data())
    max_raster <- raster::maxValue(dummy_raster_data())
    leafletProxy("result_map") |>
      clearImages() |>
      clearControls() |>
      addRasterImage(dummy_raster_data(), colors = "Spectral", opacity = 0.8, group = "Pertinencia") |>
      addLegendNumeric( pal = colorNumeric('Spectral', seq(min_raster,max_raster,0.01)) , values = seq(min_raster,max_raster,0.01), position = 'bottomright', title = 'Pertinencia', orientation = 'horizontal', shape = 'rect', decreasing = FALSE, height = 20, width = 100,labels = c(round(min_raster,2) |> paste0(), round(max_raster,2) |> paste0()),tickLength = 0)
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("listado_obras", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(dummy_raster_data())
      req(shapes_seleccionados())
      
      lineas_a_usar <- shapes_seleccionados()[[1]]
      puntos_a_usar <- shapes_seleccionados()[[2]]
      
      print("Inicia el extract")
      
      z_lineas <- raster::extract(dummy_raster_data(), lineas_a_usar |> st_transform(st_crs("EPSG:32614")),
                                  method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                  fun = mean, na.rm = TRUE)
      z_lineas[is.na(z_lineas)] <- 0
      lineas_a_usar$extract <- z_lineas
      
      z_puntos <- raster::extract(dummy_raster_data(), puntos_a_usar |> st_transform(st_crs("EPSG:32614")),
                                  method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                  fun = mean, na.rm = TRUE)
      z_puntos[is.na(z_puntos)] <- 0
      puntos_a_usar$extract <- z_puntos
      
      print("termina el extract")
      
      zz <- rbind(
        lineas_a_usar |>
          dplyr::select(Municipio:Ejecutora, Geometria_tipo, extract) |>
          dplyr::rename(indice_pertinencia = extract) |>
          st_drop_geometry(),
        puntos_a_usar |>
          dplyr::select(Municipio:Ejecutora, Geometria_tipo, extract) |>
          dplyr::rename(indice_pertinencia = extract) |>
          st_drop_geometry()
      ) |> dplyr::arrange(dplyr::desc(indice_pertinencia))
      
      openxlsx::write.xlsx(zz, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
