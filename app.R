library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras) 
library(terra)
library(raster)
library(sf)
library(sp)
library(leaflegend)
library(data.table)
source("Códigos/raster_base.R")##Regresa municipios y base
source("Códigos/leer_geojsons_c_extract.R") #Regresa funciones generate_labels, update_labels
                                            #También regresa obras tipo linea y punto de cada tipo de obra
source("Códigos/leer_rasters_generados_en_r.R") ##Regresa rasters_list_names (default obras viales nuevas)
#rsconnect::writeManifest()
####Requerimientos previos. 

raster_accesibilidad="Inputs/Rasters_Generados_en_R/Accesibilidad_cabeceras_negative_scaled.tif" |> raster()

##Definir pesos por default

weights= c(0.15,0.13,0,0,0.13,0.17,0.15,0,0.19,0.17)
  #c(9,6,0,0,6,8,7,0,0,11)#c(2*c(9,3,0,0,5,8,7,0,1),0,0,0,0,0)
rasters_list_names
rasters=list.files("Inputs/Rasters_Generados_en_R/rasters_app/",full.names = T) |> lapply(raster)##Definimos rasters del caso base
rasters_agua=list.files("Inputs/Rasters_Generados_en_R/rasters_app_agua/",full.names = T) |> lapply(raster)#Otro tema
rasters_drenaje=list.files("Inputs/Rasters_Generados_en_R/rasters_app_drenaje/",full.names = T) |> lapply(raster)#Otro tema
rasters_Espacios_publicos=list.files("Inputs/Rasters_Generados_en_R/rasters_app_espacios_publicos/",full.names = T) |> lapply(raster)#Otro tema
rasters_salud=list.files("Inputs/Rasters_Generados_en_R/rasters_app_salud/",full.names = T) |> lapply(raster)#Otro tema
rasters_educacion=list.files("Inputs/Rasters_Generados_en_R/rasters_app_educacion/",full.names = T) |> lapply(raster)#Otro tema

extent(raster_accesibilidad)=extent(rasters[[1]])##Para poder sumarlos
rasters[[1]]=-rasters[[1]]
###UI



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
              selectInput(inputId = "select_obras",label="",choices = c("Infraestructura vial",
                                                                        "Infraestructura Suministro de Agua",
                                                                        "Infraestructura Drenaje",
                                                                        "Infraestructura Salud", 
                                                                        "Infraestructura Educación",
                                                                        "Espacios Públicos"
                                                                        )) 
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
descripciones_Espacios_Publicos <- c(
  "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"
  )

descripciones_educacion <- c(
  "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra")
descripciones_salud <- c(
  "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra")

# Funciones auxiliares actualizadas

generar_lista_titulos <- function(tipo_obra, tipo_vialidad = '') {
  if (tipo_obra == 'Infraestructura vial') {
    if (is.null(tipo_vialidad) || tipo_vialidad == 'Construcción') {
      return(rasters_list_names)
    } else {#Mejoramiento
      return(c(rasters_list_names[1], "Nivel de uso"))
    }
  } else if (tipo_obra == "Infraestructura Suministro de Agua") {
    return(c(rasters_list_names[1:7], "Distancia a localidades con bajo acceso a agua entubada", rasters_list_names[9], "Percepción suministro de agua"))
  } else if (tipo_obra == "Infraestructura Drenaje") {
    return(c(rasters_list_names[1:7], "Distancia a localidades con bajo acceso a drenaje sanitario", rasters_list_names[9], "Percepción infraestructura de drenaje"))
  } else if (tipo_obra == "Espacios Públicos") {
    return(c(rasters_list_names[1:9],"Percepción de Espacios Públicos"))
  } else if (tipo_obra == "Infraestructura Educación") {
    return(c(rasters_list_names[1:9],"Percepción Infraestructura Educativa"))
  } else if (tipo_obra=='Infraestructura Salud'){
    return(c(rasters_list_names[1:9],"Percepción Infraestructura Salud"))
  }
}

generar_lista_descripciones <- function(tipo_obra, tipo_vialidad = '') {
  if (tipo_obra == 'Infraestructura vial') {
    if (is.null(tipo_vialidad) || tipo_vialidad == 'Construcción') {
      return(descripciones_minimas)
    } else {
      return(c(descripciones_minimas[1], "Se mide en número de viajes. Mayor valor significa más pertinente la obra"))
    }
  } else if (tipo_obra == "Infraestructura Suministro de Agua") {
    return(c(descripciones_minimas[1:7], "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin acceso a agua entubada. I.e. más pertinente la obra", descripciones_minimas[9], "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"))
  } else if (tipo_obra == "Infraestructura Drenaje") {
    return(c(descripciones_minimas[1:7], "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin acceso a drenaje sanitario. I.e. más pertinente la obra", descripciones_minimas[9], "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"))
  } else if (tipo_obra == "Espacios Públicos") {
    return(c(descripciones_minimas[1:9],descripciones_Espacios_Publicos[1]))
  } else if (tipo_obra == "Infraestructura Educación") {
    return(c(descripciones_minimas[1:9],descripciones_educacion[1]))
  } else if (tipo_obra=='Infraestructura Salud'){
    return(c(descripciones_minimas[1:9],descripciones_salud[1]))
  }
}

generar_lista_rasters <- function(tipo_obra, tipo_vialidad = '') {
  if (tipo_obra == 'Infraestructura vial') {
    if (is.null(tipo_vialidad) || tipo_vialidad == 'Construcción') {
      return(rasters)
      
    } else {
      raster_uso <- raster("Inputs/Rasters_Generados_en_R/Otros/nivel_de_uso_proxy_de_numero_de_viajes.tif")
      raster_uso <- ((raster_uso + 1) |> log() |> scale()) #|> aggregate(3)
      
      return(list(raster_accesibilidad, -raster_uso)) ##Cambiamos la interpretación de accesibilidad. Debe ser una zona con alta accesibilidad y mucho uso
    }
  } else if (tipo_obra == "Infraestructura Suministro de Agua") {
    return(c(rasters[1:7], rasters_agua[[1]], rasters[[9]], rasters_agua[[2]]))
  } else if (tipo_obra == "Infraestructura Drenaje") {
    return(c(rasters[1:7], rasters_drenaje[[1]], rasters[[9]], rasters_drenaje[[2]]))
  } else if (tipo_obra == "Espacios Públicos") {
    return(c(rasters[1:9],rasters_Espacios_publicos[[1]]))
  } else if (tipo_obra == "Infraestructura Educación") {
    return(c(rasters[1:9],rasters_educacion[[1]]))
  } else if (tipo_obra=='Infraestructura Salud'){
    return(c(rasters[1:9],rasters_salud[[1]]))
  }
}

# La función para los shapes también se generaliza
generar_lista_shapes <- function(tipo_obra, tipo_vialidad = '') {
  if (tipo_obra == 'Infraestructura vial') {
    if (is.null(tipo_vialidad) || tipo_vialidad == 'Construcción') {
      return(list(obras_sipdus_vialidades_mejora_lineas, obras_sipdus_vialidades_mejora_puntos))
    } else {
      return(list(obras_sipdus_vialidades_nuevas_lineas, obras_sipdus_vialidades_nuevas_puntos))
    }
  } else if (tipo_obra == "Infraestructura Suministro de Agua") {
    return(list(obras_sipdus_agua_lineas, obras_sipdus_agua_puntos))
  } else if (tipo_obra == "Infraestructura Drenaje") {
    return(list(obras_sipdus_drenaje_lineas, obras_sipdus_drenaje_puntos))
  } else if (tipo_obra == "Espacios Públicos") {
    return(list(obras_sipdus_espacios_publicos_linea, obras_sipdus_espacios_publicos_punto))
  } else if (tipo_obra == "Infraestructura Educación") {
    return(list(obras_sipdus_educacion_linea, obras_sipdus_educacion_punto))
  } else if (tipo_obra=='Infraestructura Salud'){
    return(list(obras_sipdus_salud_linea, obras_sipdus_salud_punto))
  }
}

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
    titulos <- generar_lista_titulos(input$select_obras, input$select_nuevas_o_reconstrucciones)
    descripciones <- generar_lista_descripciones(input$select_obras, input$select_nuevas_o_reconstrucciones)
    
    num_rasters <- length(titulos)
    print(num_rasters)
    lapply(1:num_rasters, function(i) {
      div(
        class = "mb-4",
        p(class = "input-label", titulos[[i]]),
        sliderInput(
          inputId = paste0("raster_weight_", i),
          label = descripciones[i],
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
    generar_lista_rasters(input$select_obras, input$select_nuevas_o_reconstrucciones)
  })
  
  shapes_seleccionados <- reactive({
    generar_lista_shapes(input$select_obras, input$select_nuevas_o_reconstrucciones)
  })
  
  dummy_raster_data <- eventReactive(input$generate_map_button, {
    num_rasters <- length(rasters_seleccionados())
    print(paste0("Numero de rasters_ sel", num_rasters))
    current_rasters <- rasters_seleccionados()
    current_rasters[[1]] <- -current_rasters[[1]]
    
    # if (length(current_rasters) != num_rasters) {
    #   stop("Este es un stop para asegurarme que uní servicios en un solo raster")
    # }

    weights <- numeric(num_rasters)
    print(num_rasters)
    for (i in 1:num_rasters) {
      weights[i] <- input[[paste0("raster_weight_", i)]]
    }
    print(weights)
    print("Haciendo la suma")
    combined_raster <- Reduce(`+`, Map(`*`, current_rasters , -weights))
    combined_raster[abs(combined_raster)<1e-4]=NA
    return(combined_raster)
  })
  
  output$result_map <- renderLeaflet({
    req(shapes_seleccionados())
    lineas_seleccionadas <- shapes_seleccionados()[[1]]
    puntos_seleccionadas <- shapes_seleccionados()[[2]]
    labels_para_usar <- generate_labels(lineas_seleccionadas, puntos_seleccionadas)
    leaflet() |>
      addTiles(options = tileOptions(opacity = 0.8)) |>
      addPolylines(data = lineas_seleccionadas, label = labels_para_usar[[1]], group = 'Obras (líneas)',layerId = 1:nrow(lineas_seleccionadas)) |>
      addMarkers(data = puntos_seleccionadas, label = labels_para_usar[[2]], group = 'Obras (puntos)',layerId = (1+nrow(lineas_seleccionadas)):(nrow(lineas_seleccionadas)+nrow(puntos_seleccionadas))) |>
      addLayersControl(overlayGroups = c("Pertinencia", 'Obras (líneas)', 'Obras (puntos)')) |> 
      leaflet.extras::addSearchFeatures(targetGroups = c('Obras (líneas)','Obras (puntos)'),options = leaflet.extras::searchFeaturesOptions(hideMarkerOnCollapse=T))
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
      paste0("listado_obras", Sys.Date(), input$select_obras, ".xlsx")
    },
    content = function(file) {
      req(dummy_raster_data())
      req(shapes_seleccionados())
      
      lineas_a_usar <- shapes_seleccionados()[[1]]
      puntos_a_usar <- shapes_seleccionados()[[2]]
      
      lista_resultados <- list()
      
      # Procesar líneas si existen
      if (nrow(lineas_a_usar) > 0) {
        z_lineas <- raster::extract(dummy_raster_data(), lineas_a_usar |> st_transform(st_crs("EPSG:32614")),
                                    method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                    fun = mean, na.rm = TRUE)
        lineas_df <- lineas_a_usar |>
          dplyr::select(ID_OBRA,Municipio:Ejecutora, Geometria_tipo) |>
          st_drop_geometry() |>
          dplyr::mutate(indice_pertinencia = z_lineas)
        lista_resultados[[length(lista_resultados) + 1]] <- lineas_df
      }
      
      # Procesar puntos si existen
      if (nrow(puntos_a_usar) > 0) {
        z_puntos <- raster::extract(ID_OBRA,dummy_raster_data(), puntos_a_usar |> st_transform(st_crs("EPSG:32614")),
                                    method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                    fun = mean, na.rm = TRUE)
        puntos_df <- puntos_a_usar |>
          dplyr::select(Municipio:Ejecutora, Geometria_tipo) |>
          st_drop_geometry() |>
          dplyr::mutate(indice_pertinencia = z_puntos)
        lista_resultados[[length(lista_resultados) + 1]] <- puntos_df
      }
      
      # Combinar los resultados si hay alguno
      if (length(lista_resultados) > 0) {
        zz <- do.call(rbind, lista_resultados) |>
          dplyr::arrange(dplyr::desc(indice_pertinencia))
        
        openxlsx::write.xlsx(zz, file, overwrite = TRUE)
      } else {
        # Si no hay obras para el tipo de obra, crear un archivo vacío o con un mensaje
        warning("No se encontraron obras para el tipo seleccionado.")
        df_vacio <- data.frame(
          Mensaje = "No hay obras de este tipo",
          stringsAsFactors = FALSE
        )
        openxlsx::write.xlsx(df_vacio, file, overwrite = TRUE)
      }
    }
  )
}

shinyApp(ui, server)
