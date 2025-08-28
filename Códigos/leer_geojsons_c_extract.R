library(sf)
##Generar los puntos y lineas de obras de agua
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_agua_linea.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_agua
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_agua_punto.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_agua

##Generar los puntos y lineas de obras de drenaje
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_drenaje_linea.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_drenaje
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_drenaje_punto.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_drenaje


####Leer obras viales en puntos y lineas
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_linea_extract.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_c_extract
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_punto_extract.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_c_extract

lineas_c_extract$extract[lineas_c_extract$extract |> is.na()]=0

generate_labels=function(lineas_seleccionadas,puntos_seleccionadas){
  lineas_labels <- lapply(1:nrow(lineas_seleccionadas), function(i) {
    obra <- lineas_seleccionadas$Obra[i]
    inversion <- ifelse(!is.na(lineas_seleccionadas$Inversión[i]),paste0("$",formatC(lineas_seleccionadas$Inversión[i], big.mark = ",",format = "d")),"-")
    #pertinencia <- round(lineas_seleccionadas$extract[i], 2)
    # Dividir el texto en un vector de palabras
    obra_words <- unlist(strsplit(obra, split = " "))
    n <- length(obra_words)
    
    # Reconstruir la cadena insertando <br> cada 5 palabras
    obra_2 <- ""
    for (j in seq(1, n, by = 5)) {
      # Tomar un bloque de 5 palabras
      end_index <- min(j + 4, n)
      palabras_bloque <- obra_words[j:end_index]
      
      # Unir las palabras y agregar un salto de línea
      linea <- paste(palabras_bloque, collapse = " ")
      obra_2 <- paste0(obra_2, linea, "<br>")
    }
    
    htmltools::HTML(paste0("<b>Obra:</b> ", obra_2,
                           "<b>Inversión:</b> ", inversion, "<br>",
                           "<b>Pertinencia:</b> ", "click para mostrar"))
    
  })
  
  # Construir etiquetas para los marcadores
  puntos_labels <- lapply(1:nrow(puntos_seleccionadas), function(i) {
    obra <- puntos_seleccionadas$Obra[i]
    
    # Dividir el texto en un vector de palabras
    obra_words <- unlist(strsplit(obra, split = " "))
    n <- length(obra_words)
    
    # Reconstruir la cadena insertando <br> cada 5 palabras
    obra_2 <- ""
    for (j in seq(1, n, by = 5)) {
      # Tomar un bloque de 5 palabras
      end_index <- min(j + 4, n)
      palabras_bloque <- obra_words[j:end_index]
      
      # Unir las palabras y agregar un salto de línea
      linea <- paste(palabras_bloque, collapse = " ")
      obra_2 <- paste0(obra_2, linea, "<br>")
    }
    
    inversion <- ifelse(!is.na(puntos_seleccionadas$Inversión[i]), paste0("$", formatC(puntos_seleccionadas$Inversión[i], big.mark = ",", format = "d")), "-")
    #pertinencia <- round(puntos_c_extract$extract[i], 2)
    
    htmltools::HTML(paste0("<b>Obra:</b> ", obra_2,
                           "<b>Inversión:</b> ", inversion, "<br>",
                           "<b>Pertinencia:</b> ", "click para mostrar"))
  })
  return(list(lineas_labels,puntos_labels))
  
}
update_labels=function(obra_sel,z){
  ###Actualizar el pop up con un label
  obra <- obra_sel$Obra[1]
  
  # Dividir el texto en un vector de palabras
  obra_words <- unlist(strsplit(obra, split = " "))
  n <- length(obra_words)
  
  # Reconstruir la cadena insertando <br> cada 5 palabras
  obra_2 <- ""
  for (j in seq(1, n, by = 5)) {
    # Tomar un bloque de 5 palabras
    end_index <- min(j + 4, n)
    palabras_bloque <- obra_words[j:end_index]
    
    # Unir las palabras y agregar un salto de línea
    linea <- paste(palabras_bloque, collapse = " ")
    obra_2 <- paste0(obra_2, linea, "<br>")
  }
  
  inversion <- ifelse(!is.na(obra_sel$Inversión[1]), paste0("$", formatC(obra_sel$Inversión[1], big.mark = ",", format = "d")), "-")
  z <- round(z, 2)
  return(htmltools::HTML(paste0("<b>Obra:</b> ", obra_2,
                                  "<b>Inversión:</b> ", inversion, "<br>",
                                  "<b>Pertinencia:</b> ", z)))
  
}

