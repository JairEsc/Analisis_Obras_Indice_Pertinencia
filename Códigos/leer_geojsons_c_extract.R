library(sf)

st_read("Inputs/Rasters_Generados_en_R/Otros/obras_linea_extract.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_c_extract
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_punto_extract.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_c_extract


lineas_labels <- lapply(1:nrow(lineas_c_extract), function(i) {
  obra <- lineas_c_extract$Obra[i]
  inversion <- ifelse(!is.na(lineas_c_extract$Inversión[i]),paste0("$",formatC(lineas_c_extract$Inversión[i], big.mark = ",",format = "d")),"-")
  pertinencia <- round(lineas_c_extract$extract[i], 2)
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
  
  inversion <- ifelse(!is.na(puntos_c_extract$Inversión[i]), paste0("$", formatC(puntos_c_extract$Inversión[i], big.mark = ",", format = "d")), "-")
  pertinencia <- round(puntos_c_extract$extract[i], 2)
  
  htmltools::HTML(paste0("<b>Obra:</b> ", obra_2,
                         "<b>Inversión:</b> ", inversion, "<br>",
                         "<b>Pertinencia:</b> ", pertinencia))

})

# Construir etiquetas para los marcadores
puntos_labels <- lapply(1:nrow(puntos_c_extract), function(i) {
  obra <- puntos_c_extract$Obra[i]
  
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
  
  inversion <- ifelse(!is.na(puntos_c_extract$Inversión[i]), paste0("$", formatC(puntos_c_extract$Inversión[i], big.mark = ",", format = "d")), "-")
  pertinencia <- round(puntos_c_extract$extract[i], 2)
  
  htmltools::HTML(paste0("<b>Obra:</b> ", obra_2,
                         "<b>Inversión:</b> ", inversion, "<br>",
                         "<b>Pertinencia:</b> ", pertinencia))
})
