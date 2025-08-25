library(sf)

st_read("Inputs/Rasters_Generados_en_R/Otros/obras_linea_extract.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_c_extract
st_read("Inputs/Rasters_Generados_en_R/Otros/obras_punto_extract.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_c_extract


lineas_labels <- lapply(1:nrow(lineas_c_extract), function(i) {
  obra <- lineas_c_extract$Obra[i]
  inversion <- ifelse(!is.na(puntos_c_extract$Inversión[i]),paste0("$",formatC(puntos_c_extract$Inversión[i], big.mark = ",",format = "d")),"-")
  pertinencia <- round(lineas_c_extract$extract[i], 2)
  
  htmltools::HTML(paste0("<b>Obra:</b> ", obra, "<br>",
                         "<b>Inversión:</b> ", inversion, "<br>",
                         "<b>Pertinencia:</b> ", pertinencia))
})

# Construir etiquetas para los marcadores
puntos_labels <- lapply(1:nrow(puntos_c_extract), function(i) {
  obra <- puntos_c_extract$Obra[i]
  inversion <- ifelse(!is.na(puntos_c_extract$Inversión[i]),paste0("$",formatC(puntos_c_extract$Inversión[i], big.mark = ",",format = "d")),"-")
  pertinencia <- round(puntos_c_extract$extract[i], 2)
  
  htmltools::HTML(paste0("<b>Obra:</b> ", obra, "<br>",
                         "<b>Inversión:</b> ", inversion, "<br>",
                         "<b>Pertinencia:</b> ", pertinencia))
})