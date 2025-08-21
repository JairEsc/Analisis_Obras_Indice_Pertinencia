
rasters_list=list.files("Inputs/Rasters_Generados_en_R/",pattern = ".tif",full.names = T) 
rasters=rasters_list |> lapply(raster)
rasters_list_names=c("Accesibilidad carretera a cabeceras municipales","
Distancia a Centros de Trabajo","
Áreas Naturales Protegidas (Distancia)","
Distancia a Escuelas","
Distancia a Hospitales","
Distancia a localidades Marginadas (Alta y Muy alta)","
Zonas Prioritarias (Distancia)",
"
Distancia a localidades con bajo acceso a agua entubada","
Distancia a localidades con bajo acceso a drenaje sanitario",
"Secciones Electorales (porcentaje de votos al partido)")
#Interpretaciones 
#rasters_list[[1]]
#Accesibilidad es la distancia en minutos a la cabecera municipal más cercana. 
#La interpretación natural es que entre mayor es el tiempo, más importante es mejorar la accesibilidad 

#+ es más pertinente.

crs(rasters[[1]])=crs(rasters[[2]])
