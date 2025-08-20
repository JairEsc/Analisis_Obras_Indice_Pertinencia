
rasters_list=list.files("Inputs/Rasters_Generados_en_R/",pattern = ".tif",full.names = T) 
rasters=rasters_list |> lapply(raster)

#Interpretaciones 
#rasters_list[[1]]
#Accesibilidad es la distancia en minutos a la cabecera municipal más cercana. 
#La interpretación natural es que entre mayor es el tiempo, más importante es mejorar la accesibilidad 

#+ es más pertinente.

crs(rasters[[1]])=crs(rasters[[2]])
