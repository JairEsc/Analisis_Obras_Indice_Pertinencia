library(raster)
rasters_list=list.files("Inputs/Rasters_Generados_en_R/",pattern = ".tif",full.names = T) 
#rasters=rasters_list |> lapply(raster)
rasters_list_names=c("Accesibilidad carretera a cabeceras municipales",
"Distancia a Centros de Trabajo",
"Áreas Naturales Protegidas (Distancia)",
"Distancia a Escuelas",
"Distancia a Hospitales",
"Distancia a localidades Marginadas (Alta y Muy alta)",
"Zonas Prioritarias (Distancia)",
"Distancia a localidades con bajo acceso a agua entubada",
"Distancia a localidades con bajo acceso a drenaje sanitario",
"Secciones Electorales (porcentaje de votos al partido)")
rasters_list_names[8]='Distancia a localidades con bajo acceso a agua entubada o drenaje sanitario'
rasters_list_names=rasters_list_names[c(1:8,10)]
rasters_list_names[[10]]="Percepción infraestructura vial"##Caso por default
descripciones_minimas=c(
  "Se mide en distancia. Mayor valor significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a centros de trabajo significa más pertinente la obra",
  "Se mide en distancia. Mayor distancia una ANP significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a escuelas significa más pertinente la obra",
  "Se mide en distancia. Menor distancia a hospitales significa más pertinente la obra",
  "Se mide en log-distancia. Menor distancia a localidades marginadas significa más pertinente la obra",
  "Se mide en log-distancia. Menor valor significa obra específica de una ZAP. I.e. más pertinente la obra",
  "Se mide en log-distancia. Menor distancia significa obra específica de una localidad sin servicios públicos I.e. más pertinente la obra",
  "Se mide en porcentaje de votos. Mayor valor significa más votos al partido",
  "Se mide en percepción. Menor valor (percepción negativa) significa más pertinente la obra"
)

#crs(rasters[[1]])=crs(rasters[[2]])
