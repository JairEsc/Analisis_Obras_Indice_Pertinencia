library(leaflet)
library(sf)
"../../Repositorios/Municipal_Inversion_3_años/Datos/SPIDUS_INHIFE_TRY.geojson" |> st_read() |> 
  st_transform(st_crs("EPSG:4326"))->obras_sipdus
obras_sipdus_agua=obras_sipdus |> 
  dplyr::filter(Rubro%in%c("Infraestructura Hídrica"))|> 
  dplyr::mutate(Geometria_tipo=
                  ifelse(st_geometry_type(geometry)%in%c("MULTILINESTRING","LINESTRING"),"Línea","Punto")
  ) |> 
  dplyr::filter(!grepl(pattern = "sanitari",x = Obra,ignore.case = T)) |> 
  dplyr::filter(!grepl(pattern = "aguas",x = Obra,ignore.case = T)) |> 
  dplyr::filter(!grepl(pattern = "saneamient",x = Obra,ignore.case = T))  

obras_sipdus_agua_multilinea=obras_sipdus_agua |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
  st_cast("LINESTRING")
obras_sipdus_agua_linea=obras_sipdus_agua |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
obras_sipdus_agua_multipunto=obras_sipdus_agua |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
obras_sipdus_agua_punto=obras_sipdus_agua |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))

obras_sipdus_agua_linea=rbind(obras_sipdus_agua_linea,
                              obras_sipdus_agua_multilinea)

obras_sipdus_agua_punto=rbind(obras_sipdus_agua_punto,
                              obras_sipdus_agua_multipunto |> st_cast("POINT"))

obras_sipdus_agua_linea |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_agua_linea.geojson",driver = "GeoJSON")
obras_sipdus_agua_punto |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_agua_punto.geojson",driver = "GeoJSON")
