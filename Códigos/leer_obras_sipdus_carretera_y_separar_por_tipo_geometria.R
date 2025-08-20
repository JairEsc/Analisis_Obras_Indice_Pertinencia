library(leaflet)
"../../Repositorios/Municipal_Inversion_3_años/Datos/SPIDUS_INHIFE_TRY.geojson" |> st_read()->obras_sipdus
obras_sipdus_carretera=obras_sipdus |> 
  dplyr::filter(Rubro%in%c("Infraestructura Carretera","Vialidades Urbanas"))|> 
  dplyr::mutate(Geometria_tipo=
                  ifelse(st_geometry_type(geometry)%in%c("MULTILINESTRING","LINESTRING"),"Línea","Punto")
  )
obras_sipdus_carretera_multilinea=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
  st_cast("LINESTRING")
#obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
obras_sipdus_carretera_linea=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
obras_sipdus_carretera_multipunto=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))
obras_sipdus_carretera_punto=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTPOINT'))

obras_sipdus_carretera_linea=rbind(obras_sipdus_carretera_linea,
                                   obras_sipdus_carretera_multilinea)
# leaflet() |> addTiles(options = leaflet::tileOptions(opacity = 0.5)) |> 
#   addRasterImage(rasters[[1]],opacity = 0.5,group = "Accesibilidad") |> 
#   addRasterImage(rasters[[2]],opacity = 0.5,group = "Distancia centros de trabajo") |> 
#   addRasterImage(rasters[[3]],opacity = 0.5,group = "Distancia áreas naturales protegidas") |> 
#   addRasterImage(rasters[[4]],opacity = 0.5,group = "Distancia Escuelas") |> 
#   addRasterImage(rasters[[5]],opacity = 0.5,group = "Distancia Hospitales") |> 
#   addRasterImage(rasters[[6]],opacity = 0.5,group = "Distancia a localidades marginadas") |> 
#   addRasterImage(rasters[[7]],opacity = 0.5,group = "Distancia a ZAP") |> 
#   addPolylines(data=obras_sipdus_carretera_linea
#                ,group = "Obras_tipo_linea"
#                ,label = obras_sipdus_carretera_linea$Obra
#   ) |> 
#   addPolylines(data=obras_sipdus_carretera_multilinea 
#                ,group = "Obras_tipo_multilinea"
#                ,label = obras_sipdus_carretera_multilinea$Obra
#   ) |>
#   # addMarkers(data=obras_sipdus_carretera_punto,group = "Obras_tipo_punto",
#   #            label = obras_sipdus_carretera_punto$Obra) |> 
#   addMarkers(data=obras_sipdus_carretera_multipunto,group = "Obras_tipo_multipunto",
#              label = obras_sipdus_carretera_multipunto$Obra) |> 
#   addLayersControl(overlayGroups = c("Accesibilidad",
#                                      "Distancia centros de trabajo",
#                                      "Distancia áreas naturales protegidas",
#                                      "Distancia Escuelas",
#                                      "Distancia Hospitales",
#                                      "Distancia a localidades marginadas",
#                                      "Distancia a ZAP"
#                                      ,"Obras_tipo_linea","Obras_tipo_multilinea","Obras_tipo_multipunto"))

