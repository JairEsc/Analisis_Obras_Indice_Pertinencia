library(leaflet)
library(sf)
"../../Repositorios/Municipal_Inversion_3_años/Datos/SPIDUS_INHIFE_TRY.geojson" |> st_read() |> 
  st_transform(st_crs("EPSG:4326"))->obras_sipdus
obras_sipdus_multilinea=obras_sipdus |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |>
  st_cast("LINESTRING")
#obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
obras_sipdus_linea=obras_sipdus |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
obras_sipdus_multipunto=obras_sipdus |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
obras_sipdus_punto=obras_sipdus |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))

obras_sipdus_linea=rbind(obras_sipdus_linea,
                         obras_sipdus_multilinea)

obras_sipdus_punto=rbind(obras_sipdus_punto,
                         obras_sipdus_multipunto |> st_cast("POINT"))
# obras_sipdus_carretera=obras_sipdus |> 
#   dplyr::filter(Rubro%in%c("Infraestructura Carretera","Vialidades Urbanas"))|> 
#   dplyr::mutate(Geometria_tipo=
#                   ifelse(st_geometry_type(geometry)%in%c("MULTILINESTRING","LINESTRING"),"Línea","Punto")
#   )
# obras_sipdus_carretera_multilinea=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
#   st_cast("LINESTRING")
# #obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
# obras_sipdus_carretera_linea=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
# obras_sipdus_carretera_multipunto=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
# obras_sipdus_carretera_punto=obras_sipdus_carretera |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))
# 
# obras_sipdus_carretera_linea=rbind(obras_sipdus_carretera_linea,
#                                    obras_sipdus_carretera_multilinea)
# 
# obras_sipdus_carretera_punto=rbind(obras_sipdus_carretera_punto,
#                                    obras_sipdus_carretera_multipunto |> st_cast("POINT"))
# #obras_sipdus_carretera_linea |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_linea.geojson",driver = "GeoJSON")
#obras_sipdus_carretera_punto |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_punto.geojson",driver = "GeoJSON")
###Vamos a hacer un simulacro de extract_value del raster de pertinencia
# source("Códigos/leer_rasters_generados_en_r.R")
# 
# origin(rasters[[1]])=origin(rasters[[2]])
# extent(rasters[[1]])=extent(rasters[[2]])
# rasters[[8]]=min(rasters[[8]],rasters[[9]],na.rm = T)rasters=rasters[c(1:8,10)]
# rasters_list_names[8]='Distancia a localidades con bajo acceso a agua entubada o drenaje sanitario'
# rasters_list_names=rasters_list_names[c(1:8,10)]
# for(i in 1:length(rasters)){
#   raster_vals <- values(rasters[[i]])
#   raster_vals <- raster_vals[!is.na(raster_vals)]
#   
#   q1 <- quantile(raster_vals, 0.25)
#   q3 <- quantile(raster_vals, 0.75)
#   iqr <- q3 - q1
#   
#   upper_limit <- q3 + 1.5 * iqr
#   lower_limit <- q1 - 1.5 * iqr
#   
#   rasters[[i]] <- clamp(rasters[[i]], 
#                         lower = lower_limit, 
#                         upper = upper_limit, 
#                         useValues=TRUE) |> scale()
# }
# 
# 
# weights=c(9,3,0,0,5,8,7,0,1)
# weights=weights/sum(weights)
# weights[1]=-weights[1]
# weights=-weights
# 
# pertinencia=rasters[[1]]*weights[1]+rasters[[2]]*weights[2]+
#   rasters[[3]]*weights[3]+rasters[[4]]*weights[4]+
#   rasters[[5]]*weights[5]+rasters[[6]]*weights[6]+
#   rasters[[7]]*weights[7]+rasters[[8]]*weights[8]+
#   rasters[[9]]*weights[9]
# 
# pertinencia |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/pertinencia_default.tif",overwrite=T)
# 
# 
# pertinencia |> raster::extract(obras_sipdus_carretera_punto, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
#                                fun=mean, na.rm=TRUE)->z
# z[z |> is.na()]=0
# obras_sipdus_carretera_punto$extract=z
# pertinencia |> raster::extract(obras_sipdus_carretera_linea, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
#                                fun=mean, na.rm=TRUE)->z
# z[z |> is.na()]=0
# obras_sipdus_carretera_linea$extract=z
# library(leaflet)
# leaflet() |> addTiles() |> addRasterImage() |> addMarkers(data=obras_sipdus_carretera_punto[z |> is.na(),])
# plot(pertinencia)
# plot(obras_sipdus_carretera_punto$geometry,add=T,col='red')
# 
# 
# obras_sipdus_carretera_linea |> dplyr::relocate(extract,.before = geometry) |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_linea_extract.geojson",driver = "GeoJSON")
# obras_sipdus_carretera_punto |> dplyr::relocate(extract,.before = geometry) |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_punto_extract.geojson",driver = "GeoJSON")



####Leer obras viales en puntos y lineas
# st_read("Inputs/Rasters_Generados_en_R/Otros/obras_linea_extract.geojson") |> st_transform(st_crs("EPSG:4326"))->lineas_c_extract
# st_read("Inputs/Rasters_Generados_en_R/Otros/obras_punto_extract.geojson")|> st_transform(st_crs("EPSG:4326"))->puntos_c_extract
# 
# (lineas_c_extract |> dplyr::filter(grepl(pattern = "Reconstrucci||Mejoram||Rehabili",Obra )))$Obra
# lineas_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){x[[1]]%in%c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#                                                                                                                           "Re-encarpetamiento","Reubicación")}) 
# c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#   "Re-encarpetamiento","Reubicación")
# lineas_c_extract$extract[lineas_c_extract$extract |> is.na()]=0
# 
# lineas_vialidades_mejora=lineas_c_extract[lineas_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){x[[1]]%in%c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#                                                                                                                                                                     "Re-encarpetamiento","Reubicación")}) 
# ,]
# puntos_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){x[[1]]}) |> unique()
# puntos_vialidades_mejora=puntos_c_extract[puntos_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){x[[1]]%in%c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#                                                                                                                                                                     "Re-encarpetamiento","Reubicación")}) 
# ,]
# lineas_vialidades_nuevas=lineas_c_extract[lineas_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){!x[[1]]%in%c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#                                                                                                                                                                     "Re-encarpetamiento","Reubicación")}) 
# ,]
# puntos_vialidades_nuevas=puntos_c_extract[puntos_c_extract$Obra |> sapply(\(x){(x |> strsplit(split = " "))},simplify = T,USE.NAMES = F) |> sapply(\(x){!x[[1]]%in%c("Ampliación","Rehabilitación","Mejoramiento","Modernización","Reconstrucción","Reconstruccíon","Rehabilitacion","Rehabilitación",
#                                                                                                                                                                     "Re-encarpetamiento","Reubicación")}) 
# ,]

# puntos_vialidades_nuevas |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_vial_punto_nueva.geojson",driver = "GeoJSON")
# lineas_vialidades_nuevas |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_vial_linea_nueva.geojson",driver = "GeoJSON")
# puntos_vialidades_mejora |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_vial_punto_mejora.geojson",driver = "GeoJSON")
# lineas_vialidades_mejora |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_vial_linea_mejora.geojson",driver = "GeoJSON")


# 
# 
# ############Espacios Públicos
# obras_sipdus_espacios_publicos=obras_sipdus |> 
#   dplyr::filter(Rubro%in%c("Espacios Públicos"))|> 
#   dplyr::mutate(Geometria_tipo=
#                   ifelse(st_geometry_type(geometry)%in%c("MULTILINESTRING","LINESTRING"),"Línea","Punto")
#   )
# obras_sipdus_salud=obras_sipdus_espacios_publicos |> 
#   dplyr::filter(grepl(pattern = "Salud",x = Obra,ignore.case = T)|grepl(pattern = "Hospital",x=Obra,ignore.case = T)|grepl(pattern = "Centro de Rehabilitación",x=Obra,ignore.case = T)) 
# obras_sipdus_espacios_publicos=obras_sipdus_espacios_publicos |> 
#   dplyr::filter(!grepl(pattern = "Salud",x = Obra,ignore.case = T)) |> 
#   dplyr::filter(!grepl(pattern = "Hospital",x=Obra,ignore.case = T)) |> 
#   dplyr::filter(!grepl(pattern = "Centro de Rehabilitación",x=Obra,ignore.case = T)) 
# 
# obras_sipdus_espacios_publicos_multilinea=obras_sipdus_espacios_publicos |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
#   st_cast("LINESTRING")
# #obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
# obras_sipdus_espacios_publicos_linea=obras_sipdus_espacios_publicos |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
# obras_sipdus_espacios_publicos_multipunto=obras_sipdus_espacios_publicos |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
# obras_sipdus_espacios_publicos_punto=obras_sipdus_espacios_publicos |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))
# 
# obras_sipdus_espacios_publicos_linea=rbind(obras_sipdus_espacios_publicos_linea,
#                                            obras_sipdus_espacios_publicos_multilinea)
# 
# obras_sipdus_espacios_publicos_punto=rbind(obras_sipdus_espacios_publicos_punto,
#                                            obras_sipdus_espacios_publicos_multipunto |> st_cast("POINT"))
# 
# obras_sipdus_espacios_publicos_linea |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_espacios_publicos_linea.geojson",driver = "GeoJSON")
# obras_sipdus_espacios_publicos_punto  |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_espacios_publicos_punto.geojson",driver = "GeoJSON")
# 
# #########Salud
# obras_sipdus_salud_multilinea=obras_sipdus_salud |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
#   st_cast("LINESTRING")
# #obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
# obras_sipdus_salud_linea=obras_sipdus_salud |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
# obras_sipdus_salud_multipunto=obras_sipdus_salud |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
# obras_sipdus_salud_punto=obras_sipdus_salud |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))
# 
# obras_sipdus_salud_linea=rbind(obras_sipdus_salud_linea,
#                                obras_sipdus_salud_multilinea)
# 
# obras_sipdus_salud_punto=rbind(obras_sipdus_salud_punto,
#                                obras_sipdus_salud_multipunto |> st_cast("POINT"))
# 
# #obras_sipdus_salud_linea |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_salud_linea.geojson",driver = "GeoJSON")
# obras_sipdus_salud_punto  |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_salud_punto.geojson",driver = "GeoJSON")
# 
# 
# 
# ###Educacion 
# obras_sipdus_educacion=obras_sipdus |> 
#   dplyr::filter(Rubro%in%c("Espacios Educativos"))|> 
#   dplyr::mutate(Geometria_tipo=
#                   ifelse(st_geometry_type(geometry)%in%c("MULTILINESTRING","LINESTRING"),"Línea","Punto")
#   )
# 
# 
# obras_sipdus_educacion_multilinea=obras_sipdus_educacion |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTILINESTRING')) |> 
#   st_cast("LINESTRING")
# #obras_sipdus_carretera_multilinea=obras_sipdus_carretera_multilinea[1:572,]
# obras_sipdus_educacion_linea=obras_sipdus_educacion |> dplyr::filter(st_geometry_type(geometry)%in%c('LINESTRING'))
# obras_sipdus_educacion_multipunto=obras_sipdus_educacion |> dplyr::filter(st_geometry_type(geometry)%in%c('MULTIPOINT'))
# obras_sipdus_educacion_punto=obras_sipdus_educacion |> dplyr::filter(st_geometry_type(geometry)%in%c('POINT'))
# 
# obras_sipdus_educacion_linea=rbind(obras_sipdus_educacion_linea,
#                                    obras_sipdus_educacion_multilinea)
# 
# obras_sipdus_educacion_punto=rbind(obras_sipdus_educacion_punto,
#                                    obras_sipdus_educacion_multipunto |> st_cast("POINT"))
# 
# #obras_sipdus_espacios_publicos_linea |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_espacios_publicos_linea.geojson",driver = "GeoJSON")
# obras_sipdus_educacion_punto  |> st_write("Inputs/Rasters_Generados_en_R/Otros/obras_educacion_punto.geojson",driver = "GeoJSON")
