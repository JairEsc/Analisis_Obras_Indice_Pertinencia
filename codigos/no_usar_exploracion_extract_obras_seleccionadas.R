#######
#Ejercicio de exploración 

obras_solicitadas=openxlsx::read.xlsx("Inputs/Nivel de uso/Obras solicitadas/OPINIÓN DE OBRAS SIPDUS.xlsx",startRow = 1,sheet = "Hoja 1")
obras_NO_solicitadas=obras_solicitadas |> 
  dplyr::filter(!is.na(X14))
obras_solicitadas=obras_solicitadas |> 
  dplyr::filter(is.na(X14))

obras_solicitadas=obras_solicitadas |> 
  merge(obras_sipdus |> dplyr::select(ID_OBRA,geometry),by.x='ID',by.y='ID_OBRA',all.x=T) |> st_as_sf() |> 
  dplyr::filter(st_geometry_type(geometry)%in%c("MULTIPOINT","POINT","MULTILINESTRING","LINESTRING"))
obras_NO_solicitadas=obras_NO_solicitadas |> 
  merge(obras_sipdus|> dplyr::select(ID_OBRA,geometry),by.x='ID',by.y='ID_OBRA',all.x=T) |> st_as_sf() |> 
  dplyr::filter(st_geometry_type(geometry)%in%c("MULTIPOINT","POINT","MULTILINESTRING","LINESTRING"))

obras_solicitadas$geometry |> st_geometry_type()
library(leaflet)
leaflet() |> addTiles() |> 
  addMarkers(data = obras_NO_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT","POINT") ) |> st_cast("POINT"),group = "ONSP")
leaflet() |> addTiles() |> 
  addMarkers(data = obras_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT","POINT") ) |> st_cast("POINT"),group = "OSP")
leaflet() |> addTiles() |> 
  addPolylines(data = obras_NO_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTILINESTRING","LINESTRING") ) |> st_cast("LINESTRING"))
leaflet() |> addTiles() |> 
  addPolylines(data = obras_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTILINESTRING","LINESTRING") ) |> st_cast("LINESTRING"))

obras_NO_solicitadas_puntos=obras_NO_solicitadas |> 
  dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT")) |> st_cast("POINT") |> st_transform(st_crs("EPSG:32614"))
obras_NO_solicitadas_lineas=obras_NO_solicitadas |> 
  dplyr::filter(st_geometry_type(geometry)%in% c("MULTILINESTRING","LINESTRING")) |> st_cast("LINESTRING") |> st_transform(st_crs("EPSG:32614"))

obras_NO_solicitadas_puntos_extract <-rasters_full |> lapply(\(x){raster::extract(x, obras_NO_solicitadas_puntos,
                                                       method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                                       fun = mean, na.rm = TRUE)}) 
obras_NO_solicitadas_lineas_extract <-rasters_full |> lapply(\(x){raster::extract(x, obras_NO_solicitadas_lineas,
                                                       method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                                       fun = mean, na.rm = TRUE)}) 

obras_solicitadas_puntos=obras_solicitadas |> 
  dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT","POINT")) |> st_cast("POINT") |> st_transform(st_crs("EPSG:32614"))
obras_solicitadas_lineas=obras_solicitadas |> 
  dplyr::filter(st_geometry_type(geometry)%in% c("MULTILINESTRING","LINESTRING")) |> st_cast("LINESTRING") |> st_transform(st_crs("EPSG:32614"))

obras_solicitadas_puntos_extract <-rasters_full |> lapply(\(x){raster::extract(x, obras_solicitadas_puntos,
                                                       method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                                       fun = mean, na.rm = TRUE)}) 
obras_solicitadas_lineas_extract <-rasters_full |> lapply(\(x){raster::extract(x, obras_solicitadas_lineas,
                                                       method = 'simple', buffer = NULL, small = FALSE, cellnumbers = FALSE,
                                                       fun = mean, na.rm = TRUE)})
for(x in 1:16){
  obras_solicitadas_lineas[,names(rasters_full[[x]])]=obras_solicitadas_lineas_extract[[x]]
}
for(x in 1:16){
  obras_solicitadas_puntos[,names(rasters_full[[x]])]=obras_solicitadas_puntos_extract[[x]]
}
for(x in 1:16){
  obras_NO_solicitadas_lineas[,names(rasters_full[[x]])]=obras_NO_solicitadas_lineas_extract[[x]]
}
for(x in 1:16){
  obras_NO_solicitadas_puntos[,names(rasters_full[[x]])]=obras_NO_solicitadas_puntos_extract[[x]]
}
  

###########################################

leaflet() |> addTiles() |> addPolylines(data=raster::rasterToContour(rasters_full[[1]]   ) |> st_as_sf() |> st_transform(st_crs("EPSG:4326"))) |> 
  addMarkers(data = obras_NO_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT","POINT") ) |> st_cast("POINT"),group = "ONSP") |>   
  addMarkers(data = obras_solicitadas |> dplyr::filter(st_geometry_type(geometry)%in% c("MULTIPOINT","POINT") ) |> st_cast("POINT"),group = "OSP") |> 
  addLayersControl(overlayGroups = c("ONSP","OSP"))




############