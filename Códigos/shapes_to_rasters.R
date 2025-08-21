##Para los shapes que sean necesarios rasterizar.
library(raster)
library(sf)
library(terra)

rasters=list.files("Inputs/Drive/",pattern = ".tif$",full.names = T) |> lapply(raster::raster)
rasters |> lapply(plot)

shapes=list.files("Inputs/Drive/VIABILIDAD DE OBRAS/",pattern = ".shp$",full.names = T) |> 
  lapply(\(x){x |> read_sf() |> st_transform(st_crs(municipios))})
shapes_list=list.files("Inputs/Drive/VIABILIDAD DE OBRAS/",pattern = ".shp$",full.names = T)

shapes_list[1]

centros_trabajo_raster=terra::distance(base, shapes[[1]]) 
centros_trabajo_raster=mask(centros_trabajo_raster,municipios)
centros_trabajo_raster |> values() |> hist()
centros_trabajo_raster |> plot()

#centros_trabajo_raster |> writeRaster("Inputs/Rasters_Generados_en_R/centros_trabajo_raster_distance.tif")
##
shapes_list[2]

densidad_localidad_raster=terra::distance(base, shapes[[2]]) 
densidad_localidad_raster=mask(densidad_localidad_raster,municipios)
densidad_localidad_raster |> values() |> hist()
densidad_localidad_raster |> plot()


##
shapes_list[7]



distancia_loc_marginadas_raster=terra::distance(base, (shapes[[7]] |> dplyr::filter(GM_2020%in%c("Alto","Muy alto")))) 
distancia_loc_marginadas_raster=mask(distancia_loc_marginadas_raster,municipios)
distancia_loc_marginadas_raster |> values() |> hist()
distancia_loc_marginadas_raster |> plot()

#distancia_loc_marginadas_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_loc_marginadas_grados_alto_y_muy_alto_raster_distance.tif")
##
shapes_list[3]



distancia_escuelas_raster=terra::distance(base, shapes[[3]]) 
distancia_escuelas_raster=mask(distancia_escuelas_raster,municipios)
distancia_escuelas_raster |> values() |> hist()
distancia_escuelas_raster |> plot()

#distancia_escuelas_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_escuelas_distance.tif")

##
shapes_list[4]



distancia_hospitales_raster=terra::distance(base, shapes[[4]]) 
distancia_hospitales_raster=mask(distancia_hospitales_raster,municipios)
distancia_hospitales_raster |> values() |> hist()
distancia_hospitales_raster |> plot()

#distancia_hospitales_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_hospitales_distance.tif")

##
shapes_list[11]



distancia_ZAP_raster=terra::distance(base, shapes[[11]]) 
distancia_ZAP_raster=mask(distancia_ZAP_raster,municipios)
distancia_ZAP_raster |> values() |> hist()
distancia_ZAP_raster |> plot()

#distancia_ZAP_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_ZAP_distance.tif")


##
shapes_list[1]



distancia_ANP_raster=terra::distance(base, shapes[[1]]) 
distancia_ANP_raster=mask(distancia_ANP_raster,municipios)
distancia_ANP_raster |> values() |> hist()
distancia_ANP_raster |> plot()

#distancia_ANP_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_ANP_distance.tif")
##
shapes_list[11]
################En este bloque generamos los rasters de distancia a localidades con más bajo acceso a agua entubada y a drenaje sanitario.

#Leemos poligonos
viviendas_s_acceso_agua_urb=read_sf("../../Reutilizables/Demograficos/scince/loc_urb.shp") |> 
  st_transform(st_crs(municipios)) |> 
  merge(read_sf("../../Reutilizables/Demograficos/scince/tablas/cpv2020_loc_urb_vivienda.dbf"),by='CVEGEO')
#Filtramos datos censurados
viviendas_s_acceso_agua_urb=
  viviendas_s_acceso_agua_urb |> dplyr::select(CVEGEO,NOM_MUN,VIV18_R,VIV24_R) |> 
  dplyr::filter(VIV18_R!=-6) |> 
  dplyr::filter(VIV24_R!=-6) |> 
  dplyr::mutate(Localidad='Urbana')

#Lo mismo para rurales
viviendas_s_acceso_agua_rural=read_sf("../../Reutilizables/Demograficos/scince/loc_rur.shp") |> 
  st_transform(st_crs(municipios)) |> 
  merge(read_sf("../../Reutilizables/Demograficos/scince/tablas/cpv2020_loc_rur_vivienda.dbf"),by='CVEGEO')
viviendas_s_acceso_agua_rural=
  viviendas_s_acceso_agua_rural |> dplyr::select(CVEGEO,NOM_MUN,VIV18_R,VIV24_R) |> 
  dplyr::filter(VIV18_R!=-6) |> 
  dplyr::filter(VIV24_R!=-6)  |> 
  dplyr::mutate(Localidad='Rural')
#Le ponemos un buffer para hacerlo polígono y para darle un área de influencia
viviendas_s_acceso_agua_rural=viviendas_s_acceso_agua_rural |> st_buffer(200)
#Los unimos porque ya son poligonos 
viviendas_s_acceso_agua=rbind(viviendas_s_acceso_agua_rural,viviendas_s_acceso_agua_urb)

#####Revisamos los quantiles de no-cobertura para cada tipo de localidad
quantile(viviendas_s_acceso_agua$VIV18_R[viviendas_s_acceso_agua$Localidad=='Urbana'],c(1:20)/20)
quantile(viviendas_s_acceso_agua$VIV18_R[viviendas_s_acceso_agua$Localidad=='Rural'],c(1:20)/20)
##Notamos que en el caso de localidades urbanas, más del 95% tiene muy buena cobertura (pocas viviendas sin acceso a agua entubada)
###Para definir las localidades urbanas con menor acceso a agua entubada, tomamos el .90-quantil (i.e. más de 3.82% de viviendas sin acceso a agua entubada)
##Notamos que en el caso de localidades rurales, el 65% tiene buena cobertura (más de 95% de viviendas con acceso a agua entubada)
###Para definir las localidades rurales con menor acceso a agua entubada, tomamos el .75-quantil(i.e. más de 9.7% de viviendas sin acceso a agua entubada)

##Entonces aplicamos los filtros y generamos los rasters


# viviendas_s_acceso_agua_raster=viviendas_s_acceso_agua |> dplyr::filter( (VIV18_R>=3.82 & Localidad=='Urbana') |
#                                             (VIV18_R>=9.7 & Localidad=='Rural')) |> 
#   terra::vect() |> terra::rasterize(base,field = "VIV18_R", fun = "mean")
viviendas_s_acceso_agua_raster=terra::distance(base, viviendas_s_acceso_agua|> dplyr::filter( (VIV18_R>=3.82 & Localidad=='Urbana') |
                                             (VIV18_R>=9.7 & Localidad=='Rural')) )
viviendas_s_acceso_agua_raster=mask(viviendas_s_acceso_agua_raster,municipios)
viviendas_s_acceso_agua_raster |> writeRaster("Inputs/Rasters_Generados_en_R/porc_viv_sin_acceso_agua_entubada_VIV18_R_buffer200m_a_loc_rurales.tif",overwrite=T)

##Repetimos para acceso a drenaje
#####Revisamos los quantiles de no-cobertura para cada tipo de localidad
quantile(viviendas_s_acceso_agua$VIV24_R[viviendas_s_acceso_agua$Localidad=='Urbana'],c(1:20)/20)
quantile(viviendas_s_acceso_agua$VIV24_R[viviendas_s_acceso_agua$Localidad=='Rural'],c(1:20)/20)
##Notamos que en el caso de localidades urbanas, más del 90% tiene muy buena cobertura (pocas viviendas sin acceso a drenaje)
###Para definir las localidades urbanas con menor acceso a drenaje, tomamos el .90-quantil (i.e. más de 3.58% de viviendas sin acceso a drenaje)
##Notamos que en el caso de localidades rurales, el 40% tiene buena cobertura (más de 95% de viviendas con acceso a drenaje)
###Para definir las localidades rurales con menor acceso a drenaje, tomamos el .60-quantil(i.e. más de 11% de viviendas sin acceso a drenaje)

##Entonces aplicamos los filtros y generamos los rasters


# viviendas_s_acceso_drenaje_raster=viviendas_s_acceso_agua |> dplyr::filter( (VIV24_R>=3.58 & Localidad=='Urbana') |
#                                                                            (VIV24_R>=11 & Localidad=='Rural')) |> 
#   terra::vect() |> terra::rasterize(base,field = "VIV24_R", fun = "mean")

viviendas_s_acceso_drenaje_raster=terra::distance(base, viviendas_s_acceso_agua|> dplyr::filter( (VIV24_R>=3.58 & Localidad=='Urbana') |
                                                                            (VIV24_R>=11 & Localidad=='Rural')))
viviendas_s_acceso_drenaje_raster=mask(viviendas_s_acceso_drenaje_raster,municipios)

viviendas_s_acceso_drenaje_raster|> writeRaster("Inputs/Rasters_Generados_en_R/porc_viv_sin_acceso_drenaje_VIV24_R_buffer200m_a_loc_rurales.tif",overwrite=T)


###########Aquí termina el bloque.


distancia_ANP_raster=terra::distance(base, shapes[[11]])
distancia_ANP_raster=mask(distancia_ANP_raster,municipios)
distancia_ANP_raster |> values() |> hist()
distancia_ANP_raster |> plot()

#distancia_ANP_raster |> writeRaster("Inputs/Rasters_Generados_en_R/distancia_ANP_distance.tif")

Accesibilidad_cabeceras_mun=raster("Inputs/Drive/Distancia_Cabeceras_Municipales.tif")
library(raster)


#[1] 40 40

#aggregate from 40x40 resolution to 120x120 (factor = 3)
meuse.raster.aggregate <- aggregate(meuse.raster, fact=3)
res(meuse.raster.aggregate)
#[1] 120 120

#disaggregate from 40x40 resolution to 10x10 (factor = 4)
Accesibilidad_cabeceras_mun <- disaggregate(Accesibilidad_cabeceras_mun, fact=3)
Accesibilidad_cabeceras_mun=mask(Accesibilidad_cabeceras_mun,municipios)


Accesibilidad_cabeceras_mun|> writeRaster("Inputs/Rasters_Generados_en_R/Accesibilidad_cabeceras_mun_dissagregate.tif")
