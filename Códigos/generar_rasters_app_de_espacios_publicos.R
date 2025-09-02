"Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Espacios publicos/diff_no_afectado_menos_afectado.tif" |> raster() ->precepcion_espacios_publicos
precepcion_espacios_publicos|> plot()
#leaflet() |> addTiles() |> addRasterImage(precepcion_espacios_publicos)
scale_m1_1=function(x){
  return (2*((x-raster::minValue(x))/(raster::maxValue(x)-raster::minValue(x))-1/2))
}
#leaflet() |> addTiles() |> addRasterImage(z_lista[[1]])
#raster::maxValue(z5)
precepcion_espacios_publicos[abs(precepcion_espacios_publicos)<1e-5]=NA
precepcion_espacios_publicos |> plot()
precepcion_espacios_publicos=precepcion_espacios_publicos |> scale_m1_1()
precepcion_espacios_publicos[is.na(precepcion_espacios_publicos)]=0
precepcion_espacios_publicos=terra::mask(precepcion_espacios_publicos,municipios)
precepcion_espacios_publicos |> plot()
precepcion_espacios_publicos |> aggregate(3) |> writeRaster("Inputs/Rasters_Generados_en_R/rasters_app_espacios_publicos/j_percepcion_parques_y_jardines.tif",overwrite=T)
