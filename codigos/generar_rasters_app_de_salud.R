###Generar los rasters de salud
##Se quedan los mismos rasters excepto
source("Códigos/leer_rasters_generados_en_r.R")
salud_precepcion="Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Salud/diff_no_afectado_menos_afectado.tif" |> raster() 
salud_precepcion[abs(salud_precepcion)<1e-5]=NA
salud_precepcion |> plot()
salud_precepcion|> scale_m1_1() |> plot()
scale_m1_1=function(x){
  return (2*((x-raster::minValue(x))/(raster::maxValue(x)-raster::minValue(x))-1/2))
}
salud_precepcion=salud_precepcion |> scale_m1_1()
salud_precepcion[is.na(salud_precepcion)]=0
salud_precepcion=terra::mask(salud_precepcion,municipios)
salud_precepcion |> plot()
salud_precepcion |> aggregate(3)|> writeRaster("Inputs/Rasters_Generados_en_R/rasters_app_salud/j_pprecepcion_infraestructura_salud.tif",overwrite=T)
