###Generar los rasters de educacion
##Se quedan los mismos rasters excepto
source("Códigos/leer_rasters_generados_en_r.R")
edu_precepcion="Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/diff_mejor_menos_peor.tif" |> raster() 
edu_precepcion[abs(edu_precepcion)<1e-5]=NA
edu_precepcion |> plot()
edu_precepcion|> scale_m1_1() |> plot()
scale_m1_1=function(x){
  return (2*((x-raster::minValue(x))/(raster::maxValue(x)-raster::minValue(x))-1/2))
}
edu_precepcion=edu_precepcion |> scale_m1_1()
edu_precepcion[is.na(edu_precepcion)]=0
edu_precepcion=terra::mask(edu_precepcion,municipios)
edu_precepcion |> plot()
edu_precepcion |> aggregate(3) |> writeRaster("Inputs/Rasters_Generados_en_R/rasters_app_educacion/j_percepcion_infra_edu.tif",overwrite=T)
