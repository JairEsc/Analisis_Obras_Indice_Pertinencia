##Datos de encuesta
library(foreign)
cat_sav_2025=read.spss("../../Tareas/Aprobación_Gobernador_Encuesta/Encuestas/Base Hidalgo Julio 2025.SAV")
sav_2025=foreign::read.spss("../../Tareas/Aprobación_Gobernador_Encuesta/Encuestas/Base Hidalgo Julio 2025.SAV",to.data.frame = T)
#mujeres que tienen hijes están afiliadas al pan
variable_labels_2025 <- attr(sav_2025, "variable.labels")
sav_2025[is.na(sav_2025)]='-'
variables_2025=as.data.frame(matrix(c(colnames(sav_2025),variable_labels_2025),ncol=2,nrow=length(colnames(sav_2025)),byrow = F))



{
read_sf("Inputs/Rasters_Generados_en_R/Otros/EncuestaGober2025_filtracion.geojson")->
  respuestas_encuesta_2025
#install.packages("fastDummies")
library(fastDummies)
#datos = sf::read_sf("EncuestaGober2025_filtracion.geojson")

respuestas_encuesta_2025 = respuestas_encuesta_2025 |> 
  fastDummies::dummy_cols(select_columns = c("¿Cuál considera que es el principal problema que enfrenta el estado de Hidalgo?" ,"Y, ¿cuál considera que es el principal problema en su colonia?"))

respuestas_encuesta_2025 = respuestas_encuesta_2025 |> 
  dplyr::mutate(
    dplyr::across(
      .cols = -c(SbjNum,`Municipio INEGI`,PESOF,geometry),
      .fns = ~ dplyr::if_else(.x == 0, "No", as.character(.x))
    ),
    dplyr::across(
      .cols = -c(SbjNum,`Municipio INEGI`,PESOF,geometry),
      .fns = ~ dplyr::if_else(.x == "1", "Sí", as.character(.x))
    )
    
  ) |> st_as_sf()

#("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/raster_salud_afectados_por_mala_infra.tif" |> raster()) |> plot()
infrestructura = respuestas_encuesta_2025 |> 
  dplyr::select(SbjNum,`Municipio INEGI`,
                `¿Cuál considera que es el principal problema que enfrenta el estado de Hidalgo?_INFRAESTRUCTURA VIAL / MANTENIMIENTO EN CALLES`,
                `Y, ¿cuál considera que es el principal problema en su colonia?_INFRAESTRUCTURA VIAL / MANTENIMIENTO EN CALLES`,
                `En el último año, ¿usted se vio afectado/a por Invertir mucho tiempo en traslados por falta de carreteras?`,
                `En el último año, ¿usted se vio afectado/a por Caminos sin posibilidad de lograr velocidad por estar en mal estado?`,
                `En el último año, ¿usted se vio afectado/a por Deterioro de calles y avenidas por falta de mantenimiento?`,
                geometry)

agua = respuestas_encuesta_2025 |> 
  dplyr::select(SbjNum,`Municipio INEGI`, 
                `¿Cuál considera que es el principal problema que enfrenta el estado de Hidalgo?_ESCASEZ DE AGUA`,
                `En el último año, ¿usted se vio afectado/a por Falta de suministro de agua en la vivienda?`,
                geometry)

respuestas_encuesta_2025=respuestas_encuesta_2025 |> 
  dplyr::select(SbjNum,`En el último año, ¿usted se vio afectado/a por Falta de suministro de agua en la vivienda?`:`SALUD - En el último año, ¿usted se vio afectado/a por Mal equipamiento en las instalaciones?`,`¿Ha mejorado, se ha quedado igual o ha empeorado: Instalaciones de planteles educativos?`,`¿Ha mejorado, se ha quedado igual o ha empeorado: Equipamiento en aulas?`,`Y, ¿cuál considera que es el principal problema en su colonia?_AGUA POTABLE`:`Y, ¿cuál considera que es el principal problema en su colonia?_TRANSPORTE`,`¿Cuál considera que es el principal problema que enfrenta el estado de Hidalgo?_ADMINISTRACIÓN DEL GOBIERNO`:`¿Cuál considera que es el principal problema que enfrenta el estado de Hidalgo?_TRANSPORTE`,PESOF,geometry)
respuestas_encuesta_2025=respuestas_encuesta_2025 |> 
  dplyr::mutate(`¿Ha mejorado, se ha quedado igual o ha empeorado: Instalaciones de planteles educativos?`=ifelse(
    
    `¿Ha mejorado, se ha quedado igual o ha empeorado: Instalaciones de planteles educativos?`=="Ha mejorado/Se ha quedado igual de bien",
    "Sí",ifelse(
      `¿Ha mejorado, se ha quedado igual o ha empeorado: Instalaciones de planteles educativos?`=="Ha empeorado/Se ha quedado igual de mal",
      "No","No sé"
    )
    
  ),
  `¿Ha mejorado, se ha quedado igual o ha empeorado: Equipamiento en aulas?`=ifelse(
    `¿Ha mejorado, se ha quedado igual o ha empeorado: Equipamiento en aulas?`=="Ha mejorado/Se ha quedado igual de bien",
    "Sí",ifelse(
      `¿Ha mejorado, se ha quedado igual o ha empeorado: Equipamiento en aulas?`=="Ha empeorado/Se ha quedado igual de mal",
      "No","No sé"
    )
  )
    
  )
}



source("Códigos/raster_base.R")
library(spatstat)
bbox <- st_bbox(base)
window <- owin(xrange = c(bbox[1], bbox[3]), yrange = c(bbox[2], bbox[4]))

library(raster)
filtrar_dada_pregunta_y_boolean=function(num_col_pregunta,boolean=T){
  #num_col_pregunta=7
  filtro_respuesta='No'
  if(boolean){
    filtro_respuesta='Sí'
  }
  filtro=respuestas_encuesta_2025[(respuestas_encuesta_2025[,num_col_pregunta] |> st_drop_geometry())==filtro_respuesta &
                                    !is.na((respuestas_encuesta_2025[,num_col_pregunta] |> st_drop_geometry())),c(1,num_col_pregunta,(length(colnames(respuestas_encuesta_2025))-1) )]
  coords <- st_coordinates(filtro|>st_transform(crs(base)))    # Extraer las coordenadas de los puntos
  puntos_ppp <- ppp(x = coords[inside.owin(x = coords[, 1], y = coords[, 2],window), 1], y = coords[inside.owin(x = coords[, 1], y = coords[, 2],window), 2], window = window)
  # # Convertir los puntos a un objeto ppp
  print(colnames(filtro))
  pesos=filtro$PESOF[inside.owin(x = coords[, 1], y = coords[, 2],window)]
  dens=density(puntos_ppp,sigma=c(1000,1000),dimyx=c(998*3,978*3),weights =pesos)|> rast()

  dens2=dens
  terra::crs(dens2)=terra::crs(base)
  #extent(dens)=extent(base)
  dens=mask(dens2,municipios)
  #writeRaster(dens,paste0("Complejidad económica/raster Actividades DENUE/rasters/",grupo_I,".tiff"))
  return(dens)
}





lista_rasters_educacion=list()
lista_rasters_educacion[["mejor1"]]=filtrar_dada_pregunta_y_boolean(8,T)#Ha mejorado
lista_rasters_educacion[["mejor2"]]=filtrar_dada_pregunta_y_boolean(9,T)#Ha empeorado
lista_rasters_educacion[["peor1"]]=filtrar_dada_pregunta_y_boolean(8,F)
lista_rasters_educacion[["peor2"]]=filtrar_dada_pregunta_y_boolean(9,F)

lista_rasters_educacion[['diff_mejor_menos_peor']]=mean(lista_rasters_educacion[["mejor1"]],lista_rasters_educacion[["mejor2"]])-
mean(lista_rasters_educacion[["peor1"]],lista_rasters_educacion[["peor1"]])
lista_rasters_educacion[['diff_mejor_menos_peor']] |> plot()

lista_rasters_educacion[["mejor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/planteles_mejor.tif")
lista_rasters_educacion[["mejor2"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/equipamiento_aulas_mejor.tif")
lista_rasters_educacion[["peor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/planteles_peor.tif")
lista_rasters_educacion[["peor2"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/equipamiento_aulas_peor.tif")
lista_rasters_educacion[["diff_mejor_menos_peor"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Infraestructura educativa/diff_mejor_menos_peor.tif")

lista_rasters_salud=list()
lista_rasters_salud[["mejor1"]]=filtrar_dada_pregunta_y_boolean(7,F)#No ha sido afectado por la falta de infraestructura de salud
lista_rasters_salud[["peor1"]]=filtrar_dada_pregunta_y_boolean(7,T)


lista_rasters_salud[['diff_mejor_menos_peor']]=(lista_rasters_salud[["mejor1"]]-lista_rasters_salud[["peor1"]])
lista_rasters_salud[['diff_mejor_menos_peor']] |> plot()

lista_rasters_salud[["mejor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Salud/no_afectado_por_mal_equipamiento.tif")
lista_rasters_salud[["peor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Salud/afectado_por_mal_equipamiento.tif")
lista_rasters_salud[["diff_mejor_menos_peor"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Salud/diff_no_afectado_menos_afectado.tif")


lista_rasters_espacios_publicos=list()
lista_rasters_espacios_publicos[["mejor1"]]=filtrar_dada_pregunta_y_boolean(5,F)#No ha sido afectado por la falta de espacios
lista_rasters_espacios_publicos[["peor1"]]=filtrar_dada_pregunta_y_boolean(5,T)

lista_rasters_espacios_publicos[['diff_mejor_menos_peor']]=(lista_rasters_espacios_publicos[["mejor1"]]-lista_rasters_espacios_publicos[["peor1"]])
lista_rasters_espacios_publicos[['diff_mejor_menos_peor']] |> plot()


lista_rasters_espacios_publicos[["mejor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Espacios publicos/no_afectado_por_falta_de_parques.tif")
lista_rasters_espacios_publicos[["peor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Espacios publicos/afectado_por_falta_de_parques.tif")
lista_rasters_espacios_publicos[["diff_mejor_menos_peor"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Espacios publicos/diff_no_afectado_menos_afectado.tif")


lista_rasters_agua=list()
lista_rasters_agua[["peor1"]]=filtrar_dada_pregunta_y_boolean(2,T)#Ha sido afectado
lista_rasters_agua[["mejor1"]]=filtrar_dada_pregunta_y_boolean(2,F)#NO Ha sido afectado
#Principal problema colonia
lista_rasters_agua[["peor2"]]=filtrar_dada_pregunta_y_boolean(10,T)#Principal problema de la colonia
lista_rasters_agua[["peor3"]]=filtrar_dada_pregunta_y_boolean(19,T)#Principal problema de la colonia
lista_rasters_agua[["peor4"]]=filtrar_dada_pregunta_y_boolean(36,T)#Principal problema de la colonia
#Principal problema estado
lista_rasters_agua[["peor5"]]=filtrar_dada_pregunta_y_boolean(46,T)#Principal problema del estado
lista_rasters_agua[["peor6"]]=filtrar_dada_pregunta_y_boolean(49,T)#Principal problema del estado
lista_rasters_agua[["peor7"]]=filtrar_dada_pregunta_y_boolean(64,T)#Principal problema del estado

lista_rasters_agua |> names() |> lapply(\(x)plot(lista_rasters_agua[[x]],main=paste0(x)))
(mean(lista_rasters_agua[["mejor1"]],lista_rasters_agua[["mejor1prima"]])-mean(lista_rasters_agua[["peor1"]],lista_rasters_agua[["peor1prima"]],
     sum(lista_rasters_agua[["peor2"]],lista_rasters_agua[["peor3"]],lista_rasters_agua[["peor4"]]),
     sum(lista_rasters_agua[["peor5"]],lista_rasters_agua[["peor6"]],lista_rasters_agua[["peor7"]])
     )) |> plot()
(lista_rasters_agua[["mejor1"]]-mean(lista_rasters_agua[["peor1"]],
     sum(lista_rasters_agua[["peor2"]],lista_rasters_agua[["peor3"]],lista_rasters_agua[["peor4"]]),
     sum(lista_rasters_agua[["peor5"]],lista_rasters_agua[["peor6"]],lista_rasters_agua[["peor7"]])
     )) ->lista_rasters_agua[["diff_mejor_menos_peor"]] 


lista_rasters_agua[["mejor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/NO_afectado_suministro_agua.tif")
lista_rasters_agua[["peor1"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/afectado_suministro_agua.tif")
lista_rasters_agua[["peor2"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_colonia_agua_potable.tif")
lista_rasters_agua[["peor3"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_colonia_drenaje.tif")
lista_rasters_agua[["peor4"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_colonia_serv_publicos.tif")
lista_rasters_agua[["peor5"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_estado_drenaje.tif")
lista_rasters_agua[["peor6"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_estado_escasez_agua.tif")
lista_rasters_agua[["peor7"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/princ_prob_estado_servicios_publicos.tif")
lista_rasters_agua[["diff_mejor_menos_peor"]] |> writeRaster("Inputs/Rasters_Generados_en_R/Otros/exploracion_encuesta/Por temas/Agua y servicios publicos/diff_buena_percepcion_menos_mala_percepcion.tif")
