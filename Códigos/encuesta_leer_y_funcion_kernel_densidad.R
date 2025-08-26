##Datos de encuesta
library(sf)
st_read("Inputs/Rasters_Generados_en_R/Otros/EncuestaGober2025_filtracion.geojson")->
  respuestas_encuesta_2025
principal_problema_estado_drenaje=respuestas_encuesta_2025 |> dplyr::select(SbjNum,X.Cuál.considera.que.es.el.principal.problema.que.enfrenta.el.estado.de.Hidalgo.) |> 
  dplyr::filter(X.Cuál.considera.que.es.el.principal.problema.que.enfrenta.el.estado.de.Hidalgo.%in%c("DRENAJE"))

principal_problema_colonia_carreteras=respuestas_encuesta_2025 |> dplyr::select(Y...cuál.considera.que.es.el.principal.problema.en.su.colonia.) |> 
  dplyr::filter(Y...cuál.considera.que.es.el.principal.problema.en.su.colonia.=='INFRAESTRUCTURA VIAL / MANTENIMIENTO EN CALLES') 
                                                                                                                                                                                              
respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Instalaciones.de.planteles.educativos.[
  respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Instalaciones.de.planteles.educativos.=="Ha mejorado/Se ha quedado igual de bien"
]='Sí'
respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Instalaciones.de.planteles.educativos.[
  respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Instalaciones.de.planteles.educativos.=="Ha empeorado/Se ha quedado igual de mal"
]='No'

respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Equipamiento.en.aulas.[
  respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Equipamiento.en.aulas.=="Ha mejorado/Se ha quedado igual de bien"
]='Sí'
respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Equipamiento.en.aulas.[
  respuestas_encuesta_2025$X.Ha.mejorado..se.ha.quedado.igual.o.ha.empeorado..Equipamiento.en.aulas.=="Ha empeorado/Se ha quedado igual de mal"
]='No'


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
  filtro=respuestas_encuesta_2025[(respuestas_encuesta_2025[,num_col_pregunta] |> st_drop_geometry())==filtro_respuesta,]
  coords <- st_coordinates(filtro|>st_transform(crs(base)))    # Extraer las coordenadas de los puntos
  puntos_ppp <- ppp(x = coords[inside.owin(x = coords[, 1], y = coords[, 2],window), 1], y = coords[inside.owin(x = coords[, 1], y = coords[, 2],window), 2], window = window)
  # # Convertir los puntos a un objeto ppp
  pesos=filtro$PESOF[inside.owin(x = coords[, 1], y = coords[, 2],window)]
  dens=density(puntos_ppp,sigma=c(2000,2000),dimyx=c(998,978),weights =1e6*100*pesos/sum(pesos))|> rast()
  
  dens2=dens 
  terra::crs(dens2)=terra::crs(base)
  #extent(dens)=extent(base)
  dens=mask(dens2,municipios)
  #writeRaster(dens,paste0("Complejidad económica/raster Actividades DENUE/rasters/",grupo_I,".tiff"))
  return(dens)
}


