#Leer inputs
source("Códigos/raster_base.R")


####Para definir las secciones a incentivar
read_sf("Inputs/Secciones/Originales/pp_ayuntamiento_seccion.shp")->secciones
secciones=secciones |> 
  dplyr::filter(!is.na(NOM_MUN)) |> 
  dplyr::mutate(GANADOR=ifelse(GANADOR=='MORENA_PNAH',
                               "Morena",##El ganador es morena (premio)
                               "Otro"))#O el ganador es otro (incentivo)
  

secciones=secciones |> 
  #dplyr::group_by(NOM_MUN) |> 
  #dplyr::summarise(pob_mun=sum(TOTAL,na.rm=T)) |> st_drop_geometry() |> merge(secciones,by='NOM_MUN') |> 
  dplyr::mutate(dens_votos_morena=100*(MORENA_PNA/TOTAL))##Calculamos el porcentaje de los votos de esa seccion con respecto a la poblacion municipal
  #Se interpreta como ##Esta sección acumula mucho voto a morena

##El filtro va a ser el top necesario para ganar el +30% de la población votante por municipio.

secciones_top_morena=secciones |> 
  dplyr::group_by(NOM_MUN,GANADOR) |> ##Agrupamos por GANADOR para incentivar a una seccion donde ganó(muchos votos) y una donde perdió por poco (porque aún tuvo muchos votos)
  dplyr::arrange(dplyr::desc(porc_morena_d_mun)) |> 
  dplyr::slice_head(n=1)##Tomamos una de cada una 

secciones_top_morena$geometry |> plot()#Vemos la cobertura estatal


secciones_top_morena_vect=terra::vect(secciones |> st_as_sf() |> st_transform(st_crs(municipios)))
######
#Le damos prioridad a las secciones por población_habitante_en_sección

secciones_top_morena_vect <- terra::rasterize(secciones_top_morena_vect, base, field = "dens_votos_morena", fun = "mean")

#values(base) <- sample(1:ncell(base),size = ncell(base),replace = T)
plot(secciones_top_morena_vect)
plot(st_geometry(municipios), add = TRUE, border = "red", lwd = 2)
#####################

secciones_top_morena_vect |> writeRaster("Inputs/Rasters_Generados_en_R/Secciones_electorales_prioritarias_slice_top_p_mun.tif",overwrite=T)
