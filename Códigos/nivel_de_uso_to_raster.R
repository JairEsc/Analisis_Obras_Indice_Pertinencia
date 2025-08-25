##Nivel de uso. 

##La meta es generar un raster que refleje el nivel de uso de obras carreteras. 

nivel_de_uso=st_read("Inputs/Nivel de uso/rutas_con_numero_de_usos_citydata.geojson") |> 
  st_transform(st_crs(municipios))
library(leaflet)
#leaflet() |> addTiles() |> addPolylines(data=nivel_de_uso |> st_transform(st_crs("EPSG:4326")))
source("Códigos/raster_base.R")
nivel_de_uso =terra::rasterize(terra::vect(nivel_de_uso), base, field = "num_registros", fun = "sum")
nivel_de_uso=mask(nivel_de_uso,municipios)
# plot(nivel_de_uso$num_registros)
# plot(st_geometry(municipios), add = TRUE, border = "red", lwd = 2)
# plot(nivel_de_uso)
leaflet() |> addTiles() |> addRasterImage(nivel_de_uso)

nivel_de_uso|> writeRaster("Inputs/Rasters_Generados_en_R/Otros/nivel_de_uso_proxy_de_numero_de_viajes.tif")

