###raster base

library(terra)
library(sf)
read_sf("Inputs/LIM_MUNICIPALES.shp") |> st_transform(st_crs("EPSG:32614"))->municipios

extent_hidalgo<- st_bbox(municipios)

base <- terra::rast(
  xmin = extent_hidalgo['xmin'],
  xmax = extent_hidalgo['xmax'],
  ymin = extent_hidalgo['ymin'],
  ymax = extent_hidalgo['ymax'],
  ncols = 2934,
  nrows = 2994,
  crs = st_crs(municipios)
)

terra::crs(base)=st_crs(municipios)$wkt
values(base)=1
base=terra::mask(base,municipios)
plot(base)
