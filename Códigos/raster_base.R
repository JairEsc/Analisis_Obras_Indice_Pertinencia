###raster base

library(terra)
library(sf)
read_sf("Inputs/LIM_MUNICIPALES.shp") |> st_transform(st_crs("EPSG:32614"))->municipios


base <- terra::rast(
  xmin = st_bbox(municipios)['xmin'],
  xmax = st_bbox(municipios)['xmax'],
  ymin = st_bbox(municipios)['ymin'],
  ymax = st_bbox(municipios)['ymax'],
  ncols = 2934,
  nrows = 2994,
  crs = st_crs(municipios)
)

terra::crs(base)=st_crs(municipios)$wkt
values(base)=1
base=terra::mask(base,municipios)
#plot(base)
