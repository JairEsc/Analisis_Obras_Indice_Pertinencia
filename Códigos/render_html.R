##Generar el html usando el entorno actual

rmarkdown::render(
   "Documentacion_Analisis_obras_indice_pertinencia.qmd",
)
library(leaflet) 
library(leaflegend) 
data(quakes) 
# Numeric Legend 
numPal <- colorNumeric('Spectral', 1:100) 
leaflet() %>% addTiles() %>% addLegendNumeric( pal = numPal, values = 1:100, position = 'bottomright', title = 'addLegendNumeric (Horizontal)', orientation = 'horizontal', shape = 'rect', decreasing = FALSE, height = 20, width = 100,labels = c('alta',"baja"),tickLength = 0) 
#%>% addLegendNumeric( pal = numPal, values = quakes$depth, position = 'topright', title = htmltools::tags$div('addLegendNumeric (Decreasing)', style = 'font-size: 24px; text-align: center; margin-bottom: 5px;'), orientation = 'vertical', shape = 'stadium', decreasing = TRUE, height = 100, width = 20 ) %>% addLegend(pal = numPal, values = quakes$depth, title = 'addLegend')
