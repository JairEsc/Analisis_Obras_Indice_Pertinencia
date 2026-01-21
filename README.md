En Documentación...html se puede consultar las cualidades no-técnicas del análisis. Aquí incluiré cualidades estríctamente técnicas. 

Requisitos: 
    - Conjunto de rasters
    - Conjunto de títulos
    - Conjunto de descripciones
    - Lista de pesos 
    - Intervalos de pesos
    - Conjuntos de obras
  
  source("codigos/leer_geojsons_c_extract.R"): 
    - lee los geojsons de obras de línea y punto y define una función para construir sus labels
  
   source("codigos/leer_rasters_generados_en_r.R"):
   - Lee indistinguidamente los rasters de Inputs/Rasters_Generados_en_R. Si quisiéramos eliminar uno, lo movemos a otra carpeta
   - Define los títulos y las descripciones mínimas del caso base. 

weights, limite_inferior, limite_superior fueron pesos default e intervalos de elegibilidad para los pesos de las elecciones. Se eligieron a ojo.

Se definen rasters, rasters_agua, _drenaje, _Espacios_publicos, _salud, _educacion. Listas de rasters a utilizar. rasters es el caso base. 


Se define la UI que ya está alimentada por el server. #1 Agregar botón para descargar el .tif

Algo raro es que las descripciones de educacion, salud, espacios. Están definidas pero no de agua y drenaje. 

Funciones para generar listas de títulos, descripciones y rasters. 

generar_lista_titulos():
    -Depende del tipo de obra seleccionada
    -Depende del tipo de vialidad (construcción o mejoramiento)
  Si es infraestructura vial de construcción: caso base
  Si es infraestructura vial de mejoramiento: Accesibilidad + nivel de uso
  
  Si es Agua, drenaje, espacios, educacion o salud: primeros del caso base + percepcion_especifica


generar_lista_descripciones():
    -Depende del tipo de obra seleccionada
    -Depende del tipo de vialidad (construcción o mejoramiento)
  Utiliza descripciones mínimas pero es la misma idea.

generar_lista_rasters():
    -Depende del tipo de obra seleccionada
    -Depende del tipo de vialidad (construcción o mejoramiento)
  Si es infraestructura vial de construcción: caso base
  Si es infraestructura vial de mejoramiento: Accesibilidad + nivel de uso (en negativo)
  