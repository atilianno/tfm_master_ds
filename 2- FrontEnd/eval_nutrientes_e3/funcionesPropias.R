# Autor: Ricardo Rodriguez
# Fecha: 30/08/2023

# Changelog
# 01/09/2023 Felipe Sánchez Fabre (FASF) - Cambio del orden de los parámetros (macronutrientes y micronutriente) y mejoras en la lista a retornar.


normalize_column <- function(column, max_value, min_value) {
  return((column - min_value) / (max_value - min_value))
}

# Buscar cluster mas cercano a producto
encontrar_cercano_cluster <- function(TIPO_PRODUCTO, DT_CARBOHYDRATES_100G, DT_PROTEINS_100G, DT_FAT_100G, DT_ENERGY_100G) { #FASF cambio en el orden
  
  if (!(TIPO_PRODUCTO %in% c("soja", "tofu", "seitan"))) {
    stop("Tipo de producto no válido.")
  }
  
  # Leer las descripciones de los clústeres desde un archivo JSON
  descripciones_clusters <- fromJSON("descripciones_clusters.json")
  
  if(TIPO_PRODUCTO == "soja") {
    max_min <- fromJSON("max_min_soja.json")
    centroides <- fromJSON("centroides_soja.json")
  }
  if(TIPO_PRODUCTO == "tofu") {
    max_min <- fromJSON("max_min_tofu.json")
    centroides <- fromJSON("centroides_tofu.json")
  }
  if(TIPO_PRODUCTO == "seitan") {
    max_min <- fromJSON("max_min.json")
    centroides <- fromJSON("centroides_seitan.json")
  }
  
  nuevo_producto_normalizado <- data.frame(
    ENERGY_100G =  normalize_column(DT_ENERGY_100G, max_min$ENERGY_100G$MAX, max_min$ENERGY_100G$MIN),
    FAT_100G = normalize_column(DT_FAT_100G, max_min$FAT_100G$MAX, max_min$FAT_100G$MIN),
    CARBOHYDRATES_100G = normalize_column(DT_CARBOHYDRATES_100G, max_min$CARBOHYDRATES_100G$MAX, max_min$CARBOHYDRATES_100G$MIN),
    PROTEINS_100G = normalize_column(DT_PROTEINS_100G, max_min$PROTEINS_100G$MAX, max_min$PROTEINS_100G$MIN)
  )
  
  # Asegurarse de que las columnas del nuevo producto y los centroides coinciden
  if (!all(names(nuevo_producto_normalizado) %in% names(centroides))) {
    stop("Las columnas del nuevo producto y los centroides deben coincidir.")
  }
  
  # Calcula las distancias euclidianas entre el nuevo producto y cada centroide
  distancias <- apply(centroides, 1, function(centroide) {
    suma_cuadrados <- sum((nuevo_producto_normalizado - centroide)^2)
    sqrt(suma_cuadrados)
  })
  
  # Encuentra el índice del centroide más cercano
  indice_centroide_cercano <- which.min(distancias)
  
  # Identificar el color y la descripción del cluster
  color_descripcion <- descripciones_clusters[[TIPO_PRODUCTO]][indice_centroide_cercano]
  
  # Devolver el mensaje y las características del centroide
  # FASF cambio al conformar la lista que se retorna
  lista <- list(
    as.character(names(color_descripcion)),
    as.character(color_descripcion[as.character(names(color_descripcion))][1])
  )
  
  # FASF se agrega el retorno de la lista de respuesta
  return(lista)
  
}



