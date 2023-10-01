
# ###############################################################################
# FUNCIONES PROCESAR DATOS
# ###############################################################################


################################################################################
# Función para preparar y limpiar DataFrames
################################################################################
# Función para determinar si una columna tiene elementos no numéricos


# # Función para convertir una columna a numérica
# convert_to_numeric <- function(column) {
#   avg_value <- mean(as.numeric(column[!is.na(suppressWarnings(as.numeric(column)))]), na.rm = TRUE)
#   column <- map_dbl(column, ~ ifelse(is.na(suppressWarnings(as.numeric(.x))), avg_value, as.numeric(.x)))
#   return(column)
# }

dataframe_objetivo <- function(df, columnas) {
  # Seleccionar columnas de interés
  df <- df[,columnas]

  return(df)
}


# Función para convertir una columna a numérica
convert_to_numeric <- function(column) {
  # Calcula el promedio de los elementos numéricos en la columna
  avg_value <- mean(as.numeric(column[!is.na(suppressWarnings(as.numeric(column)))]), na.rm = TRUE)
  
  # Reemplaza los valores no numéricos y NA por el promedio calculado
  column <- map_dbl(column, ~ ifelse(is.na(suppressWarnings(as.numeric(.x))), avg_value, as.numeric(.x)))

  return(column)
}

limpiar_dataframe <- function(df, columns_to_convert) {

  # Eliminar registros duplicados
  df <- subset(df, !duplicated(df))
  
  # Convertir solo las columnas identificadas
  df_nuevo <- df %>%
    mutate(across(all_of(columns_to_convert), convert_to_numeric))

  df_nuevo <- df %>%
  mutate(across(all_of(columns_to_convert), convert_to_numeric))

  # Eliminar filas con suma de columnas numéricas igual a 0
  df_nuevo <- df_nuevo[apply(df_nuevo[, columns_to_convert], 1, sum) != 0, ]
  
  # Omitir NA
  df_nuevo <- na.omit(df_nuevo)
  
  # Elimina registros duplicados
  df_nuevo <- subset(df_nuevo, !duplicated(df_nuevo))

  return(df_nuevo)
}
################################################################################
# Local Outlier Factor (LOF)
################################################################################

eliminar_outliers_lof <- function(df, k, threshold) {
  
  lof_scores <- lof(df, k = k)
  df_lof <- cbind(df, lof_score = lof_scores)
  df_clean <- subset(df_lof, lof_score <= threshold)
  return(df_clean)
}

################################################################################
# Función para normalizar DataFrames
################################################################################

# Función para escalar los datos
escalar_dataframe <- function(df, columnas_numericas) {
  
  # Convertir PRODUCT_NAME a factor
  df <- df %>%
    mutate(PRODUCT_NAME = as.factor(PRODUCT_NAME))
  
  # Escalar solamente las columnas numéricas
  df_nuevo <- df %>%
    mutate(across(all_of(columnas_numericas), ~ (. - min(.)) / (max(.) - min(.))))
  
  return(df_nuevo)
}

escalar_dataframe_CUANTIL <- function(df, cuantil) {
  
  df_nuevo <- df %>% 
    mutate(outlier = ifelse(lof_score > cuantil, 1, 0))
  
  return(df_nuevo)
}

################################################################################
# Función para PCA
################################################################################

PCA_dataframe <- function(df) {
  
  df_nuevo <- PCA(df, scale.unit = F, ncp = 6, graph = F)
  
  return(df_nuevo)
}

################################################################################
# Función para LOF
################################################################################

LOF_dataframe <- function(df,puntos) {
  
  # Eliminar columnas no numéricas
  #df_nuevo <- df %>% select_if(is.numeric)

  # LOF
  df_nuevo <- lof(df, puntos)
  
  return(df_nuevo)
}

################################################################################
# Función para loft score
################################################################################

LOF_SCORE_dataframe <- function(df_pca, df_lof, df_clean) {
  
  df_nuevo_before <- data.frame(df_pca$ind$coord[,1:3])
  df_nuevo <- cbind(df_nuevo_before, lof_score = df_lof)
  #df_nuevo <- cbind(df_nuevo_before, fraud = df_clean$, lof_score = df_clean$lof)
  
  return(df_nuevo)
}

################################################################################
# Función para obtener cuantiles específicos
################################################################################

obtener_cuantiles <- function(data, porcentaje) {
  
  resultado_quantile <- quantile(data$lof_score, probs = c(0, porcentaje))
  porcentaje_str <- paste0(as.character(porcentaje*100),"%")
  
  # Extraer los cuantiles
  cuantil_0 <- resultado_quantile["0%"]
  cuantil_1 <- resultado_quantile[porcentaje_str]
  
  return(list(cuantil_0 = cuantil_0, cuantil_1 = cuantil_1))
}

################################################################################
# Función para remover outliers
################################################################################

remove_outliers <- function(data, column, sd_threshold = 2) {
  data[abs(scale(data[[column]])) < sd_threshold, ]
}

# Crea una función para eliminar valores atípicos por columna con el método del rango intercuartílico
eliminar_atipicos_por_columna <- function(data, threshold = 1.5) {
  data %>%
    mutate(across(.cols = is.numeric, 
                  .fns = ~ ifelse(between(., quantile(., 0.25) - threshold * IQR(.), quantile(., 0.75) + threshold * IQR(.)), ., NA)))
}
################################################################################
# Función para Obtener las columnas cuantitativas del dataframe
################################################################################

columnas_cuantitativas <- function(df) {
  
  columnas <- sapply(df, is.numeric)
  
  return(columnas)
}

################################################################################
# Función para Aplicar filtros negativos
################################################################################

# Crear una función que verifica si alguna de las palabras está presente en el texto
verificar_palabras <- function(texto, palabras) {
  sapply(palabras, function(palabra) grepl(palabra, texto))
}

filtro_negativo <- function(df, palabras_a_eliminar) {
  
  # ELiminar columnas que ENERGY_KCAL_100G = 0
  df <- df %>% filter(ENERGY_KCAL_100G > 0)

  # Identificar las filas que contienen alguna de las palabras a eliminar
  filas_a_eliminar <- apply(df, 1, function(row) any(verificar_palabras(row["PRODUCT_NAME"], palabras_a_eliminar)))
  
  # Eliminar las filas identificadas del dataframe original
  df_nuevo1 <- df[!filas_a_eliminar, ]
  
  #Elimino los faltantes
  df_nuevo1 <- df_nuevo1[complete.cases(df_nuevo1), ]
  
  return(df_nuevo1)
}




# ###############################################################################
# FUNCIONES GRÁFICOS
# ###############################################################################



################################################################################
# Funcion de gráfico "Distribución LOF Score"
################################################################################

graf_distribucion_LOF <- function(df, producto) {
  
  titulo <- paste0("Distribución LOF Score - ", producto)
  
  distribucion <- ggplot(df, aes(x=Dim.1 ,y=Dim.2)) + 
    geom_point(aes(size=lof_score)) +
    ggtitle(titulo) +
    theme_minimal()
  
  distribucion
}

################################################################################
# Funcion de gráfico "Distribución LOF Score"
################################################################################}

graf_distribucion_OUTLIERS_LOF <- function(df, producto) {
  
  titulo <- paste0("Distribución LOF Score - [OUTLIERS] ", producto)
  
  lof_visual_lof_score <- ggplot(df, aes(x=Dim.1 ,y=Dim.2, color=outlier)) + 
    geom_point() +
    ggtitle(titulo)+
    theme_minimal()
  
  lof_visual_lof_score
}

################################################################################
# Funcion de gráfico "Distribución LOF Score"
################################################################################

graf_distribucion_densidad_LOF <- function(df, producto) {
  
  titulo <- paste0("Distribución LOF Score [DENSIDAD] - ", producto)
  
  df %>%
    filter(lof_score <= 1.75) %>% 
    ggplot( aes(x=lof_score)) +
    geom_density( color="#e9ecef", fill = "#c90076", alpha=0.7) +
    scale_fill_manual(values="#8fce00") +
    xlab("LOF Score")+
    ggtitle(titulo)+
    theme_minimal() +
    labs(fill="")
}

################################################################################
# Funcion de gráfico "Varianza explicada por cada dimensión"
################################################################################

graf_var_explicada_pca <- function(df, producto) {
  
  titulo <- paste0("Varianza explicada por cada dimensión [PCA] - ", producto)
  
  fviz_eig(df, ncp = 6, addlabels = T, main = titulo)
}

################################################################################
# Función para gráficos "HISTOGRAMAS"
################################################################################

graf_histogramas <- function(df, producto) {
  
  # Obtener las columnas cuantitativas del dataframe
  columnas_cuantitativas <- columnas_cuantitativas(df)
  
  # Crear un histograma para cada columna cuantitativa
  for (columna in names(df[columnas_cuantitativas])) {
    plot <- df[, columna]
    
    p <- ggplot(data.frame(x = plot), aes(x)) +
      geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
      labs(title = paste("Histograma de", columna, producto),
           x = columna,
           y = "Frecuencia")
    
    print(p)
  }
}

################################################################################
# Función para gráficos "DOSPERSIÓN 3D"
################################################################################

dispersion_3d <- function(df1, df2, df3, column1, column2, column3, producto){
  
  scatterplot3d(df1, df2, df3,
                xlab = column1, ylab = column2, zlab = column3,
                main = producto)
}




#Clustergrama
source("clustergram.r")


# ###############################################################################
# FUNCIONES CLUSTER
# ###############################################################################



colores = c("#00AFBB", "#E7B800", "#FC4E07","#7A378B", "#Ac9E11","#7C7899")

################################################################################
# Funcion de gráfico "Elbow"
################################################################################

graf_cluster_optimos <- function(df) {
  
  elbow <- fviz_nbclust(x = df, FUNcluster = kmeans, method = "wss",
                        k.max = 15, diss = get_dist(df, method = "euclidean"), 
                        nstart = 25)
  print(elbow)
}

################################################################################
# Funcion de CLUSTERING "KMEANS"
################################################################################

clustering_kmeans <- function(df, n_clusters) {
  
  set.seed(123)
  km_clusters <- kmeans(x = df, centers = n_clusters, nstart = 50)
  
  return(km_clusters)
  
}

graf_clustering_kmeans <- function(df, km_clusters, producto) {
  
  titulo <- paste0("Resultados clustering K-means ", producto)
  
  fviz_cluster(object = km_clusters, data = df, show.clust.cent = TRUE,
               ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
               pointsize = 0.5, outlier.color = "darkred", geom = "point",
               palette = cluster_colors) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(titulo)
}

################################################################################
# Funcion de CLUSTERING "EUCLIDEAN"
################################################################################

clustering_euclidean <- function(df, n_clusters) {
  
  set.seed(101)
  
  hc_euclidea_av <- hclust(d = dist(x = df, method = "euclidean"),
                           method = "average")
  
  return(hc_euclidea_av)
}

graf_clustering_euclidean <- function(df, n_clusters, producto, hc_euclidea_av) {
  
  titulo <- paste0("clustering Jerárquico ", producto)
  subtitulo <- paste0("Distancia euclidea Promedio, k=", n_clusters)
  
  fviz_dend(x = hc_euclidea_av, k = n_clusters, cex = 0.5,
            k_colors = cluster_colors,color_labels_by_k = T,
            lwd = 0.2,type = "r",label_cols = rainbow(nrow(df)),
            rect_lty = "lightblue") +
    geom_hline(yintercept = 3.65, linetype = "dashed") +
    labs(title = titulo,
         subtitle = subtitulo)
}

################################################################################
# Funcion de CLUSTERING "KMEANS SUPERPUESTO"
################################################################################

clustering_kmeans_SUPERPUESTO <- function(df, n_clusters, producto) {
  
  titulo <- paste0("Resultados clustering K-means superpuestos - ", producto)
  
  pam.res <- pam(df, n_clusters)
  
  # Visualización
  fviz_cluster(pam.res, geom = "point", ellipse.type = "norm",
               show.clust.cent = TRUE,star.plot = TRUE)+
    labs(title = titulo)+ theme_bw()
}

################################################################################
# Funcion de GRÁFICO "KMEANS Y PCA"
################################################################################

PCA_CLUSTER_KMEANS <- function(df, n_clusters) {
  
  # PCA
  pca<- prcomp(df, scale=FALSE)
  df.pca <- pca$x
  
  # Cluster over the three first PCA dimensions
  kc <- kmeans(df.pca, n_clusters)
  
  fviz_pca_biplot(pca, label="var", habillage=as.factor(kc$cluster),palette = cluster_colors) +
    labs(color=NULL) + ggtitle("") +
    theme(text = element_text(size = 12),
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"))
}

################################################################################
# Funcion para OBTENER CENTROIDES
################################################################################

centroides_clustering <- function(clusters) {
  
  # Obtener los centroides de cada clúster
  centroids <- as.data.frame(clusters$centers)
  
  # Obtener el valor mínimo y máximo en el dataframe
  min_value <- min(centroids, na.rm = TRUE)
  max_value <- max(centroids, na.rm = TRUE)
  
  # Escalar los datos entre 0 y 1
  scaled_df <- (centroids - min_value) / (max_value - min_value)
  
  # Define the variable ranges: maximum and minimum
  max_min <- data.frame(
    ENERGY_KCAL_100G = c(1, 0), FAT_100G = c(1, 0), CARBOHYDRATES_100G = c(1, 0),PROTEINS_100G = c(1, 0)
  )
  rownames(max_min) <- c("Max", "Min")
  
  # Bind the variable ranges to the data
  df_min_max <- rbind(max_min, scaled_df)
  
  return(df_min_max)
}

################################################################################
# Función para gráficos "radar chart"
################################################################################

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.3), plwd = 2, plty = 1,
    
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    
    # Customize the axis
    axislabcol = "grey", 
    
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
    
  )
}

graf_radarchart <- function(df){
  # Reduce plot margin using par()
  op_full <- par(mar = c(1, 2, 2, 2))
  
  # Create the radar charts
  create_beautiful_radarchart(
    data = df, caxislabels = c(0, .25, .5,.75, 1),
    color = colores
  )
  
  # Add an horizontal legend
  legend(
    x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
    bty = "n", pch = 20 , col = colores,
    text.col = "black", cex = 1, pt.cex = 1.5
  )
  par(op_full)
}

graf_radarchart_clusters <- function(df, titles, n_clusters){
  
  # Create the radar chart
  for(i in 1:n_clusters){
    create_beautiful_radarchart(
      data = df[c(1, 2, i+2), ], caxislabels = c(0, .25, .5, .75, 1),
      color = colores[i], title = titles[i],
      width = 5, height = 5, mar = c(0, 0, 0, 0)
    )
  }
}

################################################################################
# Función para 
# "top10 productos más cercanos al centroide que representan cada cluster"
################################################################################

top10_clusters <- function(df, n_clusters){
  
  # Realizar clustering en el dataframe
  set.seed(123)
  df_top <- df
  km_clusters <- kmeans(x = df_top[, c("ENERGY_KCAL_100G", "FAT_100G", "CARBOHYDRATES_100G", "PROTEINS_100G")], centers = n_clusters, nstart = 50)
  # Obtener las asignaciones de clúster
  cluster_assignments <- km_clusters$cluster
  
  # Agregar las asignaciones de clúster al dataframe
  df_top$cluster <- cluster_assignments
  
  # Inicializar una lista para almacenar los productos más cercanos a cada centroide
  closest_products <- vector("list", max(cluster_assignments))
  
  # Encontrar los productos más cercanos a cada centroide
  for (cluster in 1:max(cluster_assignments)) {
    cluster_center <- km_clusters$centers[cluster, ]
    distancedf_normalizado <- apply(df_top[, c("ENERGY_KCAL_100G", "FAT_100G", "CARBOHYDRATES_100G", "PROTEINS_100G")], 1, function(row) {
      sum((row - cluster_center)^2)
    })
    closest_products[[cluster]] <- head(order(distancedf_normalizado), 3)
  }
  
  # Imprimir los productos más cercanos a cada centroide
  for (cluster in 1:max(cluster_assignments)) {
    cat("Cluster", cluster, ":\n")
    print(df_top[closest_products[[cluster]], c("PRODUCT_NAME", "ENERGY_KCAL_100G", "FAT_100G", "CARBOHYDRATES_100G", "PROTEINS_100G")])
    cat("\n")
  }
}

