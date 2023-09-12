
# ###############################################################################
# ###############################################################################
# TOFU
# ###############################################################################
# ###############################################################################


```{r, echo = FALSE, warning = FALSE}

# LOF
tofu_lof <- LOF_dataframe(tofu_clean)

# PCA
tofu_pca <- PCA_dataframe(tofu_clean)

# LOFT SCORE
tofu_lof_score <- LOF_SCORE_dataframe(tofu_pca, tofu_lof, tofu_clean)

# corte quantil
tofu_probabilidad <- 0.9

# quantiles
tofu_quantiles_vars <- obtener_cuantiles(tofu_lof_score, tofu_probabilidad)

# Ahora 'quantiles_vars' es una lista con los cuantiles
tofu_cuantil_1 <- tofu_quantiles_vars$cuantil_0
tofu_cuantil_2 <- tofu_quantiles_vars$cuantil_1

# DATAFRAMES outliers

df_tofu <- df_tofu[tofu_lof < tofu_cuantil_2,]
df_tofu_outliers <- df_tofu[tofu_lof >= tofu_cuantil_2,]

# Escalar DATAFRAME - Cuantil
tofu_lof_score <- escalar_dataframe_CUANTIL(tofu_lof_score, tofu_cuantil_2)

# Eliminar valores atipicos univariantes

columns_to_check_tofu <- 3:ncol(df_tofu)

for (column_tofu in columns_to_check_tofu) {
  df_tofu <- remove_outliers(df_tofu, column_tofu)
}

# Omitir NA
df_tofu <- na.omit(df_tofu)
```

# ###############################################################################
# ANÁLISIS DATOS
# ###############################################################################

```{r, echo = FALSE, warning = FALSE}

graf_var_explicada_pca(tofu_pca, "tofu")

```
```{r}
par(cex.lab = 1.5, cex.main = 1.2)
Data <- scale(df_tofu[,3:6]) # notice I am scaling the vectors)
clustergram(Data, k.range = 2:8, line.width = 0.001) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.
```

```{r, echo = FALSE, warning = FALSE}

graf_distribucion_LOF(tofu_lof_score, "tofu")
```


```{r, echo = FALSE, warning = FALSE}

graf_distribucion_densidad_LOF(tofu_lof_score, "tofu")
```


```{r, echo = FALSE, warning = FALSE}

graf_distribucion_OUTLIERS_LOF(tofu_lof_score, "tofu")
```


```{r, echo = FALSE, warning = FALSE}
graf_histogramas(df_tofu, "- tofu")
```

```{r, echo = FALSE, warning = FALSE}
#Datos normalizados para el tofu
df_tofu_normalizado <- escalar_dataframe(df_tofu)

graf_histogramas(df_tofu_normalizado, "- tofu [NORMALIZADO]")
```

```{r, echo = FALSE, warning = FALSE}

# Crear el gráfico de dispersión en 3D

dispersion_3d(df_tofu_normalizado$PROTEINS_100G, df_tofu_normalizado$FAT_100G, df_tofu_normalizado$CARBOHYDRATES_100G, "PROTEINS_100G", "FAT_100G", "CARBOHYDRATES_100G", "tofu")

dispersion_3d(df_tofu_normalizado$SALT_100G, df_tofu_normalizado$ENERGY_KCAL_100G, df_tofu_normalizado$SUGARS_100G, "SALT_100G", "ENERGY_KCAL_100G", "SUGARS_100G", "tofu")
```

# ###############################################################################
# CLUSTERS
# ###############################################################################

```{r, echo = FALSE, warning = FALSE}
# total de cluster óptimos
graf_cluster_optimos(df_tofu_normalizado)
```


```{r, echo = FALSE, warning = FALSE}
# Número de clusters
n_clusters_tofu <- 3

tofu_kms_clusters <- clustering_kmeans(df_tofu_normalizado, n_clusters_tofu)
graf_clustering_kmeans(df_tofu_normalizado, tofu_kms_clusters, "tofu")
```

```{r, echo = FALSE, warning = FALSE}
tofu_euclidean_clusters <- clustering_euclidean(df_tofu_normalizado, n_clusters_tofu)
graf_clustering_euclidean(df_tofu_normalizado, n_clusters_tofu, "tofu", tofu_euclidean_clusters)
```

```{r, echo = FALSE, warning = FALSE}
clustering_kmeans_SUPERPUESTO(df_tofu_normalizado, n_clusters_tofu, "tofu")
```

```{r, echo = FALSE, warning = FALSE}
# Biplot PCA y K-Means para medir representatividad
PCA_CLUSTER_KMEANS(df_tofu_normalizado, n_clusters_tofu)
```

```{r, echo = FALSE, warning = FALSE}
df_tofu_min_max <- centroides_clustering(tofu_kms_clusters)
df_tofu_min_max
```

```{r, echo = FALSE, warning = FALSE}
graf_radarchart(df_tofu_min_max)
```

```{r, echo = FALSE, warning = FALSE}
graf_radarchart_clusters(df_tofu_min_max, c("1", "2", "3"), n_clusters_tofu)
```

```{r, echo = FALSE, warning = FALSE}
# Visualizar los productos más cercanos al centroide que representan cada cluster top10
top10_clusters(df_tofu, n_clusters_tofu)
```

# ###############################################################################
# ###############################################################################
# SOJA
# ###############################################################################
# ###############################################################################


```{r, echo = FALSE, warning = FALSE}

# LOF
soja_lof <- LOF_dataframe(soja_clean)

# PCA
soja_pca <- PCA_dataframe(soja_clean)

# LOFT SCORE
soja_lof_score <- LOF_SCORE_dataframe(soja_pca, soja_lof, soja_clean)

# corte quantil
soja_probabilidad <- 0.9

# quantiles
soja_quantiles_vars <- obtener_cuantiles(soja_lof_score, soja_probabilidad)

# Ahora 'quantiles_vars' es una lista con los cuantiles
soja_cuantil_1 <- soja_quantiles_vars$cuantil_0
soja_cuantil_2 <- soja_quantiles_vars$cuantil_1

# DATAFRAMES outliers

df_soja <- df_soja[soja_lof < soja_cuantil_2,]
df_soja_outliers <- df_soja[soja_lof >= soja_cuantil_2,]

# Escalar DATAFRAME - Cuantil
soja_lof_score <- escalar_dataframe_CUANTIL(soja_lof_score, soja_cuantil_2)

# Eliminar valores atipicos univariantes

columns_to_check_soja <- 3:ncol(df_soja)

for (column_soja in columns_to_check_soja) {
  df_soja <- remove_outliers(df_soja, column_soja)
}

# Omitir NA
df_soja <- na.omit(df_soja)
```

# ###############################################################################
# ANÁLISIS DATOS
# ###############################################################################

```{r, echo = FALSE, warning = FALSE}

graf_var_explicada_pca(soja_pca, "soja")

```

```{r}
par(cex.lab = 1.5, cex.main = 1.2)
Data <- scale(df_soja[,3:6]) # notice I am scaling the vectors)
clustergram(Data, k.range = 2:8, line.width = 0.001) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.
```

```{r, echo = FALSE, warning = FALSE}

graf_distribucion_LOF(soja_lof_score, "soja")
```


```{r, echo = FALSE, warning = FALSE}

graf_distribucion_densidad_LOF(soja_lof_score, "soja")
```


```{r, echo = FALSE, warning = FALSE}

graf_distribucion_OUTLIERS_LOF(soja_lof_score, "soja")
```


```{r, echo = FALSE, warning = FALSE}
graf_histogramas(df_soja, "- soja")
```

```{r, echo = FALSE, warning = FALSE}
#Datos normalizados para el soja
df_soja_normalizado <- escalar_dataframe(df_soja)

graf_histogramas(df_soja_normalizado, "- soja [NORMALIZADO]")
```

```{r, echo = FALSE, warning = FALSE}

# Crear el gráfico de dispersión en 3D

dispersion_3d(df_soja_normalizado$PROTEINS_100G, df_soja_normalizado$FAT_100G, df_soja_normalizado$CARBOHYDRATES_100G, "PROTEINS_100G", "FAT_100G", "CARBOHYDRATES_100G", "soja")

dispersion_3d(df_soja_normalizado$SALT_100G, df_soja_normalizado$ENERGY_KCAL_100G, df_soja_normalizado$SUGARS_100G, "SALT_100G", "ENERGY_KCAL_100G", "SUGARS_100G", "soja")
```

# ###############################################################################
# CLUSTERS
# ###############################################################################

```{r, echo = FALSE, warning = FALSE}
# total de cluster óptimos
graf_cluster_optimos(df_soja_normalizado)
```


```{r, echo = FALSE, warning = FALSE}
# Número de clusters
n_clusters_soja <- 3

soja_kms_clusters <- clustering_kmeans(df_soja_normalizado, n_clusters_soja)
graf_clustering_kmeans(df_soja_normalizado, soja_kms_clusters, "soja")
```

```{r, echo = FALSE, warning = FALSE}
soja_euclidean_clusters <- clustering_euclidean(df_soja_normalizado, n_clusters_soja)
graf_clustering_euclidean(df_soja_normalizado, n_clusters_soja, "soja", soja_euclidean_clusters)
```

```{r, echo = FALSE, warning = FALSE}
clustering_kmeans_SUPERPUESTO(df_soja_normalizado, n_clusters_soja, "soja")
```

```{r, echo = FALSE, warning = FALSE}
# Biplot PCA y K-Means para medir representatividad
PCA_CLUSTER_KMEANS(df_soja_normalizado, n_clusters_soja)
```

```{r, echo = FALSE, warning = FALSE}
df_soja_min_max <- centroides_clustering(soja_kms_clusters)
df_soja_min_max
```

```{r, echo = FALSE, warning = FALSE}
graf_radarchart(df_soja_min_max)
```

```{r, echo = FALSE, warning = FALSE}
graf_radarchart_clusters(df_soja_min_max, c("1", "2", "3"), n_clusters_soja)
```

```{r, echo = FALSE, warning = FALSE}
# Visualizar los productos más cercanos al centroide que representan cada cluster top10
top10_clusters(df_soja, n_clusters_soja)
```

