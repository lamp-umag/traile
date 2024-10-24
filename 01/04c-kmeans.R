
#kmeans cluster analysis

# 1. First standardize variables since they might be on different scales
tdf_scaled <- scale(tdf)

# 2. Determine optimal number of clusters using various methods
library(factoextra)

# Elbow method
fviz_nbclust(tdf_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method")

# Silhouette method
fviz_nbclust(tdf_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method")

# 3. Run K-means with optimal k (let's try k=2 and k=3)
set.seed(1234) # for reproducibility
km2 <- kmeans(tdf_scaled, centers = 2, nstart = 25)
km3 <- kmeans(tdf_scaled, centers = 3, nstart = 25)
km4 <- kmeans(tdf_scaled, centers = 4, nstart = 25)
km5 <- kmeans(tdf_scaled, centers = 5, nstart = 25)
km6 <- kmeans(tdf_scaled, centers = 6, nstart = 25)

# 4. Visualize clusters
# Using first two principal components for visualization
fviz_cluster(km2, data = tdf_scaled,
             main = "K-means Clustering (k=2)")

fviz_cluster(km3, data = tdf_scaled,
             main = "K-means Clustering (k=3)")

fviz_cluster(km4, data = tdf_scaled,
             main = "K-means Clustering (k=4)")

fviz_cluster(km5, data = tdf_scaled,
             main = "K-means Clustering (k=5)")

fviz_cluster(km6, data = tdf_scaled,
             main = "K-means Clustering (k=6)")


# 5. Look at cluster characteristics
# Function to get cluster means
cluster_profile <- function(km) {
  data.frame(Cluster = factor(km$cluster)) %>%
    bind_cols(tdf) %>%
    group_by(Cluster) %>%
    summarise(across(everything(), mean))
}

# Get profiles for both solutions
cluster_profile(km2)
cluster_profile(km3)
cluster_profile(km4)
cluster_profile(km5)
cluster_profile(km6)


# 1. First get PCA
pca_result <- prcomp(tdf_scaled, scale = TRUE)

# 2. Create enhanced biplot with ggplot2
library(ggplot2)

# Get data for plotting
pca_data <- data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2],
  Cluster = factor(km2$cluster)  # using k=2 solution, can change to km3 if preferred
)

# Get variable loadings for arrows
loadings <- data.frame(
  Variable = rownames(pca_result$rotation),
  PC1 = pca_result$rotation[,1],
  PC2 = pca_result$rotation[,2]
)

# Create biplot
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "darkred") +
  geom_text(data = loadings,
            aes(x = PC1*5.2, y = PC2*5.2, label = Variable),
            color = "darkred",
            size = 3) +
  theme_minimal() +
  labs(title = "Biplot of AI Perception Clusters",
       subtitle = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% variance), ",
                         "PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% variance)"))

# Print variable contributions to first two PCs
print(summary(pca_result)$importance[,1:2])










# Create boxplot data
plot_data <- data.frame(tdf_scaled) %>%
  mutate(Cluster = factor(km3$cluster)) %>%
  pivot_longer(-Cluster, 
               names_to = "Variable", 
               values_to = "Value")

# Create boxplot
ggplot(plot_data, aes(x = Variable, y = Value, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cluster Profiles",
       subtitle = "Standardized scores by cluster",
       y = "Standardized Value",
       x = "Scale") +
  scale_fill_brewer(palette = "Set2")

# For a clearer view, could also do faceted version:
ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~Variable) +
  theme_minimal() +
  labs(title = "Cluster Profiles",
       subtitle = "Standardized scores by cluster",
       y = "Standardized Value") +
  scale_fill_brewer(palette = "Set2")









# Add cluster membership to original cases, but only for those in the cluster analysis
cluster_demos <- adf %>%
  select(prol, pdep, psex, page) %>%
  slice(which(!is.na(rowSums(select(adf, starts_with("c")))))) %>%  # Keep only cases used in clustering
  mutate(cluster = factor(km3$cluster))
table(factor(km3$cluster))

adf$

# Function for quick chi-square test and table
quick_chisq <- function(var) {
  tab <- table(cluster_demos[[var]], cluster_demos$cluster)
  test <- chisq.test(tab)
  list(
    table = tab,
    chi_square = test$statistic,
    p_value = test$p.value,
    cramer_v = sqrt(test$statistic/(sum(tab) * (min(dim(tab))-1)))
  )
}

# Run tests for each demographic variable
demo_vars <- c("prol", "pdep", "psex", "page")
lapply(demo_vars, function(x) {
  cat("\nAnalysis for:", x, "\n")
  result <- quick_chisq(x)
  print(result$table)
  cat("Chi-square =", round(result$chi_square, 2), 
      ", p =", round(result$p_value, 3),
      ", Cramer's V =", round(result$cramer_v, 3), "\n")
})
# Create Spanish labels for demographic variables
demo_labs <- list(
  prol = c("Estudiantes", "Académicos"),
  pdep = c("Pedagogía", "Otras carreras"),
  psex = c("Hombres", "Mujeres"),
  page = c("< 28 años", "≥ 28 años")
)

# Create Spanish labels for clusters
cluster_labs <- c("Escépticos\ndesconectados", 
                  "Entusiastas\ncomprometidos", 
                  "Temerosos\nconflictuados")

# Refined color palette:
# Cool grey for skeptics, vibrant teal for enthusiasts, deep coral for anxious
cluster_cols <- c("#5f9bff", "#00ba38", "#f8766d")

# Create plots
lapply(demo_vars, function(x) {
  ggplot(cluster_demos, aes(y = factor(!!sym(x), 
                                       labels = demo_labs[[x]]), 
                            fill = cluster)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = cluster_cols,
                      labels = cluster_labs) +
    labs(title = paste("Distribución de clusters por", x),
         x = "Proporción", 
         y = "",
         fill = "Perfiles") +
    theme_minimal() +
    scale_x_continuous(labels = scales::percent) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 11),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
})
