SE <- read.csv("1-dataset-MASTER.csv")

SE_for_kmeans <- SE[,3:13]
rownames(SE_for_kmeans) <- SE[,2]
colnames(SE_for_kmeans) <- col_names
write.csv(SE_for_kmeans, "1-dataset-MASTER-for-kmeans.csv")

# Plot histograms --------------
png(filename="GDC_z-variable-hist.png", width=620, height=876)
par(mfrow = c(4, 3), mar=c(4,3,2,3))
for (i in 1:ncol(SE_for_kmeans)){
  hist(SE_for_kmeans[,i], main="", xlab="", ylim=c(0, 24))
  title(main=paste("(",letters[i],") ", colnames(SE_for_kmeans)[i], sep=""))
  lines(density(SE_for_kmeans[,i], adjust=2), col="blue", lwd=2) 
}
dev.off()

rownames(sg_means) <- "National"
SE_for_kmeans <- rbind(SE_for_kmeans, sg_means)

library(effectsize)
SE_for_kmeans_z <- setNames(data.frame(matrix(ncol = 11, nrow = 29)), col_names)
for (i in 1:ncol(SE_for_kmeans)){
  SE_for_kmeans_z[,i] <- normalize(SE_for_kmeans[,i])
}

sg_means_z <- SE_for_kmeans_z[29,]
SE_for_kmeans_z <- SE_for_kmeans_z[-29,]

# Determine k -----------
# Check total within-cluster sum of squares 
png(filename="GDC_z_sum-of-squares.png", height=300, width=600)
par(mfrow=c(1, 2))
within_groups <- NULL
for (i in 1:15) within_groups[i] <- 
  kmeans(SE_for_kmeans_z,centers = i,iter.max = 1000)$tot.withinss

plot(1:15, within_groups, type = "b", pch = 19, xlab = "Number of Clusters",
     ylab="", main = "(a) Total within-cluster 
sum of squares", cex.main=1.3)

# Check total between-cluster sum of squares 
between_groups <- NULL
for (i in 1:15) between_groups[i] <- 
  kmeans(SE_for_kmeans_z,centers = i,iter.max = 1000)$betweenss

plot(1:15, between_groups, type = "b", pch = 19, xlab = "Number of Clusters",
     ylab="", main = "(b) Between-cluster 
sum of squares", cex.main=1.3)

dev.off()

set.seed(2)
rownames(SE_for_kmeans_z) <- SE$PA
k2 <- kmeans(SE_for_kmeans_z, 5, nstart=10)

# Cluster membership ----------
clusters <- as.data.frame(as.matrix(k2$cluster))
clusters$PA <- rownames(clusters)
rownames(clusters) <- NULL
colnames(clusters) <- c("cluster", "PA")
SE_for_kmeans_z$PA <- SE$PA
SE_for_kmeans_z <- merge(SE_for_kmeans_z, clusters, by.y="PA",
                       by.x="PA")

# Clusters by PC -----------
centers <- as.data.frame(k2$centers)
library(factoextra) 
rownames(SE_for_kmeans_z) <- SE_for_kmeans_z$PA
cluster_plot <- fviz_cluster(k2, data = SE_for_kmeans_z[,3:12]) + ggtitle(label="")
ggsave("GDC_z_clusterplot.png", height=5, width=7)

pca_fit <- prcomp(SE_for_kmeans_z[,3:12],scale.=TRUE)
summary(pca_fit)
write.csv(as.data.frame(t(round(pca_fit$rotation[,1:2], 3))), "GDC_z_PCs.csv")

# Create radar plots -----------
library(plotrix)
library(fmsb)
to_bind <- rbind(rep(1,11), rep(0,11), sg_means_z)

for (i in 1:nrow(centers)) {
  png(filename=paste("GDC_z_Cluster", i, ".png"), width=700, height=500)
  par(mar=c(2.1, 0.5, 0.5, 2.1))
  plot_this <- rbind(to_bind, centers[i,], use.names = F)
  
  radarchart(plot_this, 
             axistype=0,
             pcol=c(NA, "#d7504d", NA),
             pfcol=c(rgb(0.2,0.2,0.2,0.4), NA, NA),
             plty = 1,
             plwd = 3,
             vlcex = 1.3,
             axislabcol="grey")
  dev.off()
}

write.table(t(round(centers,3)), "GDC_z_centers.doc", sep=",")
write.csv(t(round(centers,3)), "GDC_z_centers.csv")

# GDC map for Singapore -----------
sg <- readOGR("./Datasets/sg-by-planning-area-2014", "MP14_PLNG_AREA_WEB_PL_fix")
SE_for_kmeans_z$PA <- toupper(row.names(SE_for_kmeans_z))
sg_clusters_z <- merge(sg, SE_for_kmeans_z, by.x="PLN_AREA_N", by.y="PA")

# remove needless PAs
remove <- !(sg_clusters_z$PLN_AREA_N %in% PA)
sg_clusters_z$PLN_AREA_N[remove]<- ""

gdc_labels <- c("Working Parents", "City Elites", "Comfortable Retirees", "Constrained Elderly", "Diverse Typicals")
prettiest_colors_2 <- c("#F7A8B2", "#FEF8DD", "#D1EED3", "#cce6ff", "#D8C3E0")
sg_gdc_z <- tm_shape(sg_clusters_z) + 
  tm_fill(col="cluster", palette=prettiest_colors_2, title="Cluster", colorNA="Gray95", showNA = F,
          labels = gdc_labels) + 
            tm_text("PLN_AREA_N", col="grey40", size = 1/2) +
  tm_borders(col="Gray80", alpha=0.9, lwd=0.5) +
  tm_layout(frame=T, 
            main.title="Geodemographic classification of Singapore's residential planning areas",
            main.title.size=1)
sg_gdc_z
tmap_save(sg_gdc_z, "GDC_z-cluster-map-color.png", height=5, width=7)

# De-normalize -----------
SE_for_kmeans <- SE_for_kmeans[-29,]
denormalised <- setNames(data.frame(matrix(ncol = 11, nrow = 5)), col_names)
for (i in 1:ncol(denormalised)){
  denormalised[i] <- centers[i]*(max(SE_for_kmeans[i])-min(SE_for_kmeans[i])) + min(SE_for_kmeans[i])
} 

write.csv(t(round(denormalised, 3)*100), "GDC_z-cluster-centres-denormalised.csv")

v1 <- list(1,2,3)
v2 <- data.frame(c(1,2,3))
vbound <- cbind(v1,v2)
class(vbound)
colnames(as.matrix(sg_means))
View(sg_means)
x<- c(12L, 6L, 10L, 8L)
is.numeric(median(x))

v <- 1:3
names <- c("a", "b", "c")
v[4] <- 4
names(v[4])
