#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Green Leader paper
# 2021-07-23
# PCA and clustering
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # Numerous data wrangling packages
library(data.table)   # Quick data loading
library(lubridate)    # Working with dates
library(RColorBrewer) # Nice colours for plots
library(ggpubr)       # Arrange multiple plots
library(scales)       # Special scales for plots (log, %)

library("ggallin")    # Negative pseudo log scale (https://stackoverflow.com/questions/20924705/plot-negative-values-in-logarithmic-scale-with-ggplot-2)
library(pheatmap)     # Cluster heatmap


#%#%#%#%#%#%#%#%#%#%
# Loading data
#%#%#%#%#%#%#%#%#%#%

df <- fread(paste(getwd(), "/Fig1_data.csv", sep = ""))

df <- as.data.frame(df)

df <- df %>% mutate(GreenLeaderBinary = ifelse(GreenLeaderBinary == 1, "Green Leader", "Other"))

df2 <- df

# Identify continuous variables
continuous_vars <- c("HotelClass", "AvgRating", "PriceRangeLow", "PriceRangeHigh", "NumberRooms","LanguagesSpoken", 
  "TotalPhotos","DiningPhotos", "PoolBeachPhotos", "RoomSuitePhotos", "TravelerPhotos", 
  "VideosPhotos","ReviewCount", "ExcellentReviewCount", 
  "VeryGoodReviewCount", "AverageReviewCount", 
  "PoorReviewCount", "TerribleReviewCount", "QACount", "RoomTipCount", "LocationRating",
  "CleanlinessRating", "ServiceRating", "ValueRating", "WalkerScore", "RestaurantScore", 
  "AttractionsScore", "DescriptionLengthWords", "AmenitiesSectionLength", "RoomFeatSectionLength",
  "AreaComparison", "EnglishReviewProportion", "LocalReviewProportion")

# Just keep the continuous variables
df_cont <- df2 %>% select(continuous_vars)

# Scale the data (z-scores) before PCA
df_s <- scale(df_cont)

#%#%#%#%
# PCA
#%#%#%#%

# Get the principal components
df_pca <- princomp(df_s)

# Keep the first four PCs
df_pca_4 <- data.frame(df_pca$scores[,c(1,2,3,4)])

# Add to the main data set
df2 <- cbind(df2, df_pca_4)

#%#%#%#%
# Clustering
#%#%#%#%

# On the scaled data, run k-means clustering with four clusters
df_k <- kmeans(df_s, 4, nstart = 20, iter.max = 50)

# Add the cluster labels to the main data frame
clusters <- df_k$cluster
df2 <- df2 %>% mutate(cluster = clusters)

# Add green label again
df2 <- df2 %>% mutate(green = ifelse(GreenLeaderBinary == "Green Leader", 1, 0))

#%#%#%#%
# Table (Fig 2B)
#%#%#%#%

# Calculate main variables by cluster for the table (Overleaf)
df2 %>% group_by(cluster) %>% summarise(count = n(), 
                                        stars = mean(HotelClass), 
                                        rooms = median(NumberRooms),
                                        photos = median(TotalPhotos / NumberRooms),
                                        reviews = median(ReviewCount / NumberRooms),
                                        price = median((PriceRangeHigh + PriceRangeLow)/2),
                                        englishreviews = median(EnglishReviewProportion),
                                        green = mean(green)) %>% mutate(rel = count / sum(count))

#%#%#%#%
# Visualisation of Clusters in PCA dimensions (Fig 2C)
#%#%#%#%

# We need to make sure to always have the same order of the clusters for the plot
cluster_order <- df2 %>% group_by(cluster) %>% summarise(count = n()) %>% arrange(count) %>% mutate(cluster2 = c(1,2,3,4)) %>% select(-count)

# Merge data frame and cluster order to a fixed cluster order
df2 <- merge(df2, cluster_order, by = "cluster")

# For the plot, name the clusters
df2 <- df2 %>% mutate(cluster_name = ifelse(cluster2 == 1, "Cluster 1: large, high-class hotels\n(19% Green Leader)",
                                            ifelse(cluster2 == 2, "Cluster 2: small, high-class hotels\n(11% Green Leader)",
                                                   ifelse(cluster2 == 3, "Cluster 3: low price hotels\n(2% Green Leader)",
                                                          ifelse(cluster2 == 4, "Cluster 4: other accommodations\n(2% Green Leader)", "other")))))

# Individual ordering for alpha values in the figure
df2 <- df2 %>% mutate(cluster_gl = ifelse(cluster2 == 1 & green ==1, 1,
                                          ifelse(cluster2 == 1 & green ==0, 2,
                                                 ifelse(cluster2 == 2 & green ==1, 3,
                                                        ifelse(cluster2 == 2 & green ==0, 4,
                                                               ifelse(cluster2 == 3 & green ==1, 5,
                                                                      ifelse(cluster2 == 3 & green ==0, 6,
                                                                             ifelse(cluster2 == 4 & green ==1, 7,
                                                                                    ifelse(cluster2 == 4 & green ==0, 8,9)))))))))


# Plot
df2 %>% sample_frac(0.1) %>%
  ggplot(aes(x = Comp.1, y = Comp.2, fill = GreenLeaderBinary,colour = GreenLeaderBinary,
             alpha = factor(cluster_gl))) + 
  geom_hline(yintercept = 0, lty = 3, colour = "grey", lwd = 0.5) +
  geom_vline(xintercept = 0, lty = 3, colour = "grey", lwd = 0.5) +
  geom_point(shape = 21, size = 2.5) +
  stat_ellipse(type = "norm", linetype = 1, alpha = 1, level = 0.66, lwd = 1, show.legend = "none") +      
  scale_alpha_manual(values = c(0.8,0.25,0.8,0.05,0.8,0.05,0.8,0.015), guide = F) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[10],
                                 brewer.pal(11,"RdBu")[2])) + 
  facet_wrap(~cluster_name) + 
  theme_minimal() +
  scale_x_continuous(trans = pseudolog10_trans, breaks = c(-10,-5,0,5,10,20)) +
  scale_y_continuous(trans = pseudolog10_trans, breaks = c(-10,-5,0,5,10,20)) +
  annotation_logticks(colour = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.8)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.8) +
  labs(x= "First Principal Component", y = "Second Principal Component",
       fill = "", colour = "", alpha = "") +
  theme(text = element_text(size = 13), legend.text = element_text(size = 13),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid = element_blank(), 
        legend.position = "bottom") +
  guides(fill=guide_legend(override.aes=list(alpha=1)))


#%#%#%#% 
# Plot the loadings heatmap (Fig 2A)
#%#%#%#%

# Get the loadings from the PC data
loadings <- as.data.frame(df_pca$loadings[,1:4])

# Rename the PC1
names(loadings) <- c("PC1", "PC2", "PC3", "PC4")

# Proper variable names (columns)
rownames(loadings) <- c("Hotel class (stars)","Avg. Rating","Price range (low)","Price range (high)",         
"Number of rooms","No. of languages spoken","Total no. of photos","No. of dining photos",           
"No. of pool/beach photos","No. of room/suite photos","No. of traveller photos","No. of videos/photos",           
"Review count (total)","Review count: excellent ","Review count: very good","Review count: average",     
"Review count: poor","Review count: terrible","Q&A count","Room tips count",           
"Location rating ","Cleanliness rating","Service rating","Value rating",            
"Walker score","Restaurant score","Attractions score","Description length", 
"Amenities desc. length","Room features desc. length","Area comparison rating","English review share",
"Local review share")  

# Use pheatmap() to plot the heatmap
pheatmap(loadings, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")), bias = 0.75)(100),
         treeheight_col = 0, angle_col = 0, cluster_cols = 0, 
         cutree_rows = 4, cutree_cols = 4, legend_labels = c(0.5,0.1))

# END OF SCRIPT
#==============