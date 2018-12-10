library(dplyr)
library(tidyverse)
library(cluster)
library(factoextra)

# Load data ---------------------------------------------------------
weather_data <- read_csv("data/pm2.5_data_2010_2014.csv")
weather_data <- na.omit(weather_data)

# Pre-process data --------------------------------------------------

data <- read_csv("data/pm2.5_data_2010_2014.csv")
data <- na.omit(data)
data <- select(data, year:Ir)
data[5] <- d(data[5])
data[6] <- scale(data[6])
data[7] <- scale(data[7])
data[8] <- scale(data[8])
data[10] <- scale(data[10])
data[11] <- scale(data[11])
dim(data)


dim(weather_data)
names(weather_data)
head(weather_data)

class(weather_data)
str(weather_data)

df <- na.omit(weather_data)

# focus on clustering average value 
df_mean <- df %>% 
  select(month, pm2.5:PRES, Iws:Ir) %>% 
  group_by(month) %>% 
  summarize(pm2.5_mean = mean(pm2.5), dewp_mean = mean(DEWP), temp_mean = mean(TEMP), 
            pres_mean = mean(PRES),  iws_mean = mean(Iws), is_mean = mean(Is), 
            ir_mean = mean(Ir), na.rm=TRUE)

df_mean <- data.frame(df_mean)
head(df_mean)

# rename arrow uniquely
rownames(df_mean) <- df_mean[,1]
df_mean <- scale(df_mean[,2:8])

df_mean <- data.frame(df_mean)

# select featues to clsuter
df_cluster <- select(df_mean, c(pm2.5_mean, temp_mean))
head(df_cluster)

# k-means
k2 <- kmeans(df_cluster, centers = 2, nstart = 10)
str(k2)
fviz_cluster(k2, data = df_cluster)

# hierarchy clustering

# Dissimilarity matrix
dmx <- dist(df_cluster, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc <- hclust(dmx, method = "complete" )
# Plot the obtained dendrogram
plot(hc, cex = 0.6, hang = -1)
rect.hclust(hc, k = 2, border = 2:5)

# measure the value 
recom_subset <- weather_data %>%
  na.omit() %>%
  filter(pm2.5 <= 100) %>% 
  filter(TEMP <= 20, DEWP <= 15) %>% 
  filter(PRES <= 1000, PRES >= 500, Iws <= 100) %>% 
  filter(Is <= 4, Ir <= 0) 

value_cnt <- as.numeric(count(recom_subset))
if(value_cnt>0){
  recom_month <- unlist(select(recom_subset, month))
  recom_day <- unlist(select(recom_subset, day))
  paste("you can visit to BJ in day - month - year: ", recom_day, "-", recom_month, "-20XX")
} else {
  print("Reset your range !")
}


