# Data manipaulation for dimensionality reduction
# included k-means algorithm implementation and ordering of the output groups
# The final output is the dataset with domain variables

set.seed(123)
require(tidyverse)
library(plyr)

# import functions for conditional entropy computations
source('../rosenberg/funs.R')

# import dataset
mhs_long <- read.csv("mhs_long.csv")
mhs_long[mhs_long == '.'] <- NA
nrow(mhs_long)
mhs_long <- as.data.frame(sapply(mhs_long, as.numeric))

# modify k according to the number of groups to be output by the k-means algorithm
k <- 4
# modify time point as needed
# timestamp <- 1
mhs_time <- mhs_long[mhs_long$time == timestamp,]

mhs_time_reduced <- mhs_time[,c(1,9:58)]
mhs_time_reduced_clean <- mhs_time_reduced[!rowSums(is.na(mhs_time_reduced))>0,]
nrow(mhs_time_reduced_clean)

mhs_time_reduced_clean_pc <- mhs_time_reduced_clean[,c("pc1", "pc2", "pc3", "pc4")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_pc), centers = k, nstart = 50)
pc_groups <- as.factor(kmeans_res$cluster)
reordering <- data.frame(mhs_time_reduced_clean_pc,
                          cluster = pc_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
pc_groups <- factor(mapvalues(pc_groups, from = 1:k,to = new_levels),
                    levels = 1:4)

mhs_time_reduced_clean_pa <-  mhs_time_reduced_clean[,c("pa1", "pa2", "pa3", "pa4")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_pa), centers = k, nstart = 50)
pa_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_pa,
                          cluster = pa_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
pa_groups <- factor(mapvalues(pa_groups, from = 1:k,to = new_levels),
                       levels = 1:4)


mhs_time_reduced_clean_gsw <-  mhs_time_reduced_clean[,c("gsw1", "gsw2", "gsw3", "gsw4")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_gsw), centers = k, nstart = 50)
gsw_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_gsw,
                         cluster = gsw_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
gsw_groups <- factor(mapvalues(gsw_groups, from = 1:k,to = new_levels),
                     levels = 1:4)

mhs_time_reduced_clean_psw <-  mhs_time_reduced_clean[,c("psw1", "psw2", "psw3", "psw4", "psw5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_psw), centers = k, nstart = 50)
psw_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_psw,
                         cluster = psw_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
psw_groups <- factor(mapvalues(psw_groups, from = 1:k,to = new_levels),
                     levels = 1:4)

mhs_time_reduced_clean_ps <-  mhs_time_reduced_clean[,c("ps1", "ps2", "ps3", "ps4", "ps5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_ps), centers = k, nstart = 50)
ps_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_ps,
                          cluster = ps_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
ps_groups <- factor(mapvalues(ps_groups, from = 1:k,to = new_levels),
                    levels = 1:4)

mhs_time_reduced_clean_ts <-  mhs_time_reduced_clean[,c("ts1", "ts2", "ts3", "ts4", "ts5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_ts), centers = k, nstart = 50)
ts_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_ts,
                         cluster = ts_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
ts_groups <- factor(mapvalues(ts_groups, from = 1:k,to = new_levels),
                    levels = 1:4)


mhs_time_reduced_clean_cfs <- mhs_time_reduced_clean[,c("cfs1", "cfs2", "cfs3", "cfs4", "cfs5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_cfs), centers = k, nstart = 50)
cfs_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_cfs,
                          cluster = cfs_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
cfs_groups <- factor(mapvalues(cfs_groups, from = 1:k,to = new_levels),
                     levels = 1:4)


mhs_time_reduced_clean_cms <- mhs_time_reduced_clean[,c("cms1", "cms2", "cms3", "cms4", "cms5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_cms), centers = k, nstart = 50)
cms_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_cms,
                         cluster = cms_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
cms_groups <- factor(mapvalues(cms_groups, from = 1:k,to = new_levels),
                     levels = 1:4)

mhs_time_reduced_clean_mot <- mhs_time_reduced_clean[,c("mot1", "mot2", "mot3", "mot4", "mot5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_mot), centers = k, nstart = 50)
mot_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_mot,
                         cluster = mot_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
mot_groups <- factor(mapvalues(mot_groups, from = 1:k,to = new_levels),
                     levels = 1:4)

mhs_time_reduced_clean_enj <- mhs_time_reduced_clean[,c("enj1", "enj2", "enj3")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_enj), centers = k, nstart = 50)
enj_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_enj,
                        cluster = enj_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
enj_groups <- factor(mapvalues(enj_groups, from = 1:k,to = new_levels),
                     levels = 1:4)

mhs_time_reduced_clean_sd <- mhs_time_reduced_clean[,c("sd1", "sd2", "sd3", "sd4", "sd5")]
kmeans_res <- kmeans(dist(mhs_time_reduced_clean_sd), centers = k, nstart = 50)
sd_groups <- kmeans_res$cluster
reordering <- data.frame(mhs_time_reduced_clean_sd,
                        cluster = sd_groups)
new_levels <- rank(colMeans(sapply(split(reordering[,-ncol(reordering)], reordering$cluster), colMeans)))
sd_groups <- factor(mapvalues(sd_groups, from = 1:k,to = new_levels),
                    levels = 1:4)

df_reduced_variables <- data.frame(ID = mhs_time_reduced_clean$ID,
                                   pc_groups, pa_groups, gsw_groups, psw_groups,
                                   ps_groups, ts_groups, cfs_groups, cms_groups,
                                   mot_groups, enj_groups, sd_groups)


