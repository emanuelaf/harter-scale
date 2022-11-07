# Mutual Conditional Entropy using domain variables
# Generates Figure 1 and Figure 2

timestamp <- 4
source('domain-variables-generation.R')
ce_matrix <- matrix(ncol=11, nrow=11)
df_reduced_variables <- df_reduced_variables %>%
  select(-ID)

for (q1 in 1:10) {
  #  for (q2 in (q1+1):10) {
  for (q2 in (q1+1):11) {
    x1 <- table(df_reduced_variables[[q1]],
                df_reduced_variables[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    ce_matrix[q1, q2] <- ce1
  }
}

for (q1 in 1:10) {
  #  for (q2 in (q1+1):10) {
  for (q2 in (q1+1):11) {
    x1 <- table(df_reduced_variables[[q2]],
                df_reduced_variables[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x1)[[1]]
    ce_matrix[q2, q1] <- ce2
  }
}

colnames(ce_matrix) <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')
rownames(ce_matrix) <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")

qgraph::qgraph(1-ce_matrix,mode="direct",
               shape='circle', posCol='darkblue', threshold = 0.2,
               negCol='darkred', layout="spring", vsize = 6,
               color=colors, tuning = 0.5, legend = F, edge.labels=TRUE)

########

df <- cbind(mhs_time_reduced_clean, df_reduced_variables)
require(dplyr)
means <- df[,startsWith(colnames(df), 'pa')] %>%
  group_by(pa_groups) %>%
  summarize_all(mean) 

as.matrix(means)
cbind(as.matrix(means), rowMeans(as.matrix(means)[,2:5]))

# MCE on reduced number of variables
mce_matrix <- matrix(ncol=11, nrow=11)

for (q1 in 1:11) {
  #  for (q2 in (q1+1):10) {
  for (q2 in 1:11) {
    x1 <- table(df_reduced_variables[[q1]],
                df_reduced_variables[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(df_reduced_variables[[q2]],
                df_reduced_variables[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x2)[[1]]
    mce_matrix[q1, q2] <- mce(ce1, ce2)
  }
}

diag(mce_matrix) <- NA

colnames(mce_matrix) <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')
rownames(mce_matrix) <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

col <- colorRampPalette(c("red", "white"))(20)
#png(file=paste0("mce_matrix_reduced_time", timestamp, '.png'), width = 600, height = 600)
heatmap(mce_matrix, col=col, symm=TRUE, zlim=c(0,1))
#dev.off()


dimgroups <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')
colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")

library(qgraph)
#pdf(file=paste0("network_reduced_time",timestamp,'.pdf'), width = 5.9, height = 4)
qgraph(1-mce_matrix, shape='circle', posCol='darkblue', threshold = 0.2,
       negCol='darkred', layout="spring", vsize = 6,
       color=colors, tuning = 0.5, legend = F)
#dev.off()