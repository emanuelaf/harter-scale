# Mutual Conditional Entropy using all original items
# Generates figures A1 and A2

rm(list=ls())

# Import functions for computing mce
source('../rosenberg/funs.R')

# import data
mhs_long <- read.csv("mhs_long.csv")
mhs_long[mhs_long == '.'] <- NA
nrow(mhs_long)

mhs_long <- as.data.frame(sapply(mhs_long, as.numeric))
mhs_long

# modify time point as needed
timestamp <- 4
mhs_time <- mhs_long[mhs_long$time == timestamp,]
nrow(mhs_time[!is.na(rowSums(mhs_time[,9:58])),])

mce_matrix <- matrix(ncol=50, nrow=50)

for (q1 in 9:58) {
  #  for (q2 in (q1+1):10) {
  for (q2 in 9:58) {
    x1 <- table(mhs_time[[q1]],
                mhs_time[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(mhs_time[[q2]],
                mhs_time[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x2)[[1]]
    mce_matrix[q1-8, q2-8] <- mce(ce1, ce2)
  }
}

diag(mce_matrix) <- NA
colnames(mce_matrix) <- colnames(mhs_time[,9:58])
rownames(mce_matrix) <- colnames(mhs_time[,9:58])

col <- colorRampPalette(c("red", "white"))(20)

#png(file=paste0("mce_matrix_time", timestamp, '.png'), width = 1000, height = 1000)
heatmap(mce_matrix, col=col, symm=TRUE, zlim=c(0,1))
#dev.off()
# pa4 and pc4 are identical!

dimgroups <- c(rep('pc', 4), rep('pa', 4), rep('gsw', 4),
               rep('psw', 5), rep('ps', 5), rep('ts', 5),
               rep('cfs', 5), rep('cms', 5), rep('mot',5), 
               rep('enj', 3), rep('sd', 5))

library(qgraph)
#pdf(file=paste0("network_time",timestamp,'.pdf'), width = 5.9, height = 4)
qgraph(1-mce_matrix, shape='circle', posCol='darkblue', 
       labels = rownames(mce_matrix),
       negCol='darkred', layout="spring", threshold = 0.2, vsize = 4,
       groups=dimgroups, tuning = 0.5, legend = F)
#dev.off()




