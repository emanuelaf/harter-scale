###############################
#### Longitudinal analysis ####
###############################

# Computation of MCE and CE with lagged variables
# Generates Figures 5,6,7,8, A1 and A4
# Generates Tables 2, 5, Table A3 and A4

# Time 2
rm(list=ls())
timestamp <- 1
source('domain-variables-generation.R')
rm(list=setdiff(ls(), "df_reduced_variables"))
df_reduced_variables_time1 <- df_reduced_variables
names(df_reduced_variables_time1) <- paste0(names(df_reduced_variables_time1),
                                            "time1")
timestamp <- 2
source('domain-variables-generation.R')
df_reduced_variables_time2 <- left_join(df_reduced_variables, 
                                        df_reduced_variables_time1,
                                        by = c("ID"="IDtime1"))
df_reduced_variables_time2 <- df_reduced_variables_time2[!rowSums(is.na(df_reduced_variables_time2))>0,]
df_reduced_variables_time2_id <- df_reduced_variables_time2$ID
df_reduced_variables_time2 <- df_reduced_variables_time2 %>%
  select(-ID)
vars_number <- 1:(11*timestamp)
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time2)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables_time2)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(df_reduced_variables_time2[[q]],
              df_reduced_variables_time2[[response_number]], 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

mce_matrix <- matrix(ncol=11*timestamp, nrow=11*timestamp)

for (q1 in 1:(11*timestamp-1)) {
  #  for (q2 in (q1+1):10) {
  for (q2 in (q1+1):(11*timestamp)) {
    x1 <- table(df_reduced_variables_time2[[q1]],
                df_reduced_variables_time2[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(df_reduced_variables_time2[[q2]],
                df_reduced_variables_time2[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x1)[[1]]
    mce_matrix[q1, q2] <- mce(ce1,ce2)
    mce_matrix[q2, q1] <- mce(ce1,ce2)
  }
}

colnames(mce_matrix) <- str_remove(colnames(df_reduced_variables_time2), '_groups')
rownames(mce_matrix) <- str_remove(colnames(df_reduced_variables_time2), '_groups')

colors = rep(c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
               "red", "green", "#FFFF00", "#CC00CC"), timestamp)
col <- colorRampPalette(c("red", "white"))(20)

# Figure 5
library(qgraph)
# pdf(file=paste0("network_previous_time",timestamp,'.pdf'), width = 5.9, height = 4)
qgraph(1-mce_matrix, shape='circle', posCol='darkblue', threshold = 0.2,
       negCol='darkred', layout="spring", vsize = 6,
       color=colors, tuning = 0.5, legend = F)
# dev.off()
#png(file=paste0("mce_matrix_previous_time", timestamp, '.png'), width = 600, height = 600)
heatmap(mce_matrix, col=col, symm=TRUE, zlim=c(0,1))
#dev.off()

# Table 2
df_entropy_var <- data.frame(covariate = factor(na.omit(independent_name),
                                                levels=na.omit(independent_name)), 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation))

df_rowwise_entropy <- data.frame(covariate = factor(na.omit(independent_name),
                                                    levels = na.omit(independent_name)),
                                 na.omit(rowwise_entropy))

names(df_rowwise_entropy) <- c("covariate", 1:k)
df_rowwise_entropy <- tidyr::gather(df_rowwise_entropy, "level", "rowwise_ce", -covariate)

colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")
colors_var = c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

df_colors = data.frame(colors, colors_var)

color_ordered = df_colors %>%
  arrange(colors_var) %>% 
  select(colors)

# Figure 6
ggplot(df_rowwise_entropy) +
  geom_point(aes(x=level, y = rowwise_ce, col = covariate)) +
  facet_wrap(~covariate) +
  labs(title = paste('k=', k, ', time=', timestamp))+
  scale_color_manual(values=as.vector(unlist(rep(color_ordered, timestamp))[-response_number])) +
  theme(legend.position='none')

####################
####### Time 3 #####
####################

rm(list=setdiff(ls(), list("df_reduced_variables_time2", "df_reduced_variables_time2_id")))
names(df_reduced_variables_time2)[1:11] <- paste0(names(df_reduced_variables_time2)[1:11],
                                                  "time2")
df_reduced_variables_time2 <- data.frame(IDtime2 = df_reduced_variables_time2_id,
                                         df_reduced_variables_time2)
timestamp <- 3
source('domain-variables-generation.R')
df_reduced_variables_time3 <- left_join(df_reduced_variables, 
                                        df_reduced_variables_time2,
                                        by = c("ID"="IDtime2"))

df_reduced_variables_time3 <- df_reduced_variables_time3[!rowSums(is.na(df_reduced_variables_time3))>0,]
df_reduced_variables_time3_id <- df_reduced_variables_time3$ID
df_reduced_variables_time3 <- df_reduced_variables_time3 %>%
  select(-ID)
vars_number <- 1:(11*timestamp)
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time3)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables_time3)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(df_reduced_variables_time3[[q]],
              df_reduced_variables_time3[[response_number]], 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

mce_matrix <- matrix(ncol=11*timestamp, nrow=11*timestamp)

for (q1 in 1:(11*timestamp-1)) {
  #  for (q2 in (q1+1):10) {
  for (q2 in (q1+1):(11*timestamp)) {
    x1 <- table(df_reduced_variables_time3[[q1]],
                df_reduced_variables_time3[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(df_reduced_variables_time3[[q2]],
                df_reduced_variables_time3[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x1)[[1]]
    mce_matrix[q1, q2] <- mce(ce1,ce2)
    mce_matrix[q2, q1] <- mce(ce1,ce2)
  }
}

colnames(mce_matrix) <- str_remove(colnames(df_reduced_variables_time3), '_groups')
rownames(mce_matrix) <- str_remove(colnames(df_reduced_variables_time3), '_groups')

colors = rep(c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
               "red", "green", "#FFFF00", "#CC00CC"), timestamp)
col <- colorRampPalette(c("red", "white"))(20)

# Figure A3
library(qgraph)
#pdf(file=paste0("network_previous_time",timestamp,'.pdf'), width = 5.9, height = 4)
qgraph(1-mce_matrix, shape='circle', posCol='darkblue', threshold = 0.2,
       negCol='darkred', layout="spring", vsize = 6,
       color=colors, tuning = 0.5, legend = F)
#dev.off()
#png(file=paste0("mce_matrix_previous_time", timestamp, '.png'), width = 600, height = 600)
heatmap(mce_matrix, col=col, symm=TRUE, zlim=c(0,1))
#dev.off()

# Table A4
df_entropy_var <- data.frame(covariate = factor(na.omit(independent_name),
                                                levels=na.omit(independent_name)), 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation))

df_rowwise_entropy <- data.frame(covariate = factor(na.omit(independent_name),
                                                    levels = na.omit(independent_name)),
                                 na.omit(rowwise_entropy))

names(df_rowwise_entropy) <- c("covariate", 1:k)
df_rowwise_entropy <- tidyr::gather(df_rowwise_entropy, "level", "rowwise_ce", -covariate)

colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")
colors_var = c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

df_colors = data.frame(colors, colors_var)

color_ordered = df_colors %>%
  arrange(colors_var) %>% 
  select(colors)

# Figure A4
ggplot(df_rowwise_entropy) +
  geom_point(aes(x=level, y = rowwise_ce, col = covariate)) +
  facet_wrap(~covariate) +
  labs(title = paste('k=', k, ', time=', timestamp))+
  scale_color_manual(values=as.vector(unlist(rep(color_ordered, timestamp))[-response_number])) +
  theme(legend.position='none')

################
#### Time 4 ####
################

rm(list=setdiff(ls(), list("df_reduced_variables_time3", "df_reduced_variables_time3_id")))
names(df_reduced_variables_time3)[1:11] <- paste0(names(df_reduced_variables_time3)[1:11],
                                                  "time3")
df_reduced_variables_time3 <- data.frame(IDtime3 = df_reduced_variables_time3_id,
                                         df_reduced_variables_time3)
timestamp <- 4
source('domain-variables-generation.R')
df_reduced_variables_time4 <- left_join(df_reduced_variables, 
                                        df_reduced_variables_time3,
                                        by = c("ID"="IDtime3"))

df_reduced_variables_time4 <- df_reduced_variables_time4[!rowSums(is.na(df_reduced_variables_time4))>0,]
df_reduced_variables_time4_id <- df_reduced_variables_time4$ID
df_reduced_variables_time4 <- df_reduced_variables_time4 %>%
  select(-ID)
vars_number <- 1:(11*timestamp)
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time4)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables_time4)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(df_reduced_variables_time4[[q]],
              df_reduced_variables_time4[[response_number]], 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

mce_matrix <- matrix(ncol=11*timestamp, nrow=11*timestamp)

for (q1 in 1:(11*timestamp-1)) {
  #  for (q2 in (q1+1):10) {
  for (q2 in (q1+1):(11*timestamp)) {
    x1 <- table(df_reduced_variables_time4[[q1]],
                df_reduced_variables_time4[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(df_reduced_variables_time4[[q2]],
                df_reduced_variables_time4[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x1)[[1]]
    mce_matrix[q1, q2] <- mce(ce1,ce2)
    mce_matrix[q2, q1] <- mce(ce1,ce2)
  }
}

colnames(mce_matrix) <- colnames(df_reduced_variables_time4)
rownames(mce_matrix) <- colnames(df_reduced_variables_time4)

colors = rep(c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
               "red", "green", "#FFFF00", "#CC00CC"), timestamp)
col <- colorRampPalette(c("red", "white"))(20)

# Figure 7
library(qgraph)
#pdf(file=paste0("network_previous_time",timestamp,'.pdf'), width = 5.9, height = 4)
qgraph(1-mce_matrix, shape='circle', posCol='darkblue', threshold = 0.2,
       negCol='darkred', layout="spring", vsize = 6,
       color=colors, tuning = 0.5, legend = F)
#dev.off()

#png(file=paste0("mce_matrix_previous_time", timestamp, '.png'), width = 600, height = 600)
heatmap(mce_matrix, col=col, symm=TRUE, zlim=c(0,1))
#dev.off()

# Table 5
df_entropy_var <- data.frame(covariate = factor(na.omit(independent_name),
                                                levels=na.omit(independent_name)), 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation))

df_rowwise_entropy <- data.frame(covariate = factor(na.omit(independent_name),
                                                    levels = na.omit(independent_name)),
                                 na.omit(rowwise_entropy))

names(df_rowwise_entropy) <- c("covariate", 1:k)
df_rowwise_entropy <- tidyr::gather(df_rowwise_entropy, "level", "rowwise_ce", -covariate)

colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")
colors_var = c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

df_colors = data.frame(colors, colors_var)

color_ordered = df_colors %>%
  arrange(colors_var) %>% 
  select(colors)

# Figure 8
ggplot(df_rowwise_entropy) +
  geom_point(aes(x=level, y = rowwise_ce, col = covariate)) +
  facet_wrap(~covariate) +
  labs(title = paste('k=', k, ', time=', timestamp))+
  scale_color_manual(values=as.vector(unlist(rep(color_ordered, timestamp))[-response_number])) +
  theme(legend.position='none')

### Two-way analysis for time = 4
table(df_reduced_variables_time4$psw_groupstime3)
split1 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groupstime3 == 1 |
                                       df_reduced_variables_time4$psw_groupstime3 == 2 ,]
split2 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groupstime3 == 3 ,]
split3 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groupstime3 == 4 ,]

vars_number <- 1:(11*timestamp)
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time4)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables_time4)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split1[[q]], levels = c(1,2,3,4)),
              factor(split1[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split1 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

#####
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split2[[q]], levels = c(1,2,3,4)),
              factor(split2[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split2 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

############
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split3[[q]], levels = c(1,2,3,4)),
              factor(split3[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split3 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

#####
table(df_reduced_variables_time4$psw_groups)
split1 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groups == 1 |
                                       df_reduced_variables_time4$psw_groups == 2 ,]
split2 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groups == 3 ,]
split3 <- df_reduced_variables_time4[df_reduced_variables_time4$psw_groups == 4 ,]

vars_number <- 1:(11*timestamp)
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time4)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables_time4)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split1[[q]], levels = c(1,2,3,4)),
              factor(split1[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split1 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

#####
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split2[[q]], levels = c(1,2,3,4)),
              factor(split2[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split2 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

############
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL

for (q in independent_number) {
  x1 <- table(factor(split3[[q]], levels = c(1,2,3,4)),
              factor(split3[[response_number]], levels = c(1,2,3,4)), 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var_split3 <- data.frame(covariate = factor(na.omit(independent_name),
                                                       levels=na.omit(independent_name)), 
                                    entropy_explained = na.omit(entropy_explained),
                                    entropy_var = na.omit(entropy_variation))

