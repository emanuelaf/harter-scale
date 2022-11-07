##########################
# Major Factor Selection #
##########################

# Time 3
# Generates Table A4, Table A6 and Table A6

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
names(df_reduced_variables_time2)[1:12] <- paste0(names(df_reduced_variables_time2)[1:12],
                                                  "time2")

rm(list=setdiff(ls(), list("df_reduced_variables", "df_reduced_variables_time2")))
timestamp <- 3
source('domain-variables-generation.R')
df_reduced_variables_time3 <- left_join(df_reduced_variables, 
                                        df_reduced_variables_time2,
                                        by = c("ID"="IDtime2"))

df_reduced_variables_time3 <- df_reduced_variables_time3[!rowSums(is.na(df_reduced_variables_time3))>0,]
df_reduced_variables_time3 <- df_reduced_variables_time3 %>%
  select(-ID)
vars_number <- 1:(11*timestamp)
response_number <- 3
response_name <- colnames(df_reduced_variables_time3)[response_number]
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables_time3)[!vars_number %in% response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
entropy_variation <- NULL
entropy_explained <- NULL
rowwise_entropy_var <- NULL

for (q in independent_number) {
  x1 <- table(df_reduced_variables_time3[[q]],
              df_reduced_variables_time3[[response_number]])
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
  rowwise_entropy_var[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[2]])^2*rowSums(x1)/sum(x1))
}

rm(list="x1")

df_entropy_var <- data.frame(covariate = factor(na.omit(independent_name),
                                                levels=na.omit(independent_name)), 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation),
                             rowwise_entropy_var = na.omit(rowwise_entropy_var))

df_entropy_var_to_print <- dplyr::arrange(df_entropy_var, desc(entropy_explained))

# Table A4
xtable::xtable(df_entropy_var_to_print, digits=3)

####
df_entropy_var <- data.frame(covariate = factor(na.omit(independent_name),
                                                levels=na.omit(independent_name)), 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation))

top_4 <- head(df_entropy_var[order(df_entropy_var$entropy_explained, decreasing = T),]$covariate,4)
freq <- t(sapply(df_reduced_variables_time3[, colnames(df_reduced_variables_time3) %in% top_4], table))[as.character(top_4),]

df_entropy_var[order(df_entropy_var$entropy_explained, decreasing = T),]
# Table A5
xtable::xtable(df_entropy_var[order(df_entropy_var$entropy_explained, decreasing = T),], digits=c(0,0,3,4))

rm(list=c('df_entropy_var'))

### ROW 1
split1 <- df_reduced_variables_time3[df_reduced_variables_time3$psw_groups == 1 |
                                       df_reduced_variables_time3$psw_groups == 2, ]
split2 <- df_reduced_variables_time3[df_reduced_variables_time3$psw_groups == 3 ,]
split3 <- df_reduced_variables_time3[df_reduced_variables_time3$psw_groups == 4,]
splitted_datasets <- list(split1, split2, split3)
df_entropy_var_split <- list()
top_3_split <- list()

for (i in 1:3) {
  rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
  entropy_variation <- NULL
  entropy_explained <- NULL
  for (q in independent_number[!(independent_name %in% top_4[1])]) {
    x1 <- table(factor(splitted_datasets[[i]][[q]], levels = c(1,2,3,4)),
                factor(splitted_datasets[[i]][[response_number]], levels = c(1,2,3,4)))
    rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
    entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
    entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
    rm('x1')
  }
  df_entropy_var_split[[i]] <- data.frame(covariate = factor(na.omit(independent_name[!(independent_name %in% top_4[1])])), 
                                          entropy_explained = na.omit(entropy_explained),
                                          entropy_var = na.omit(entropy_variation))
  top_3_split[[i]] <- head(df_entropy_var_split[[i]][order(df_entropy_var_split[[i]]$entropy_explained, decreasing = T),]$covariate,3)
}

row_1 <- c(paste(paste0('(', paste(str_remove(as.character(top_3_split[[1]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[2]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[3]]), '_groups'), collapse = (',')),
                        ')'), "\\"))

#### ROW 2
split1 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groups == 1 |
                                       df_reduced_variables_time3$pa_groups == 2,]
split2 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groups == 3 ,]
split3 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groups ==  4,]
splitted_datasets <- list(split1, split2, split3)
df_entropy_var_split <- list()
top_3_split <- list()

for (i in 1:3) {
  rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
  entropy_variation <- NULL
  entropy_explained <- NULL
  for (q in independent_number[!(independent_name %in% top_4[2])]) {
    x1 <- table(factor(splitted_datasets[[i]][[q]], levels = c(1,2,3,4)),
                factor(splitted_datasets[[i]][[response_number]], levels = c(1,2,3,4)))
    rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
    entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
    entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
    rm('x1')
  }
  df_entropy_var_split[[i]] <- data.frame(covariate = factor(na.omit(independent_name[!(independent_name %in% top_4[2])])), 
                                          entropy_explained = na.omit(entropy_explained),
                                          entropy_var = na.omit(entropy_variation))
  top_3_split[[i]] <- head(df_entropy_var_split[[i]][order(df_entropy_var_split[[i]]$entropy_explained, decreasing = T),]$covariate,3)
}

row_2 <- c(paste(paste0('(', paste(str_remove(as.character(top_3_split[[1]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[2]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[3]]), '_groups'), collapse = (',')),
                        ')'), "\\"))

#### ROW 3
split1 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groupstime2 == 1 |
                                       df_reduced_variables_time3$pa_groupstime2 == 2,]
split2 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groupstime2 == 3 ,]
split3 <- df_reduced_variables_time3[df_reduced_variables_time3$pa_groupstime2 ==  4,]
splitted_datasets <- list(split1, split2, split3)
df_entropy_var_split <- list()
top_3_split <- list()

for (i in 1:3) {
  rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
  entropy_variation <- NULL
  entropy_explained <- NULL
  for (q in independent_number[!(independent_name %in% top_4[3])]) {
    x1 <- table(factor(splitted_datasets[[i]][[q]], levels = c(1,2,3,4)),
                factor(splitted_datasets[[i]][[response_number]], levels = c(1,2,3,4)))
    rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
    entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
    entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
    rm('x1')
  }
  df_entropy_var_split[[i]] <- data.frame(covariate = factor(na.omit(independent_name[!(independent_name %in% top_4[2])])), 
                                          entropy_explained = na.omit(entropy_explained),
                                          entropy_var = na.omit(entropy_variation))
  top_3_split[[i]] <- head(df_entropy_var_split[[i]][order(df_entropy_var_split[[i]]$entropy_explained, decreasing = T),]$covariate,3)
}

row_3 <- c(paste(paste0('(', paste(str_remove(as.character(top_3_split[[1]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[2]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[3]]), '_groups'), collapse = (',')),
                        ')'), "\\"))

### ROW 4
split1 <- df_reduced_variables_time3[df_reduced_variables_time3$sd_groups == 1 |
                                       df_reduced_variables_time3$sd_groups == 2,]
split2 <- df_reduced_variables_time3[df_reduced_variables_time3$sd_groups == 3 ,]
split3 <- df_reduced_variables_time3[df_reduced_variables_time3$sd_groups ==  4,]
splitted_datasets <- list(split1, split2, split3)
df_entropy_var_split <- list()
top_3_split <- list()

for (i in 1:3) {
  rowwise_entropy <- matrix(ncol=k, nrow=11*timestamp)
  entropy_variation <- NULL
  entropy_explained <- NULL
  for (q in independent_number[!(independent_name %in% top_4[4])]) {
    x1 <- table(factor(splitted_datasets[[i]][[q]], levels = c(1,2,3,4)),
                factor(splitted_datasets[[i]][[response_number]], levels = c(1,2,3,4)))
    rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
    entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
    entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
    rm('x1')
  }
  df_entropy_var_split[[i]] <- data.frame(covariate = factor(na.omit(independent_name[!(independent_name %in% top_4[4])])), 
                                          entropy_explained = na.omit(entropy_explained),
                                          entropy_var = na.omit(entropy_variation))
  top_3_split[[i]] <- head(df_entropy_var_split[[i]][order(df_entropy_var_split[[i]]$entropy_explained, decreasing = T),]$covariate,3)
}

row_4 <- c(paste(paste0('(', paste(str_remove(as.character(top_3_split[[1]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[2]]), '_groups'), collapse = (',')),
                        ')'), "&",
                 paste0('(', paste(str_remove(as.character(top_3_split[[3]]), '_groups'), collapse = (',')),
                        ')'), "\\"))

results <- rbind(row_1, row_2, row_3, row_4)
rownames(results) <- top_4

# Table A6
results
