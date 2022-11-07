# Generates Figure 4 and Table 1
# gsw as response variable
# set k in the domain variables generation script

rm(list=ls())
require(tidyverse)
# Choose time point
timestamp <- 4
# Includes relevant dataset
source('domain-variables-generation.R')

# Manipulate dataset
df_reduced_variables <- df_reduced_variables %>% select(-ID)

# Prepare for looping over all covariates in order to calculate 10 Conditional entropies
vars_number <- 1:11
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11)
ce <- NULL

# Looping over all the covariates
for (q in independent_number) {
  x1 <- table(df_reduced_variables[[q]],
              df_reduced_variables[[response_number]], 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  ce[q] <- conditional_entropy(x1)[[1]]
}

# Prepare dataset containing conditional entropy meausures
rowwise_entropy <- rowwise_entropy[rowSums(is.na(rowwise_entropy))==0,]
rownames(rowwise_entropy) <- independent_name
colnames(rowwise_entropy) <- 1:k
df_rowwise_entropy <- as.data.frame(rowwise_entropy)
df_rowwise_entropy$var <- rownames(df_rowwise_entropy)
df_rowwise_entropy <- tidyr::gather(df_rowwise_entropy, "level", "rowwise_ce", -var)

df_rowwise_entropy <- dplyr::arrange(df_rowwise_entropy, var)
df_rowwise_entropy$level <- as.numeric(df_rowwise_entropy$level)
df <- cbind(mhs_time_reduced_clean, df_reduced_variables)

name_starts <- c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')
independent_name_starts <- name_starts[!vars_number %in% response_number]

new_levels <- data.frame(var = c(), level = c(), new_value = c())

for (q in 1:length(independent_name_starts)) {
  tmp <- df[,startsWith(colnames(df), independent_name_starts[q])] %>%
  group_by_at(independent_name[q]) %>%
  summarize_all(mean) 
  new_levels <- rbind(new_levels,
                      data.frame(var = colnames(tmp)[1], 
                                 level = 1:k, 
                                 new_value = rank(rowMeans(tmp[,2:ncol(tmp)]))))
}

df_rowwise_entropy_final <- left_join(df_rowwise_entropy, new_levels)

require(ggplot2)
colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")
colors_var = c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

df_colors = data.frame(colors, colors_var)

color_ordered = df_colors %>%
  arrange(colors_var) %>% 
  filter(colors_var %in% independent_name_starts) %>%
  select(colors)

df_rowwise_entropy$var <- str_remove(df_rowwise_entropy$var, "_groups")

ggplot(df_rowwise_entropy)+
  geom_point(aes(x=level, y = rowwise_ce, col = var)) +
#  labs(xlab=)+
  facet_wrap(~var) +
  scale_color_manual(values=color_ordered$colors) +
  theme(legend.position='none')

# ADD C.I.

mimicked_matrix <- matrix(NA, ncol = k, nrow =k)
list_mimicked_matrix_m <- replicate(1000, mimicked_matrix, simplify=F)
conditional_entropies_distribution <- matrix(ncol=1000, nrow=k)
df_ci <- data.frame(lower_bound=numeric(), 
                    upper_bound=numeric(),
                    var = character(),
                    level = numeric())

for (q in 1:length(independent_number)) {
  x1 <- table(df_reduced_variables[[independent_number[q]]],
              df_reduced_variables[[response_number]], 
              deparse.level = 2)
  for (r in 1:1000) {
    for (i in 1:nrow(x1)) {
      v_prob = x1[i,]/sum(x1[i,])
      n = sum(x1[i,])
      list_mimicked_matrix_m[[r]][i,] <- rmultinom(1, n, prob = v_prob)
    }
    conditional_entropies_distribution[, r] <- 
      conditional_entropy(list_mimicked_matrix_m[[r]])[[4]]
  }
  lower_bound <- apply(conditional_entropies_distribution, 1, quantile, 0.025, na.rm =T)
  upper_bound <- apply(conditional_entropies_distribution, 1, quantile, 0.975, na.rm=T)
  df_ci <- rbind(df_ci,
                 data.frame(lower_bound, upper_bound, 
                            var = independent_name[q], 
                            level = 1:k))
}
new_levels$var <- str_remove(new_levels$var, "_groups")
df_ci$var <- str_remove(df_ci$var, "_groups")
df_rowwise_entropy_final_ci <- left_join(df_rowwise_entropy, new_levels) %>%
  left_join(df_ci)

colors = c('#33CCFF', '#00FFCC', "#00CC33", "#6600CC", "blue" , "hotpink", "orange" ,
           "red", "green", "#FFFF00", "#CC00CC")
colors_var = c('pc','pa', 'gsw', 'psw', 'ps', 'ts', 'cfs', 'cms', 'mot', 'enj', 'sd')

df_colors = data.frame(colors, colors_var)

color_ordered = df_colors %>%
  arrange(colors_var) %>% 
  filter(colors_var %in% independent_name_starts) %>%
  select(colors)


p <- ggplot(df_rowwise_entropy_final_ci)+
  geom_point(aes(x=new_value, y = rowwise_ce, col = var)) +
  geom_errorbar(aes(x = new_value, 
                    ymin=lower_bound, 
                    ymax=upper_bound,
                    col = var), 
                width=.1) +
  xlab("level") +
  ylab("Row-wise conditional entropy")+
  facet_wrap(~var) +
  labs(title = paste('k=', k, ', time=', timestamp))+
  scale_color_manual(values=color_ordered$colors) +
  theme(legend.position='none')

p

#png(paste0("rowwise_CE", response_name, "time", timestamp, ".png"),
#    width = 700, height = 500)
plot(p)
#dev.off()

############################################
# CE and Between groups entropy variation
############################################

vars_number <- 1:11
response_number <- 3
independent_number <- vars_number[!vars_number %in% response_number]
independent_name <- colnames(df_reduced_variables)[!vars_number %in% response_number]
response_name <- colnames(df_reduced_variables)[response_number]
rowwise_entropy <- matrix(ncol=k, nrow=11)
entropy_variation <- NULL
entropy_explained <- NULL
rowwise_entropy_var <- NULL

for (q in independent_number) {
  x1 <- table(df_reduced_variables[[q]],
              df_reduced_variables[[response_number]], 
              deparse.level = 2)
  rowwise_entropy[q,] <- conditional_entropy(x1)[[4]]
  entropy_explained[q] <- conditional_entropy(x1)[[3]]-conditional_entropy(x1)[[2]]
  entropy_variation[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[3]])^2*rowSums(x1)/sum(x1))
  rowwise_entropy_var[q] <- sum((rowwise_entropy[q,] - conditional_entropy(x1)[[2]])^2*rowSums(x1)/sum(x1))
}

df_entropy_var <- data.frame(covariate = independent_name, 
                             entropy_explained = na.omit(entropy_explained),
                             entropy_var = na.omit(entropy_variation),
                             rowwise_entropy_var = round(na.omit(rowwise_entropy_var), 3))
df_entropy_var


