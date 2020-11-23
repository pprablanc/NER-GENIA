#### Naive Bayes ####

library(e1071)  # vieux package
library(naivebayes) # package utilisé par "caret"

# Chargement des features
features.full.svt.train.df <- read.table("features.full.svt.train.var")
features.full.svt.test.df <- read.table("features.full.svt.test.var")


create_folds_data <- function(train, test, nfold){
  doc_id <- train$doc_id
  n.doc_id <- max(train$doc_id) %/% nfold
  set.train = list()
  set.test = list()
  for(i in 1:nfold){
    interval <- which(features.full.svt.test.df$doc_id >= (1+(i-1)*n.doc_id) & features.full.svt.test.df$doc_id <= (1+i*n.doc_id))
    set.train[[i]] <- train[-c(interval), ]
    set.test[[i]] <- test[interval, ]
  }
  return(list("train" = set.train, "test" = set.test))
}

nfold = 5
dataset <- create_folds_data(features.full.svt.train.df, features.full.svt.test.df, nfold = nfold)

# Liste des features:
# word, lemma, pos, svt, wfp, prefixes, suffixes

# Training + test cross-validation
naivebayes.cross_val <- function(dataset, var_sel){
  precision <- numeric()
  recall <- numeric()
  F1_score <- numeric()
  for(i in 1:nfold){
    f.train <- select( dataset$train[[i]], var_sel, label)
    f.test <- select( dataset$test[[i]], var_sel)
    f.test.label <- select( dataset$test[[i]], label)
    
    model <- naiveBayes(label ~ ., data = f.train, laplace = 1)
    pred.raw <- predict(model, f.test, type = "raw")
    
    pred.label <-apply(pred.raw, 1, function(x) names(x[which.max(x)]))
    tab_error <- table(factor(pred.label, levels = levels(f.test.label$label)), f.test.label$label)
    
    tab_error.mat <- as.matrix(tab_error)
    
    # calcul des indicateurs en multiclasse est la moyenne des indicateurs pour chaque classe
    precision <- c(precision, mean( diag(tab_error.mat) / colSums(tab_error.mat) ) )
    recall <- c(recall, mean( diag(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] / rowSums(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] ) )
    F1_score <- c(F1_score, 2 * (precision * recall) / (precision + recall) )
  }
  precision.global <- mean(precision)
  recall.global <- mean(recall)
  F1_score.global <- mean(F1_score)
  print(paste('précision: ',precision.global))
  print(paste('recall: ',recall.global))
  print(paste('F1-score: ',F1_score.global))
  return( list(precision = precision, recall = recall, F1_score = F1_score, 
               precision.global = precision.global, recall.global = recall.global, F1_score.global)
          )
}
res1 <- naivebayes.cross_val(dataset, var_sel = c('word', 'pos'))
res2 <- naivebayes.cross_val(dataset, var_sel = c('word', 'word_prev', 'word_next', 'pos_prev', 'pos_next', 'pos'))
res3 <- naivebayes.cross_val(dataset, var_sel = c('word', 'WFP'))
res4 <- naivebayes.cross_val(dataset, var_sel = c('word', 'svt'))
res5 <- naivebayes.cross_val(dataset, var_sel = c('word', 'pos', 'prefixes', 'suffixes'))
res6 <- naivebayes.cross_val(dataset, var_sel = c('word', 'prefixes', 'suffixes'))
