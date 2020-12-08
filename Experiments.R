#### Naive Bayes ####
# install.packages("e1071")
# install.packages("naivebayes")

library(dplyr)
library(e1071)  # vieux package

# Chargement des features
load(file = "data/full_features.RData")


create_folds_data <- function(train, test, nfold){
  doc.id <- train$doc_id
  n.doc_id <- max(doc.id) %/% nfold
  set.train = list()
  set.test = list()
  for(i in 1:nfold){
    interval <- which(test$doc_id >= (1+(i-1)*n.doc_id) & test$doc_id <= (1+i*n.doc_id))
    set.train[[i]] <- train[-c(interval), ]
    set.test[[i]] <- test[interval, ]
  }
  return(list("train" = set.train, "test" = set.test))
}

nfold <- 5
data_set <- create_folds_data(features.full.svt.train.df, features.full.svt.test.df, nfold = nfold)

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
    
    label.init <- factor(f.test.label$label)
    label.pred <- factor(pred.label)
    
    if(length(levels(label.init))!= length(levels(label.pred))){
      u <- union(pred.label,f.test.label$label)
      tab_error <- table(factor(pred.label, u), factor(f.test.label$label, u))
    }else{
      tab_error <- table(pred.label, f.test.label$label)
    }
    
    tab_error.mat <- as.matrix(tab_error)
    # tab_error <- table(factor(pred.label, levels = levels(f.test.label$label)), f.test.label$label)
    tab_error <- table(pred.label,f.test.label$label)
    tab_error.mat <- as.matrix(tab_error)
    
    # calcul des indicateurs en multiclasse est la moyenne des indicateurs pour chaque classe
    P <- mean( diag(tab_error.mat) / colSums(tab_error.mat) )
    R <- mean( diag(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] / rowSums(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] )
    precision <- c(precision, P )
    recall <- c(recall, R )
    F1_score <- c(F1_score, (2 * (P * R) / (P + R)))
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

res1 <- naivebayes.cross_val(data_set, var_sel = c('word'))
res2 <- naivebayes.cross_val(data_set, var_sel = c('lemma'))
res3 <- naivebayes.cross_val(data_set, var_sel = c('word', 'pos'))
res4 <- naivebayes.cross_val(data_set, var_sel = c('word', 'svt'))
res5 <- naivebayes.cross_val(data_set, var_sel = c('word', 'WFP'))
res6 <- naivebayes.cross_val(data_set, var_sel = c('word', 'prefixes', 'suffixes'))
res7 <- naivebayes.cross_val(data_set, var_sel = c('word', 'word_previous', 'word_next'))
res8 <- naivebayes.cross_val(data_set, var_sel = c('word', 'word_previous', 'word_next', 'pos_previous', 'pos_next', 'pos'))
res9 <- naivebayes.cross_val(data_set, var_sel = c('word', 'pos', 'prefixes', 'suffixes'))
res10 <- naivebayes.cross_val(data_set, var_sel = c('word', 'pos', 'svt', 'WFP', 'prefixes', 'suffixes'))
res11 <- naivebayes.cross_val(data_set, var_sel = c('word', 'word_previous', 'word_next', 
                                                   'pos_previous', 'pos_next', 'pos',
                                                   'svt', 'svt_previous', 'svt_next',
                                                   'WFP', 'WFP_previous', 'WFP_next',
                                                   'prefixes', 'prefixes_previous', 'prefixes_next',
                                                   'suffixes', 'suffixes_previous', 'suffixes_next'))

  