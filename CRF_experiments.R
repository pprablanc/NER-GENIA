#### Conditional Random Field ####

# install.packages("crfsuite")
library(crfsuite)
library(data.table)

load("data/full_features.RData")

##  dataset SVT = {TRUE , FALSE}

#pos_previous with defaut : pos[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, pos_previous:= txt_sprintf("pos[w-1]=%s", pos_previous), by = list(doc_id)]
#pos_next with defaut : pos[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, pos_next       := txt_sprintf("pos[w+1]=%s", pos_next), by = list(doc_id)]

#word_previous ; word[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, word_previous := txt_sprintf("word[w-1]=%s", word_previous), by = list(doc_id)]
#word_next : word[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, word_next     := txt_sprintf("word[w-1]=%s", word_next), by = list(doc_id)]

#lemma_previous with defaut : lemma[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, lemma_previous:= txt_sprintf("lemma[w-1]=%s", lemma_previous), by = list(doc_id)]
#lemma_next with defaut : lemma[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, lemma_next       := txt_sprintf("lemma[w+1]=%s", lemma_next), by = list(doc_id)]

#converse vers character
features.full.svt.train.df$svt_previous <- as.character(features.full.svt.train.df$svt_previous)
features.full.svt.train.df$svt_next <- as.character(features.full.svt.train.df$svt_next)
features.full.svt.train.df$svt <- as.character(features.full.svt.train.df$svt)
#svt_previous ; svt[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, svt_previous := txt_sprintf("svt[w-1]=%s", svt_previous), by = list(doc_id)]
#svt_next : svt[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, svt_next     := txt_sprintf("svt[w-1]=%s", svt_next), by = list(doc_id)]

#WFP_previous ; WFP[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, WFP_previous := txt_sprintf("WFP[w-1]=%s", WFP_previous), by = list(doc_id)]
#WFP_next : WFP[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, WFP_next     := txt_sprintf("WFP[w-1]=%s", WFP_next), by = list(doc_id)]

#converse vers character
features.full.svt.train.df$suffixes_previous <- as.character(features.full.svt.train.df$suffixes_previous)
features.full.svt.train.df$suffixes_next <- as.character(features.full.svt.train.df$suffixes_next)
features.full.svt.train.df$suffixes <- as.character(features.full.svt.train.df$suffixes)
#suffixes_previous ; suffixes[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, suffixes_previous := txt_sprintf("suffixes[w-1]=%s", suffixes_previous), by = list(doc_id)]
#suffixes_next : suffixes[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, suffixes_next     := txt_sprintf("suffixes[w-1]=%s", suffixes_next), by = list(doc_id)]

#converse vers character
features.full.svt.train.df$prefixes_previous <- as.character(features.full.svt.train.df$prefixes_previous)
features.full.svt.train.df$prefixes_next <- as.character(features.full.svt.train.df$prefixes_next)
features.full.svt.train.df$prefixes <- as.character(features.full.svt.train.df$prefixes)
#prefixes_previous ; prefixes[w-1]
features.full.svt.train.df <- features.full.svt.train.df[, prefixes_previous := txt_sprintf("prefixes[w-1]=%s", prefixes_previous), by = list(doc_id)]
#prefixes_next : prefixes[w+1]
features.full.svt.train.df <- features.full.svt.train.df[, prefixes_next     := txt_sprintf("prefixes[w-1]=%s", prefixes_next), by = list(doc_id)]


##  dataset SVT = {0,1}

#pos_previous with defaut : pos[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, pos_previous:= txt_sprintf("pos[w-1]=%s", pos_previous), by = list(doc_id)]
#pos_next with defaut : pos[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, pos_next       := txt_sprintf("pos[w+1]=%s", pos_next), by = list(doc_id)]

#word_previous ; word[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, word_previous := txt_sprintf("word[w-1]=%s", word_previous), by = list(doc_id)]
#word_next : word[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, word_next     := txt_sprintf("word[w-1]=%s", word_next), by = list(doc_id)]

#lemma_previous with defaut : lemma[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, lemma_previous:= txt_sprintf("lemma[w-1]=%s", lemma_previous), by = list(doc_id)]
#lemma_next with defaut : lemma[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, lemma_next       := txt_sprintf("lemma[w+1]=%s", lemma_next), by = list(doc_id)]

#converse vers character
features.full.svt.test.df$svt_previous <- as.character(features.full.svt.test.df$svt_previous)
features.full.svt.test.df$svt_next <- as.character(features.full.svt.test.df$svt_next)
features.full.svt.test.df$svt <- as.character(features.full.svt.test.df$svt)
#svt_previous ; svt[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, svt_previous := txt_sprintf("svt[w-1]=%s", svt_previous), by = list(doc_id)]
#svt_next : svt[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, svt_next     := txt_sprintf("svt[w-1]=%s", svt_next), by = list(doc_id)]

#WFP_previous ; WFP[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, WFP_previous := txt_sprintf("WFP[w-1]=%s", WFP_previous), by = list(doc_id)]
#WFP_next : WFP[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, WFP_next     := txt_sprintf("WFP[w-1]=%s", WFP_next), by = list(doc_id)]

#converse vers character
features.full.svt.test.df$suffixes_previous <- as.character(features.full.svt.test.df$suffixes_previous)
features.full.svt.test.df$suffixes_next <- as.character(features.full.svt.test.df$suffixes_next)
features.full.svt.test.df$suffixes <- as.character(features.full.svt.test.df$suffixes)
#suffixes_previous ; suffixes[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, suffixes_previous := txt_sprintf("suffixes[w-1]=%s", suffixes_previous), by = list(doc_id)]
#suffixes_next : suffixes[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, suffixes_next     := txt_sprintf("suffixes[w-1]=%s", suffixes_next), by = list(doc_id)]

#converse vers character
features.full.svt.test.df$prefixes_previous <- as.character(features.full.svt.test.df$prefixes_previous)
features.full.svt.test.df$prefixes_next <- as.character(features.full.svt.test.df$prefixes_next)
features.full.svt.test.df$prefixes <- as.character(features.full.svt.test.df$prefixes)
#prefixes_previous ; prefixes[w-1]
features.full.svt.test.df <- features.full.svt.test.df[, prefixes_previous := txt_sprintf("prefixes[w-1]=%s", prefixes_previous), by = list(doc_id)]
#prefixes_next : prefixes[w+1]
features.full.svt.test.df <- features.full.svt.test.df[, prefixes_next     := txt_sprintf("prefixes[w-1]=%s", prefixes_next), by = list(doc_id)]

subset(features.full.svt.train.df, doc_id == 100, select = c("doc_id", "word", "svt_previous", "word_next"))

features.full.svt.train.df <- as.data.frame(features.full.svt.train.df)
features.full.svt.test.df <- as.data.frame(features.full.svt.test.df)

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


# Training + test cross-validation
crf.cross_val <- function(dataset, var_sel){
  precision <- numeric()
  recall <- numeric()
  F1_score <- numeric()
  for(i in 1:nfold){
    f.train <- select(dataset$train[[i]], var_sel, label, doc_id)
    f.test <- select( dataset$test[[i]], var_sel,doc_id)
    f.test.label <- select( dataset$test[[i]], label)
    
    model <- crf(y = f.train$label,
                 x = subset( f.train, select = -c(label,doc_id) ),
                 group = f.train$doc_id,
                 method = "lbfgs", 
                 file  = "tagger.crfsuite",
                 options = list(max_iterations = 30,
                                feature.minfreq = 5,
                                c1 = 0,
                                c2 = 1)
    )
    scores <- predict(model, 
                      newdata = subset( f.test, select = -doc_id),
                      group = f.test$doc_id)
    
    label.init <- factor(f.test.label$label)
    label.pred <- factor(scores$label)
    
    if(length(levels(label.init))!= length(levels(label.pred))){
      u <- union(scores$label,f.test.label$label)
      tab_error <- table(factor(scores$label, u), factor(f.test.label$label, u))
    }else{
      tab_error <- table(scores$label, f.test.label$label)
    }
    
    tab_error.mat <- as.matrix(tab_error)
    
    # calcul des indicateurs en multiclasse est la moyenne des indicateurs pour chaque classe
    P <- mean( diag(tab_error.mat) / colSums(tab_error.mat) )
    R <- mean( diag(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] / rowSums(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] )
    precision <- c(precision, P )
    recall <- c(recall, R )
    F1_score <- c(F1_score, 2 * (P * R) / (P + R) )
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

res1_crf <- crf.cross_val(data_set, var_sel = c('word', 'pos'))
res2_crf <- crf.cross_val(data_set, var_sel = c('word', 'word_previous', 'word_next', 'pos_previous', 'pos_next', 'pos'))
res3_crf <- crf.cross_val(data_set, var_sel = c('word', 'word_previous', 'word_next', 'WFP', 'WFP_next','WFP_previous'))
res4_crf <- crf.cross_val(data_set, var_sel = c('word', 'svt'))
res5_crf <- crf.cross_val(data_set, var_sel = c('word', 'pos', 'prefixes', 'suffixes'))
res6_crf <- crf.cross_val(data_set, var_sel = c('word', 'prefixes', 'suffixes'))
