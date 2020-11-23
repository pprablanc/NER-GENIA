###Hidden Markov Model###
#Notre l'état caché 
labels<-c("B-CLIEN","B-CTYPE","B-DNA","B-PROTEIN","B-RNA","I-CLIEN","I-CTYPE","I-DNA","I-PROTEIN","I-RNA","O" )

#construction HMM
initmyHMM<-function(data_ent){
  #######construire la matrice transProbs IOB#######
  #compte la frequence de label dans le jeu entrainement
  labels_num <- table(data_ent$label)
    
  #enlever le nombre de word "," et "."
  #car ces sont ponctuation comme bruit
  labels_num[11] <- labels_num[11]- nrow(data_ent[which(data_ent$word==","),])-nrow(data_ent[which(data_ent$word=="."),])
    
  #matrice ligne et col sont label，label.transform[i,j] est la frequence de label i transfere a j
  label.transform<-matrix(rep(0,11*11),nrow = 11,dimnames = list( names(labels_num), names(labels_num)))#initialise matrice
    
  #compte la frequece de labels suivre
  for(i in 1:nrow(data_ent)){
    if(data_ent$word[i]=='.'||data_ent$word[i]==',') next
    label.transform[data_ent$label[i],data_ent$label[i+1]]<-label.transform[data_ent$label[i],data_ent$label[i+1]]+1
  }
    
  #label.tp est le matrice de probabilite transfere
  label.tp<-label.transform
  for(i in 1:11){#11 est le nombre de different label
    for(j in 1:11){
      label.tp[i,j]<-label.tp[i,j]/labels_num[[i]]
    }
  }
  
  #######construire la matrice start PI IOB#######
  startp<-labels_num
  for (i in 1:11) {
    startp[i]<-0
  }
  sumstart<-0
  for (i in 1:nrow(data_ent)) {
    if(i==1){
      startp[data_ent$label[i]]<-1
      sumstart<-1        
      next
    }
    if(i==nrow(data_ent)){
      break
    }
    if(data_ent$word[i]=="."||data_ent$word[i]==","){
      startp[data_ent$label[i+1]]<-startp[data_ent$label[i+1]]+1
      sumstart<-sumstart+1
    }
  }
  for (i in 1:11) {
    startp[i]<-startp[i]/sumstart
  }
    
  return(list(label.tp=label.tp,startp=startp))
}

# Chargement des features
features.full.svt.train.df <- read.table("features_svt_train.var")
features.full.svt.test.df <- read.table("features_svt_test.var")

#######construire la matrice emissionProbs IOB#######
  #on utilise tous les data calculer
  #car il y a de word existe dans data_test, non existe dans data_ent
  #enlever le feature lemma,label et doc_id
  data_all_ts<-features.full.svt.train.df[,c(-2,-4,-9)] %>% as.matrix()
  #                              word          pos      svt        WFP         prefix     suffix
  #mettre les feature comme "c(\"reactive\", \"ADJ\", \"FALSE\", \"Others\", \"FALSE\", \"FALSE\")"
  #ici pour tranfere tous les donnees, il passe presque 1 heure
  #
  observations<-c()
  for (i in 1:nrow(data_all_ts)) {
    observations<-c(observations,list(as.vector(data_all_ts[i,])))
  }
  
  #la ligne est label, colonne est different word avec features
  all_labels_num <- table(features.full.svt.train.df$label)
  epmatrix<-table(data_all$label,as.character(observations))
  for (i in 1:11) {
    for (j in 1:length(unique(observations))) {
      epmatrix[i,j]<-epmatrix[i,j]/all_labels_num[[i]]
    }
  }
  
  #######Algorithme viterbi#######
  ###input 
  ###     tp:matrice A(transProbs)
  ###     ep:matrice B(émissionProbs)
  ###     pi:matrice startProbs
  ###     test:data test 
  ###     labels:nom d'état caché 
  ###output 
  ###     outp:outp[i,j] est la probabilité de mot i sachant que label j
  ###     res:la séquence de d'état caché correspond à donnée tester
  myViterbi<-function(tp,ep,pi,test,labels){
    #initialise outp, ligne est word,colonne est label
    outp<-array(0,dim = c(length(test),11))
    #fonction itération premier jusqu'à la fin
    for (k in 1:length(test)){
      #on a 11 label
      for(v in 1:11){
        #si c'est premier mot dans la phrase 
        if(k==1||test[k-1]=="."||test[k-1]==","){
          outp[k,v]<-pi[v]*ep[v,test[k]]
        }
        else{
          #sinon calcule la probabilité à temp t-1
          for (u in 1:11) {
            maxvalue<-outp[k-1,u]*tp[u,v]*ep[v,test[k]]
            if(maxvalue>outp[k,v]) outp[k,v]<-maxvalue
          }
        }
      }
    }
    colnames(outp)<-labels
    #retourner le plus grand de probabilité à chaque ligne 
    res<-apply(outp, 1, function(t) colnames(outp)[which.max(t)])
    #return(list(res=res,outp=outp))
    return(res)
  }
  
  create_folds_data <- function(train, test, nfold){
  doc_id <- train$doc_id
  n.doc_id <- max(train$doc_id) %/% nfold
  set.train = list()
  set.test = list()
  
  for(i in 1:nfold){
    interval <- which(features.full.svt.test.df$doc_id >= (1+(i-1)*n.doc_id) &features.full.svt.test.df$doc_id <= (1+i*n.doc_id))
    set.train[[i]] <- train[-c(interval), ]
    set.test[[i]] <- test[interval, ]
  }
  return(list("train" = set.train, "test" = set.test))
}

nfold = 5
dataset <- create_folds_data(features.full.svt.train.df, features.full.svt.test.df, nfold = nfold)
precision <- numeric()
recall <- numeric()
F1_score <- numeric()

#du coup il passe longtemp pour tester croisement 
#je fais tester 1 flod
i<-1
#for(i in 1:nfold){
  f.train <- dataset$train[[i]][,c(-2,-9)]
  f.test <-  dataset$test[[i]][,c(-2,-4,-9)]
  f.test.label <- select( dataset$test[[i]], label)
  
  myparam<-initmyHMM(f.train)
  #ici j'ai un bug bizzare, je ne predire que 100 mots, apres 100 mot,il ne rendre pas resultat,donc je fais un boucle par 100 mots
  tmp<-rownames(f.test) %>% as.numeric()
  begin<-tmp[1]
  res<-c()
  while(begin<=tmp[length(tmp)]){
    if((begin+99)<length(tmp)) end<-begin+99
    else end<-length(tmp)
    resultat<-myViterbi(myparam$label.tp,epmatrix,myparam$startp,as.character(observations[begin:end]),labels)
    res<-c(res,resultat)
    begin<-end+1
  }
  tab_error <- table(factor(res, levels = labels), f.test.label$label)
  tab_error.mat <- as.matrix(tab_error)
  
  # calcul des indicateurs en multiclasse est la moyenne des indicateurs pour chaque classe
  precision <- c(precision, mean( diag(tab_error.mat) / colSums(tab_error.mat) ) )
  recall <- c(recall, mean( diag(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] / rowSums(tab_error.mat)[which(rowSums(tab_error.mat) > 0)] ) )
  F1_score <- c(F1_score, 2 * (precision * recall) / (precision + recall) )
#}

#precision.global <- mean(precision)
#recall.global <- mean(recall)
#F1_score.global <- mean(F1_score)
#print(paste('précision: ',precision.global))
#print(paste('recall: ',recall.global))
#print(paste('F1-score: ',F1_score.global))

print(paste('précision: ',precision))
print(paste('recall: ',recall))
print(paste('F1-score: ',F1_score))
