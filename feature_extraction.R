# Extraction des features
# Les features sont sélectionnés:
# word, lemma, pos, svt, wfp, prefixes, suffixes

#### POS tagging avec NLP####

#telecharger le model en anglais
dl <- udpipe_download_model(language = "english")
# str(dl)

#Entrer le fichier qu'on a just téléchargé dans le directoire actuel
udmodel_english <- udpipe_load_model(file = "english-ud-2.0-170801.udpipe")
# udmodel_english <- udpipe_load_model(file = dl$file_model)

#Vu qu'on a 497.668 tokens, il prend bcp de temps a executer la fonction toute seule
#On va couper iob.word en 49 parties équales (length=10.000) et 1 partie (length=7894)
#Ensuite une boucle pour ajouter le resultat dans un dataframe
#Cette étape va prendre environ 4 heures à executer

## On a décidé d'exporter le résultat en fichier txt pour économiser le temps##

# Chargement des variables générées dans le pré-traitement (nécessaire pour la suite)
# iob.word <- read.table("old_var/iob_word.var",stringsAsFactors = FALSE)
# iob.label <- read.table("old_var/iob_label.var",stringsAsFactors = FALSE)
# doc_id <- read.table("old_var/doc_id.var",stringsAsFactors = FALSE)

load("data/word_label_id.RData")

#couper en paquets de 10000
seg_size <- 10000
nb_bloc <- nrow(doc_id)%/%seg_size
index_row <- split(1:(nb_bloc*seg_size), ceiling(seq_along(1:(nb_bloc*seg_size))/seg_size))
#la derniere partie est le reste
index_row[[as.character(nb_bloc+1)]] <- ((nb_bloc*seg_size)+1):nrow(doc_id)


#pos tagging pour 10.000 premiers tokens
pos1 <- as.data.frame(udpipe_annotate(udmodel_english,
                                      x = paste(iob.word$x[index_row$`1`],collapse = '\n'),
                                      tokenizer = "vertical"))

# ind pour observer les itérations
ind <- 0
for (i in index_row[-1]){
  #pos tagging de la partie suivante 
  pos <- as.data.frame(udpipe_annotate(udmodel_english,
                                       x = paste(iob.word$x[i],collapse = '\n'),
                                       tokenizer = "vertical"))
  #combiner pos1 et pos
  pos1 <- rbind(pos1,pos)
  
  ind <- ind + 1
  print(ind)
  print(nrow(pos1))
  # break
}
# Sauvegarder le fichier
# write.table(pos1, "data/POS_tag.txt")

####Charger le fichier de POS pour économiser le temps####
udpipe.data <- read.table("data/POS_tag.txt",header = TRUE,stringsAsFactors = FALSE)
features.df <- data.frame(word = udpipe.data$token,
                          lemma = udpipe.data$lemma, 
                          pos = udpipe.data$upos, 
                          label = iob.label, stringsAsFactors = FALSE)

####Special Verb Trigger####
special_verb_trigger <- function(data, win_size){
  # Description: fonction qui permet de récupérer une table ordonnée des verbes 
  #             les plus fréquents occurant au voisinage des entités nommées labélisées.
  # Input: 
      # data (data.frame): contient les lemma (forme lemmatisée des tokens), pos (part of speech) et label.
      # win_size (integer): taille de la fenêtre de voisinage du mot analysé.
  # Output:
      # table.verb (table): table ordonnée des special verb trigger.
  list.ind <- which(data$label != "O")
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  list.verb <- c()
  for(i in list.ind2){
    for(w in win){
      if( data$pos[i + w] == "VERB" && !is.na(data$pos[i + w] )){
        list.verb <- c(list.verb, as.character(data$lemma[i+w]))
      }
    }
  }
  
  table.verb <- sort(table(list.verb), decreasing = TRUE)
  return(table.verb)
}
spe.verb.trig <- special_verb_trigger(features.df, 1)

# On retient les 60 premiers verbes les plus fréquents
spe.verb.trig.60 <- head(spe.verb.trig, n = 60)

# Pour l'extraction des special verb trigger, on tient compte de la table de verbes obtenue, et des labels.
# Cette extraction est spécifique à l'entrainement, dans le jeu de test, on ne peut pas prendre en compte les labels (qui sont inconnus)

svt.feat.train.binary <- function(data, svt_table, win_size = 1){
  # Description: Pour l'extraction des special verb trigger, on tient compte de la table de verbes obtenue, et des labels.
  # Cette extraction est spécifique à l'entrainement, dans le jeu de test, on ne peut pas prendre en compte les labels (qui sont inconnus)
  
  # Input: 
    # data (data.frame): contient les lemma (forme lemmatisée des tokens), pos (part of speech) et label.
    # svt_table (table int): contient la table des special verb trigger
    # win_size (integer): taille de la fenêtre de voisinage du mot analysé.
  # Output:
    # svt (vecteur de string): vecteur des features de svt. TRUE si un special verb trigger a été trouvé au voisinage de l'entité nommée, FALSE sinon.
  
  
  list.ind <- which(data$label != "O")
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  svt <- logical(length(data$word))
  for(i in list.ind2){
    for(w in win){
      if( data$lemma[i + w] %in% names(svt_table) && data$label[i + w] == "O"){
        svt[i] <- TRUE
        break
      }
    }
  }
  return(svt)
}
svt.feat.train <- function(data, svt_table, win_size = 1){
  # Description: Pour l'extraction des special verb trigger, on tient compte de la table de verbes obtenue, et des labels.
  # Cette extraction est spécifique à l'entrainement, dans le jeu de test, on ne peut pas prendre en compte les labels (qui sont inconnus)
  
  # Input: 
    # data (data.frame): contient les lemma (forme lemmatisée des tokens), pos (part of speech) et label.
    # svt_table (table int): contient la table des special verb trigger
    # win_size (integer): taille de la fenêtre de voisinage du mot analysé.
  # Output:
    # svt (vecteur de string): vecteur des features de svt. <verb> si un special verb trigger a été trouvé au voisinage de l'entité nommée, "noverb" sinon.
  
  list.ind <- which(data$label != "O")
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  svt <- rep("noverb", length = length(data$word))
  for(i in list.ind2){
    for(w in win){
      if( data$lemma[i + w] %in% names(svt_table) && data$label[i + w] == "O"){
        svt[i] <- data$lemma[i + w]
        break
      }
    }
  }
  return(svt)
}
svt.feat.test.binary <- function(data, svt_table, win_size = 1){
  # Pour l'extraction des special verb trigger, on tient compte de la table de verbes obtenue.
  # Cette extraction est dédiée au test.
  list.ind <- list(length(data$lemma))
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  svt <- logical(length(data$word))
  for(i in list.ind2){
    for(w in win){
      if( data$lemma[i + w] %in% names(svt_table)){
        svt[i] <- TRUE
        break
      }
    }
  }
  return(svt)
}
svt.feat.test <- function(data, svt_table, win_size = 1){
  # Pour l'extraction des special verb trigger, on tient compte de la table de verbes obtenue.
  # Cette extraction est dédiée au test.
  list.ind <- list(length(data$lemma))
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  svt <- logical(length(data$word))
  for(i in list.ind2){
    for(w in win){
      if( data$lemma[i + w] %in% names(svt_table)){
        svt[i] <- data$lemma[i + w]
        break
      }
    }
  }
  return(svt)
}

# svt.feat <- svt.feat.train(features.df, spe.verb.trig.60)
svt.feat.train <- svt.feat.train.binary(features.df, spe.verb.trig.60)
# svt.feat <- svt.feat.test(features.df, spe.verb.trig.60)
svt.feat.test <- svt.feat.test.binary(features.df, spe.verb.trig.60)
# features.df <- dplyr::mutate(features.df, svt = svt.feat)
features.svt.train.df <- dplyr::mutate(features.df, svt = svt.feat.train)
features.svt.test.df <- dplyr::mutate(features.df, svt = svt.feat.test)

#### Word formation pattern####
#fonction attribuer code WFP
# wfp_tag <- function(word){
# t <- data.frame(word = a[1:300], label = b[1:300], stringsAsFactors = FALSE)

#liste des mots greecs
greek <- c("alpha","beta","gamma","delta","epsilon","zeta"
           ,"eta","theta","iota","kappa","lambda","mu","nu"
           ,"xi","omicron","pi","rho","sigma","tau","upsilon"
           ,"phi","chi","psi","omega")

#alphabet sans A, T, C , G
sequence <- LETTERS[! LETTERS %in% c("A","G","T","C")]

#creer un dataframe avec word/label IOB/tag WFP
#Train dataset
features.svt.train.df <- dplyr::mutate(
  # data.frame(word=c("ACGC",",",".","(","1,25","A","1","23525",
  #                   "II","0.31","The","Whereas","IgM",
  #                   "kDa","H2A","T4","6C2","19D","alpha"),stringsAsFactors = FALSE) ,
  features.svt.train.df,
  WFP =  
    dplyr::case_when(
      word  == ","  ~ "Comma",
      word  == "."  ~ "Dot",
      word  %in% c("(",")","[","]") ~ " Parenthesis",
      word  %in% 0:9 ~ "OneDigit",
      grepl("^[[:digit:]]+$", word) & nchar(word) > 1 ~ "AllDigits",
      word %in% LETTERS ~ "OneCap",
      tolower(word)  %in% tolower(stopwords(language = "en")) ~ "StopWord",
      !grepl(paste0(c(sequence,0:9),collapse = "|"),gsub("[[:punct:]]", "", toupper(word))) ~ "ATCGsequence",
      
      !is.na(as.roman(word)) & is.na(as.numeric(word)) ~ "RomanDigit",
      grepl("^[[:upper:]]+$", word) & nchar(word) > 1 ~ "AllCaps",
      
      tolower(word) %in% greek ~ "GreekLetter",
      grepl("^[[:digit:]]\\,.*[[:digit:]]$",word) ~ "DigitCommaDigit",
      grepl("^[[:digit:]]\\..*[[:digit:]]$",word) ~ "DigitDotDigit",
      
      grepl("^[[:upper:]].*[[:lower:]]$",word) ~ "CapLowAlpha",
      grepl("^[[:upper:]].*[[:lower:]].*[[:upper:]]$",word) ~ "CapMixAlpha",
      grepl("^[[:lower:]].*[[:upper:]].*[[:lower:]]$",word) ~ "LowMixAlpha",
      grepl("^[[:upper:]].*[[:digit:]].*[[:upper:]]$",word) ~ "AlphaDigitAlpha",
      grepl("^[[:upper:]].*[[:digit:]]$",word) ~ "AlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]].*[[:digit:]]$",word) ~ "DigitAlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]|[:lower:]]$",word) ~ "DigitAlpha",
      
      TRUE                     ~ "Others"
    ))

#Test dataset
features.svt.test.df <- dplyr::mutate(
  # data.frame(word=c("ACGC",",",".","(","1,25","A","1","23525",
  #                   "II","0.31","The","Whereas","IgM",
  #                   "kDa","H2A","T4","6C2","19D","alpha"),stringsAsFactors = FALSE) ,
  features.svt.test.df,
  WFP =  
    dplyr::case_when(
      word  == ","  ~ "Comma",
      word  == "."  ~ "Dot",
      word  %in% c("(",")","[","]") ~ " Parenthesis",
      word  %in% 0:9 ~ "OneDigit",
      grepl("^[[:digit:]]+$", word) & nchar(word) > 1 ~ "AllDigits",
      word %in% LETTERS ~ "OneCap",
      tolower(word)  %in% tolower(stopwords(language = "en")) ~ "StopWord",
      !grepl(paste0(c(sequence,0:9),collapse = "|"),gsub("[[:punct:]]", "", toupper(word))) ~ "ATCGsequence",
      
      !is.na(as.roman(word)) & is.na(as.numeric(word)) ~ "RomanDigit",
      grepl("^[[:upper:]]+$", word) & nchar(word) > 1 ~ "AllCaps",
      
      tolower(word) %in% greek ~ "GreekLetter",
      grepl("^[[:digit:]]\\,.*[[:digit:]]$",word) ~ "DigitCommaDigit",
      grepl("^[[:digit:]]\\..*[[:digit:]]$",word) ~ "DigitDotDigit",
      
      grepl("^[[:upper:]].*[[:lower:]]$",word) ~ "CapLowAlpha",
      grepl("^[[:upper:]].*[[:lower:]].*[[:upper:]]$",word) ~ "CapMixAlpha",
      grepl("^[[:lower:]].*[[:upper:]].*[[:lower:]]$",word) ~ "LowMixAlpha",
      grepl("^[[:upper:]].*[[:digit:]].*[[:upper:]]$",word) ~ "AlphaDigitAlpha",
      grepl("^[[:upper:]].*[[:digit:]]$",word) ~ "AlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]].*[[:digit:]]$",word) ~ "DigitAlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]|[:lower:]]$",word) ~ "DigitAlpha",
      
      TRUE                     ~ "Others"
    ))

#les 100eres lignes de data_wfp
head(features.svt.train.df, n = 10)


#### Affixes ####

####Construire dataframe de affix base de label
#input
#    a: tous les token
#    b: labell de token
#output
#   un vector de suffix qui est efficacite
construt_suffix<-function(a,b){
  df_af <- data.frame(word=a,label=str_replace_all(b,"[BI-]",""),stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix
  suf5 <- ifelse(nchar(df_af$word)>=5 ,str_sub(df_af$word, -5),'non_non')
  ####Construire dataframe suffix(5 grams) avec weight####
  df_suf<-data.frame(suf5,df_af$label,stringsAsFactors = FALSE)
  #enlever le mot moins 5 caractere et suffix contient '-'
  df_suf<-df_suf[df_suf$suf5 != "non_non" & is.na(str_extract(df_suf$suf5, "-"))==TRUE,]
  #compte frequence de suffix
  F_suf<-as.data.frame(table(df_suf))
  #mettre ordre descendre
  F_suf<-F_suf[order(-F_suf$Freq),]
  #chercher 100 plus frequence suffix qui est entitee
  F_suf_100<-F_suf[which(F_suf$df_af.label!="O"),]
  suf_100<-unique(F_suf_100$suf5)[1:100]
  #calculer weight par formule (in[]-out[])/(in[]+out[])
  weight_suf<-rep(0, 100)
  j<-1
  for(i in suf_100){
    nb_in<-sum(F_suf$Freq[which(F_suf$suf5==i & F_suf$df_af.label!="O")])
    nb_out<-sum(F_suf$Freq[which(F_suf$suf5==i & F_suf$df_af.label=="O")])
    weight_suf[j]<- (nb_in - nb_out)/(nb_in + nb_out)
    j<-j+1
  }
  df_weight_suf<-data.frame(suf_100,weight_suf)
  #on prend weight superieur 0.5
  df_weight_suf<-df_weight_suf[weight_suf>=0.7,]
  #construire comme dictionary de labels
  return(df_weight_suf[,1])
}

####Construire dataframe de préfixes de base des label
#input
#    a: tous les token
#    b: label de token
#output
#   un vector de prefix qui est efficacite 
construt_prefix<-function(a,b){
  df_af <- data.frame(word=a,label=str_replace_all(b,"[BI-]",""),stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix
  pre4 <- ifelse(nchar(df_af$word)>=5 ,str_sub(df_af$word, 1,4),'non_non')
  ####Construire dataframe prefix(4 grams) avec weight####
  df_pref<-data.frame(pre4,df_af$label,stringsAsFactors = FALSE)
  df_pref<-df_pref[df_pref$pre4 != "non_non" & is.na(str_extract(df_pref$pre4, "-"))==TRUE,]
  F_pref<-as.data.frame(table(df_pref))#compte frequence de suffix
  F_pref<-F_pref[order(-F_pref$Freq),]
  F_pref_100<-F_pref[which(F_pref$df_af.label!="O"),]
  pref_100<-unique(F_pref_100$pre4)[1:100]
  weight_pref<-rep(0, 100)
  j<-1
  for(i in pref_100){
    nb_in<-sum(F_pref$Freq[which(F_pref$pre4==i & F_pref$df_af.label!="O")])
    nb_out<-sum(F_pref$Freq[which(F_pref$pre4==i & F_pref$df_af.label=="O")])
    weight_pref[j]<- (nb_in - nb_out)/(nb_in + nb_out)
    j<-j+1
  }
  df_weight_pref<-data.frame(pref_100,weight_pref)
  df_weight_pref<-df_weight_pref[weight_pref>=0.7,]
  return(df_weight_pref[,1])
}

install.packages("hash")
library(hash)
###find affix par word###
#input
#    words:word token
#    pref:un vector de prefix
#    suf:un vector de suffix
#output
#    un dataframe 3 colonne (word,prefix,suffix)
#    si le prefix/suffix de word est dans le vector prefix/suffix, on mets TRUE dans cette colonne
#    sion FALSE
extract_affixes<-function(words, pref, suf){
  require(hash)
  h<-hash()
  uniq_word<-unique(words)
  for(w in uniq_word){
    if(nchar(w)<5) .set(h, keys = w, values = rep(FALSE,2))
    else{
      w_suf<-w %>%
        str_sub(-5)
      w_pref<-w %>%
        str_sub(1,4)
      
      if(length(grep(w_suf, suf))==0) wsuf <- FALSE
      else wsuf <- TRUE#suf[grep(w_suf,suf[,1]),2]
      if(length(grep(w_pref, pref))==0) wpref<-FALSE
      else wpref<-TRUE#suf[grep(w_pref,pref[,1]),2]
      .set(h, keys = w, values = c(as.character(wpref),as.character(wsuf)))
    }
  }
  df_af<-foreach(w=words,.combine = "rbind")%do% c(w,values(h, w))
  
  colnames(df_af)<-c("word","prefix","suffix")
  return(df_af)
}

#Creer les affixes
# suffixes.dict <- construct_suffix(features.df$word, features.df$label)
# prefixes.dict <- construct_prefix(features.df$word, features.df$label)
# affixes.df <- extract_affixes(features.df$word, prefixes.dict, suffixes.dict)

#Exporter les affixes pour economiser le temps d executer
# write.table(affixes.df, "data/affixes.var")
#Charger les affixes
affixes.df <- read.table("data/affixes.var", stringsAsFactors = FALSE)


#### Coller les features dans un dataframe ####
features.svt.train.df <- dplyr::mutate(doc_id = doc_id$x,
                                       features.svt.train.df, 
                                       prefixes = affixes.df$prefix,
                                       suffixes = affixes.df$suffix)
                                       

features.svt.test.df <- dplyr::mutate(doc_id = doc_id$x,
                                      features.svt.test.df, 
                                      prefixes = affixes.df$prefix,
                                      suffixes = affixes.df$suffix)
# Enregistrer sous forme RData
# save(features.svt.train.df,features.svt.test.df,file = "data/full_features_wo_win.RData")

#### Features fenêtre symétrique w[-1], w[0], w[1] ####
library(data.table)

  ## dataset avec SVT = {TRUE,FALSE} ####
features.full.svt.train.df <- as.data.table(features.svt.train.df)

#pos before
features.full.svt.train.df <- features.full.svt.train.df[, pos_previous   := shift(pos, n = 1, type = "lag"), by = list(doc_id)]
#pos next
features.full.svt.train.df <- features.full.svt.train.df[, pos_next       := shift(pos, n = 1, type = "lead"), by = list(doc_id)]

#word previous
features.full.svt.train.df <- features.full.svt.train.df[, word_previous := shift(word, n = 1, type = "lag"), by = list(doc_id)]
#word next
features.full.svt.train.df <- features.full.svt.train.df[, word_next     := shift(word, n = 1, type = "lead"), by = list(doc_id)]

#lemma before
features.full.svt.train.df <- features.full.svt.train.df[, lemma_previous   := shift(lemma, n = 1, type = "lag"), by = list(doc_id)]
#lemma next
features.full.svt.train.df <- features.full.svt.train.df[, lemma_next       := shift(lemma, n = 1, type = "lead"), by = list(doc_id)]

#svt previous
features.full.svt.train.df <- features.full.svt.train.df[, svt_previous := shift(svt, n = 1, type = "lag"), by = list(doc_id)]
#svt next
features.full.svt.train.df <- features.full.svt.train.df[, svt_next     := shift(svt, n = 1, type = "lead"), by = list(doc_id)]

#wfp previous
features.full.svt.train.df <- features.full.svt.train.df[, WFP_previous := shift(WFP, n = 1, type = "lag"), by = list(doc_id)]
#wfp next
features.full.svt.train.df <- features.full.svt.train.df[, WFP_next     := shift(WFP, n = 1, type = "lead"), by = list(doc_id)]

#prefixes previous
features.full.svt.train.df <- features.full.svt.train.df[, prefixes_previous := shift(prefixes, n = 1, type = "lag"), by = list(doc_id)]
#prefixes next
features.full.svt.train.df <- features.full.svt.train.df[, prefixes_next     := shift(prefixes, n = 1, type = "lead"), by = list(doc_id)]

#suffixes previous
features.full.svt.train.df <- features.full.svt.train.df[, suffixes_previous := shift(suffixes, n = 1, type = "lag"), by = list(doc_id)]
#suffixes next
features.full.svt.train.df <- features.full.svt.train.df[, suffixes_next     := shift(suffixes, n = 1, type = "lead"), by = list(doc_id)]

  ## dataset avec SVT = {1,0} ####
features.full.svt.test.df <- as.data.table(features.svt.test.df)

#pos before
features.full.svt.test.df <- features.full.svt.test.df[, pos_previous   := shift(pos, n = 1, type = "lag"), by = list(doc_id)]
#pos next
features.full.svt.test.df <- features.full.svt.test.df[, pos_next       := shift(pos, n = 1, type = "lead"), by = list(doc_id)]

#word previous
features.full.svt.test.df <- features.full.svt.test.df[, word_previous := shift(word, n = 1, type = "lag"), by = list(doc_id)]
#word next
features.full.svt.test.df <- features.full.svt.test.df[, word_next     := shift(word, n = 1, type = "lead"), by = list(doc_id)]

#lemma before
features.full.svt.test.df <- features.full.svt.test.df[, lemma_previous   := shift(lemma, n = 1, type = "lag"), by = list(doc_id)]
#lemma next
features.full.svt.test.df <- features.full.svt.test.df[, lemma_next       := shift(lemma, n = 1, type = "lead"), by = list(doc_id)]

#svt previous
features.full.svt.test.df <- features.full.svt.test.df[, svt_previous := shift(svt, n = 1, type = "lag"), by = list(doc_id)]
#svt next
features.full.svt.test.df <- features.full.svt.test.df[, svt_next     := shift(svt, n = 1, type = "lead"), by = list(doc_id)]

#wfp previous
features.full.svt.test.df <- features.full.svt.test.df[, WFP_previous := shift(WFP, n = 1, type = "lag"), by = list(doc_id)]
#wfp next
features.full.svt.test.df <- features.full.svt.test.df[, WFP_next     := shift(WFP, n = 1, type = "lead"), by = list(doc_id)]

#prefixes previous
features.full.svt.test.df <- features.full.svt.test.df[, prefixes_previous := shift(prefixes, n = 1, type = "lag"), by = list(doc_id)]
#prefixes next
features.full.svt.test.df <- features.full.svt.test.df[, prefixes_next     := shift(prefixes, n = 1, type = "lead"), by = list(doc_id)]

#suffixes previous
features.full.svt.test.df <- features.full.svt.test.df[, suffixes_previous := shift(suffixes, n = 1, type = "lag"), by = list(doc_id)]
#suffixes next
features.full.svt.test.df <- features.full.svt.test.df[, suffixes_next     := shift(suffixes, n = 1, type = "lead"), by = list(doc_id)]

#### Enregistrer sous forme RData ####

# save(features.full.svt.train.df,features.full.svt.test.df,file = "data/full_features.RData")



