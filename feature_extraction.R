# Extraction des features

#### POS tagging avec NLP####

#telecharger le model en anglais
dl <- udpipe_download_model(language = "english")
# str(dl)

#Entrer le fichier qu'on a just téléchargé dans le directoir actuel
udmodel_english <- udpipe_load_model(file = "english-ud-2.0-170801.udpipe")
# udmodel_english <- udpipe_load_model(file = dl$file_model)

#Vu qu'on a 497.668 tokens, il prend bcp de temps a executer la fonction toute seule
#On va couper iob.word en 49 parties équales (length=10.000) et 1 partie (length=7668)
#Ensuite une boucle pour ajouter le resultat dans un dataframe
#Cette étape va prendre environ 4 heures à executer

#### On a décidé d'exporter le résultat en fichier txt pour économiser le temps####

# Chargement des variables générées dans le pré-traitement (nécessaire pour la suite)
iob.word <- read.table("iob_word.var")
iob.label <- read.table("iob_label.var")
doc_id <- read.table("doc_id.var")


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

#ind pour observer les itérations
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
write.table(pos1, "POS_tag.txt")

####Charger le fichier de POS pour économiser le temps####
udpipe.data <- read.table("POS_tag.txt",header = TRUE,stringsAsFactors = FALSE)
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

#dataframe avec tokens, lemma, pos tags, iob tags




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


construct_suffix<-function(a, b){
  ####Construire dataframe de suffixes de base des label
  #input
      #    a: tous les token
      #    b: label de token
  #output
      #   dataframe "suffix"  "lablel"
  
  df_af <- data.frame(word = a, label = str_replace_all(b,"[BI-]",""), stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix 
  suf5 <- ifelse(nchar(df_af$word) >= 5, str_sub(df_af$word, -5), 'non_non')
  ####Construire dataframe suffix(5 grams) avec weight####
  df_suf <- data.frame(suf5, df_af$label, stringsAsFactors = FALSE)
  #enlever le mot moins 5 caractere et suffix contient '-'
  df_suf <- df_suf[df_suf$suf5 != "non_non" & is.na(str_extract(df_suf$suf5, "-")) == TRUE, ]
  #compte frequence de suffix
  F_suf <- as.data.frame(table(df_suf))
  #mettre ordre descendre
  F_suf <- F_suf[order(-F_suf$Freq), ]
  #chercher 100 plus frequence suffix qui est entitee
  F_suf_100 <- F_suf[which(F_suf$df_af.label!="O"), ]
  suf_100 <- unique(F_suf_100$suf5)[1:100]
  #calculer weight par formule (in[]-out[])/(in[]+out[])
  weight_suf <- rep(0, 100)
  j<-1
  for(i in suf_100){
    nb_in <- sum(F_suf$Freq[which(F_suf$suf5 == i & F_suf$df_af.label != "O")])
    nb_out <- sum(F_suf$Freq[which(F_suf$suf5 == i & F_suf$df_af.label == "O")])
    weight_suf[j] <- (nb_in - nb_out)/(nb_in + nb_out)
    j <- j+1
  }
  df_weight_suf <- data.frame(suf_100, weight_suf)
  #on prend weight superieur 0.7
  df_weight_suf <- df_weight_suf[weight_suf >= 0.7,]
  #construire comme dictionary de labels
  # s_lab<-sapply(df_weight_suf[,1], function(x) subset(F_suf[F_suf$suf5==x,],Freq==max(Freq),select = df_af.label)$df_af.label)
  # df_labs<-data.frame(suffix=df_weight_suf[,1],label=s_lab,stringsAsFactors=FALSE)
  # return(df_labs)
  return(df_weight_suf[,1])
}

construct_prefix<-function(a, b){
  ####Construire dataframe de préfixes de base des label
  #input
      #    a: tous les token
      #    b: label de token
  #output
      #   dataframe "prefix"  "label" 
  
  
  df_af <- data.frame(word = a,label = str_replace_all(b,"[BI-]",""), stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix 
  pre4 <- ifelse(nchar(df_af$word) >=5 ,str_sub(df_af$word, 1,4), 'non_non')
  ####Construire dataframe prefix(4 grams) avec weight####
  df_pref <- data.frame(pre4,df_af$label, stringsAsFactors = FALSE)
  df_pref <- df_pref[df_pref$pre4 != "non_non" & is.na(str_extract(df_pref$pre4, "-"))==TRUE,]
  F_pref <- as.data.frame(table(df_pref))#compte frequence de suffix
  F_pref <- F_pref[order(-F_pref$Freq), ]
  F_pref_100 <- F_pref[which(F_pref$df_af.label != "O"), ]
  pref_100 <- unique(F_pref_100$pre4)[1:100]
  weight_pref <- rep(0, 100)
  j<-1
  for(i in pref_100){
    nb_in<-sum(F_pref$Freq[which(F_pref$pre4 == i & F_pref$df_af.label != "O")])
    nb_out<-sum(F_pref$Freq[which(F_pref$pre4 == i & F_pref$df_af.label == "O")])
    weight_pref[j] <- (nb_in - nb_out)/(nb_in + nb_out)
    j <- j+1
  }
  df_weight_pref <- data.frame(pref_100,weight_pref)
  df_weight_pref <- df_weight_pref[weight_pref>=0.7,]
  # p_lab<-sapply(df_weight_pref[,1], function(x) subset(F_pref[F_pref$pre4==x,],Freq==max(Freq),select = df_af.label)$df_af.label)
  # df_labs<-data.frame(prefix=df_weight_pref[,1],label=p_lab,stringsAsFactors=FALSE)
  # return(df_labs)
  return(df_weight_pref[,1])
}

extract_affixes<-function(words, pref, suf){
  ###find affix par word###
  #input
      #    w:word token
      #    pref:dataframe de prefix
      #    suf:dataframe de suffix
  #output
      # (data.frame): feature des affixes
  df_af<-data.frame()
  for(w in words){
    if(nchar(w)<5) df_af<-rbind(df_af,rep(FALSE,2))
    else{
      w_suf<-str_sub(w, -5)
      w_pref<-str_sub(w, 1,4)
      if(length(grep(w_suf, suf))==0) wsuf <- FALSE
      else wsuf <- TRUE   #suf[grep(w_suf,suf[,1]),2]
      if(length(grep(w_pref, pref))==0) wpref<-FALSE
      else wpref <- TRUE  #suf[grep(w_pref,pref[,1]),2]
      
      # if(class(wsuf)=="data.frame"){
      #   wsuf<-wsuf[1,1]
      # }
      # if(class(wpref)=="data.frame"){
      #   wpref<-wpref[1,1]
      # }
      df_af<-rbind(df_af,c(as.character(wpref),as.character(wsuf)),stringsAsFactors=FALSE)
    }
  }
  colnames(df_af)<-c("suffix","prefix")
  return(data.frame(words,df_af,stringsAsFactors=FALSE))
}

# suffixes.dict <- construct_suffix(features.df$word, features.df$label)
# prefixes.dict <- construct_prefix(features.df$word, features.df$label)
# affixes.df <- extract_affixes(features.df$word, prefixes.dict, suffixes.dict)

# write.table(affixes.df, "affixes.var")
affixes.df <- read.table("affixes.var")

features.svt.train.df <- dplyr::mutate(features.svt.train.df, 
                                       prefixes = affixes.df$prefix,
                                       suffixes = affixes.df$suffix,
                                       doc_id = doc_id$x)

features.svt.test.df <- dplyr::mutate(features.svt.test.df, 
                                      prefixes = affixes.df$prefix,
                                      suffixes = affixes.df$suffix,
                                      doc_id = doc_id$x)



write.table(features.svt.train.df, "features_svt_train.var")
write.table(features.svt.test.df, "features_svt_test.var")


# Features fenêtre symétrique w[-1], w[0], w[1]

features.full.svt.train.df <- features.svt.train.df[2:(nrow(features.svt.train.df)-1),]
features.full.svt.train.df <- dplyr::mutate(features.full.svt.train.df, 
                                            word_prev = features.svt.train.df$word[1:(nrow(features.svt.train.df)-2)],
                                            lemma_prev = features.svt.train.df$lemma[1:(nrow(features.svt.train.df)-2)],
                                            pos_prev = features.svt.train.df$pos[1:(nrow(features.svt.train.df)-2)],
                                            svt_prev = features.svt.train.df$svt[1:(nrow(features.svt.train.df)-2)],
                                            prefixes_prev = features.svt.train.df$prefixes[1:(nrow(features.svt.train.df)-2)],
                                            suffixes_prev = features.svt.train.df$suffixes[1:(nrow(features.svt.train.df)-2)]
                                            )
features.full.svt.train.df <- dplyr::mutate(features.full.svt.train.df, 
                                            word_next = features.svt.train.df$word[3:nrow(features.svt.train.df)],
                                            lemma_next = features.svt.train.df$lemma[3:nrow(features.svt.train.df)],
                                            pos_next = features.svt.train.df$pos[3:nrow(features.svt.train.df)],
                                            svt_next = features.svt.train.df$svt[3:nrow(features.svt.train.df)],
                                            prefixes_next = features.svt.train.df$prefixes[3:nrow(features.svt.train.df)],
                                            suffixes_next = features.svt.train.df$suffixes[3:nrow(features.svt.train.df)]
                                            )

features.full.svt.test.df <- features.svt.test.df[2:(nrow(features.svt.test.df)-1),]
features.full.svt.test.df <- dplyr::mutate(features.full.svt.test.df, 
                                            word_prev = features.svt.test.df$word[1:(nrow(features.svt.test.df)-2)],
                                            lemma_prev = features.svt.test.df$lemma[1:(nrow(features.svt.test.df)-2)],
                                            pos_prev = features.svt.test.df$pos[1:(nrow(features.svt.test.df)-2)],
                                            svt_prev = features.svt.test.df$svt[1:(nrow(features.svt.test.df)-2)],
                                            prefixes_prev = features.svt.test.df$prefixes[1:(nrow(features.svt.test.df)-2)],
                                            suffixes_prev = features.svt.test.df$suffixes[1:(nrow(features.svt.test.df)-2)]
)
features.full.svt.test.df <- dplyr::mutate(features.full.svt.test.df, 
                                            word_next = features.svt.test.df$word[3:nrow(features.svt.test.df)],
                                            lemma_next = features.svt.test.df$lemma[3:nrow(features.svt.test.df)],
                                            pos_next = features.svt.test.df$pos[3:nrow(features.svt.test.df)],
                                            svt_next = features.svt.test.df$svt[3:nrow(features.svt.test.df)],
                                            prefixes_next = features.svt.test.df$prefixes[3:nrow(features.svt.test.df)],
                                            suffixes_next = features.svt.test.df$suffixes[3:nrow(features.svt.test.df)]
)

write.table(features.full.svt.train.df, "features.full.svt.train.var")
write.table(features.full.svt.test.df, "features.full.svt.test.var")


