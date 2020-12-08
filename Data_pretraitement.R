####install et charger les packages####
list.of.packages <- c("udpipe", "dplyr", "xml2", "stringr", "quanteda","svMisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(udpipe)
library(dplyr)
library(xml2)
library(stringr)
library(quanteda)
#### Import du fichier xml####

#import version de Yang Yang avec rootNode
# xml_data <- read_xml(x = as(rootNode,"character"))

#importer avec xml2
xml_data <- read_xml("GENIA_term_3.02/GENIAcorpus3.02.xml")

#### Extraction de tous les éléments lex et sem####
# lex (lexical): entité labelisée
# sem (semantic): label de l'entité
#initialise sem et lex comme vecteurs vides
sem <- c()
lex <- c()

#initialise path comme chemin pour trouver tous les balises <cons></cons>
path <- "//sentence/cons"

#trouver tous les balises <cons></cons> avec xml2
att <- xml_find_all(xml_data, xpath = path)

#boucle tant qu'on trouve encore de balises <cons></cons>
while (length(att) > 0){
  
  #ajouter dans vecteur sem les valeurs de l'attribute sem
  sem <- c(sem,xml_attr(att,"sem"))
  
  #ajouter dans vecteur lex les valeurs de l'attribute lex
  lex <- c(lex,xml_attr(att,"lex"))
  
  #modifier le path en ajoutant encore un autre node cons 
  path <- paste0(path,"/cons")

  #retrouver tous les balises <cons></cons> embriquées par une autre balise <cons></cons>
  att <- xml_find_all(xml_data, xpath = path)
}

#### Création d'un dataframe avec 2 colonnes lex et sem####
lex_sem.df <- as.data.frame(cbind(lex,sem),stringsAsFactors = FALSE )
  
#enlever tous les lignes qui ont des valeurs NA (les termes sans sem)
lex_sem.df <- lex_sem.df[complete.cases(lex_sem.df),]

#garder que des lignes uniques
lex_sem.df <- unique(lex_sem.df)

#### Fonctions pour couper les lex et sem compliqués (ceux avec AND OR ...)####

#fonction split_all
split_all <- function(str){
  # str <- gsub("[][()]","",str)
  # supprimer les "(" , ")" , "*" 
  # str <- gsubfn(".", list("(" = "", ")" = "","*"=""), str)
  str <- str_replace_all(str,"\\(|\\)|\\*","")
  #couper les termes qui sont separées par " " 
  return(unlist(strsplit(x = str," ")))
}

#fonction find_intersect
find_intersect <- function(df){
  
  #creer une copie de dataframe
  df_temp <- df
  
  #creer un dataframe vide mais a les memes colonnes que df
  df_empt <- df[0,]
  
  #initialise un vecteur vide pour stoker les index des lignes a enlever
  index <- c()
  
  #boucle pour aller tout au long du dataframe
  for (i in 1:nrow(df)){
    #afficher ieme iteration
    # print(i)
    require(svMisc)
    progress(i, max.value = nrow(df))
    # Sys.sleep(0.01)
    
    #utiliser split_all pour ieme valeur de la colonne lex de dataframe
    lex <- split_all(df[i,1])
    #utiliser split_all pour ieme valeur de la colonne sem de dataframe
    sem <- split_all(df[i,2])
    
    #trouver le mot en commun de ces 2 valeurs
    common <- intersect(lex,sem)
    
    #s'il existe common
    if(length(common) > 0){
      #on a trouvé
      # print("found!")
      
      #stoker i dans index
      index <- c(index, i )
      
      #supprimer le(s) mot(s) commun(s) de lex et sem 
      lex <- setdiff(lex,common)
      sem <- setdiff(sem,common)
      
      #creer un data frame en combinant en colonne lex et sem
      mat <- as.data.frame(cbind(lex,sem),stringsAsFactors = FALSE)
      
      #ajouter chaque ligne de mat vers le dataframe vide
      df_empt <- rbind(df_empt,mat)
    }
  
  }
  
  #supprimer les index des lignes complexe dans la copie
  df_temp <- df_temp[-index,]
  
  #combiner df_temp et df_empt
  df_temp <- rbind(df_temp, df_empt)
  
  return(df_temp)
}

#### Création d'un nouveau dataframe sans les lignes de lex et sem complexes (voir fontions ci-dessus)####

lex_sem_clean.df <- find_intersect(lex_sem.df) #il prend quelques moments car on a plus de 30000 lignes
lex_sem_clean.df <- unique(lex_sem_clean.df)
# nrow(lex_sem_clean.df)
# [1] 35508

#### Ensemble des articles####
# id <- xml_find_all(test, "//bibliomisc")
# vals.id <- trimws(xml_text(id))
# vals.id <- 1:2000

#fonction de concatener les balises de sentences
concat_text <- function(xmlnode){
  #converti xmlnode 
  xml.node <- read_xml(as.character(xmlnode))
  
  #extraire seulement les phrases
  xml.sentence <- xml_find_all(xml.node,"./sentence")
  
  #concatener les phrases
  xml.full <- paste(xml_text(xml.sentence),collapse = " ")
  
  return(xml.full)
}

#id des docs
vals.id <- 1:2000
#stocker les balises de titre
title <- xml_find_all(xml_data, "//article/title")
#extraire les titres et les stocker dans un vecteur
vals.title <- sapply(title, function(x) {concat_text(x)})

#stocker les balises d'abstract
abstract <- xml_find_all(xml_data, "//article/abstract")
# extraire les resumes et les stocker dans un vecteur
vals.abstract <- sapply(abstract, function(x) {concat_text(x)})

#combiner les titres et les abstracts
vals.article <- paste(vals.title,vals.abstract)

#data frame qui stock les articles
data_article <- data.frame(cbind(doc_id = vals.id, doc = vals.article),
                          stringsAsFactors = FALSE)

#### Création d'un dictionnaire par rapport a lex_sem_clean.df####
#frequence des classes d'entites
sort(table(lex_sem_clean.df$sem),decreasing = TRUE)

#Choix de classes d'entité: DNA, PROTEIN, RNA, CELL_TYPE, CELL_LINE
#trouver tous les sem qui contiennent ces 5 entités
##DNA
ent_dna <- grep(pattern = "DNA",x = unique(lex_sem_clean.df$sem), value = TRUE)
##PROTEIN
ent_protein <- grep(pattern = "protein",x = unique(lex_sem_clean.df$sem), value = TRUE)
##RNA
ent_rna <- grep(pattern = "RNA",x = unique(lex_sem_clean.df$sem), value = TRUE)
##CELL_TYPE
ent_cell_type <- grep(pattern = "cell_type",x = unique(lex_sem_clean.df$sem), value = TRUE)
##CELL_LINE
ent_cell_line<- grep(pattern = "cell_line",x = unique(lex_sem_clean.df$sem), value = TRUE)

#garder que des lignes qui appartiennent a ces 5 entités
lex_sem_5_ent.df <- lex_sem_clean.df[which(lex_sem_clean.df$sem %in% c(ent_dna,ent_protein,ent_rna,ent_cell_type,ent_cell_line)),]

#reconstruire un dictionnaire de ces 5 entités
labelling <- function(cellule){
  
  if(cellule %in% ent_dna) cellule <- "DNA"
  else if (cellule %in% ent_protein) cellule <- "PROTEIN"
  else if (cellule %in% ent_rna) cellule <- "RNA"
  else if (cellule %in% ent_cell_type) cellule <- "CTYPE"
  else cellule <- "CLINE"
  
  return(cellule)
}
#relibeller les entites
lex_sem_5_ent.df$sem <- sapply(lex_sem_5_ent.df$sem,function(x) {labelling(x)})
#garder seulement les lignes uniques
lex_sem_5_ent.df <- unique(lex_sem_5_ent.df)
# length(unique(lex_sem_5_ent$lex))

#replacer les espaces en underscores
lex_sem_5_ent.df <- as.data.frame(apply(lex_sem_5_ent.df, 2, function(x){gsub(" ","_",x)}),stringsAsFactors = FALSE)

#garder que des lex uniques (probleme avec les annotations du corpus)
lex_sem_5_ent.df <- lex_sem_5_ent.df[!duplicated(lex_sem_5_ent.df["lex"]),]

#### IOB tagging####
#les lex uniques qui designent DNA,PROTEIN,RNA,CTYPE,CLINE
term.dna <- unique(lex_sem_5_ent.df$lex[which(lex_sem_5_ent.df$sem == "DNA")])
term.protein <- unique(lex_sem_5_ent.df$lex[which(lex_sem_5_ent.df$sem == "PROTEIN")])
term.rna <- unique(lex_sem_5_ent.df$lex[which(lex_sem_5_ent.df$sem == "RNA")])
term.ctype <- unique(lex_sem_5_ent.df$lex[which(lex_sem_5_ent.df$sem == "CTYPE")])
term.cline <- unique(lex_sem_5_ent.df$lex[which(lex_sem_5_ent.df$sem == "CLINE")])

#remplacer "_" (underscore) par " " (espace) 
term.dna <- as.character(sapply(term.dna, function(x) {gsub("_"," ",x)}))
term.protein <- as.character(sapply(term.protein, function(x) {gsub("_"," ",x)}))
term.rna <- as.character(sapply(term.rna, function(x) {gsub("_"," ",x)}))
term.ctype <- as.character(sapply(term.ctype, function(x) {gsub("_"," ",x)}))
term.cline <- as.character(sapply(term.cline, function(x) {gsub("_"," ",x)}))

#creer un dictionaire avec package quanteda
dict <- dictionary(list(DNA=term.dna,
                        PROTEIN=term.protein
                        ,RNA=term.rna
                        ,CTYPE=term.ctype,
                         CLINE=term.cline
                        ))

#tokeniser les abstracts (tokeniser les mots composés)
#les mots qui se ressemblent et les remplacer dans la liste des tokens 
# Ici on traite le cas particulier de "cell/cells".
# Si besoin, créer une fonction générique pour ajouter d'autres cas particuliers
similar.word <- c("cell","cells") #les mots qui se ressemblent
lemma <- rep("cell", length(similar.word))
toks_replace <- tokens_replace(tokens(data_article$doc), similar.word, lemma) #remplacer les mots
toks_compound <- tokens_compound(toks_replace,dict,join = FALSE)

#enregistrer les id des documents dans doc_id
doc_id <- c()
for (i in 1:length(docnames(toks_compound))){

  svMisc::progress(i, max.value = length(docnames(toks_compound)))
  
  doc_id <- c(doc_id,rep(i,length(as.character(toks_compound[i]))))
}

#data frame pour les tokens et leurs documents id
data_toks <- data.frame(doc_id, toks = as.character(toks_compound), stringsAsFactors = FALSE)

#fonction pour libeller en IOB/BIO les tokens 
iob_tag <- function(word, semantic){
  #separer le mot par "_"
  if (grepl(pattern = "_", word,fixed=TRUE)){
    word.vec <- unlist(strsplit(word,"_"))
  
    label.vec <- ifelse(word.vec == word.vec[1], 
                        paste("B",semantic,sep = "-"), 
                        paste("I",semantic,sep = "-"))
    
  }else{
    word.vec <- word
    
    label.vec <- paste("B",semantic,sep = "-")
    
  }
  
  # return(data.frame(word = word.vec, label = label.vec,row.names = NULL,stringsAsFactors = FALSE))
  return(list(word=word.vec , label=label.vec))
}

#identifiant des doc selon IOB tags
iob_id_doc <- function(word,id,semantic){
  #separer le mot par "_"
  if (grepl(pattern = "_", word,fixed=TRUE)){
    
    word.vec <- unlist(strsplit(word,"_"))
    
    doc.vec <- rep(id,length(word.vec))
    
  }else{
    
    doc.vec <- id
  }
  
  # return(data.frame(word = word.vec, label = label.vec,row.names = NULL,stringsAsFactors = FALSE))
  return(doc.vec)
}

#initialiser les vecteurs pour les tokens et IOB tags
iob.word <- c()
iob.label <- c()
doc_id <- c()

#vecteur qui stocke les tokens apres avoir separé et libellé avec les tag IOB
iob.word <- unlist(sapply(as.character(toks_compound), function(x){
                  if(x %in% lex_sem_5_ent.df$lex) iob_tag(x, lex_sem_5_ent.df$sem[which(lex_sem_5_ent.df$lex==x )])$word
                  else x}),use.names = FALSE)

#vecteur qui stocke les tag IOB qui correspondent a des tokens au dessus
iob.label <- unlist(sapply(as.character(toks_compound), function(x){
                  if(x %in% lex_sem_5_ent.df$lex) iob_tag(x, lex_sem_5_ent.df$sem[which(lex_sem_5_ent.df$lex==x )])$label
                  else "O"}),use.names = FALSE)

#identifiant des documents

doc_id <- unlist(apply(data_toks ,1, function(x){
  if(x[2] %in% lex_sem_5_ent.df$lex) c(doc_id,iob_id_doc(x[2], x[1] ,lex_sem_5_ent.df$sem[which(lex_sem_5_ent.df$lex==x[2])]))
  else c(doc_id,x[1])}),use.names = FALSE)

#longueur de vecteur a
length(iob.word)
#longueur de vecteur b
length(iob.label)
#longueur de vecteur identifiant document
length(doc_id)
#il faut que iob.word et  aient de meme longueur = 497894

names(iob.word) <- "word"
names(iob.label) <- 'label'

# write.table(iob.word, "iob_word.var")
# write.table(iob.label, "iob_label.var")
# write.table(doc_id, "doc_id.var")

# Enregistrer sus forme RData
# save(iob.label,iob.word,doc_id,file="data/word_label_id.RData")
