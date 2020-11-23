####Construire dataframe de suffixes de base des label
#input
#    a: tous les token
#    b: label de token
#output
#   dataframe "suffix"  "lablel"
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
  #on prend weight superieur 0.7
  df_weight_suf<-df_weight_suf[weight_suf>=0.7,]
  #construire comme dictionary de labels
  # s_lab<-sapply(df_weight_suf[,1], function(x) subset(F_suf[F_suf$suf5==x,],Freq==max(Freq),select = df_af.label)$df_af.label)
  # df_labs<-data.frame(suffix=df_weight_suf[,1],label=s_lab,stringsAsFactors=FALSE)
  # return(df_labs)
  return(df_weight_suf[,1])
}

####Construire dataframe de préfixes de base des label
#input
#    a: tous les token
#    b: label de token
#output
#   dataframe "prefix"  "label" 
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
  # p_lab<-sapply(df_weight_pref[,1], function(x) subset(F_pref[F_pref$pre4==x,],Freq==max(Freq),select = df_af.label)$df_af.label)
  # df_labs<-data.frame(prefix=df_weight_pref[,1],label=p_lab,stringsAsFactors=FALSE)
  # return(df_labs)
  return(df_weight_pref[,1])
}

###find affix par word###
#input
#    w:word token
#    pref:dataframe de prefix
#    suf:dataframe de suffix
find_Affix<-function(words,pref,suf){
  df_af<-data.frame()
  for(w in words){
    if(nchar(w)<5) df_af<-rbind(df_af,rep(FALSE,2))
    else{
      w_suf<-str_sub(w, -5)
      w_pref<-str_sub(w, 1,4)
      if(length(grep(w_suf, suf))==0) wsuf <- FALSE
      else wsuf <- TRUE#suf[grep(w_suf,suf[,1]),2]
      if(length(grep(w_pref, pref))==0) wpref<-FALSE
      else wpref<-TRUE#suf[grep(w_pref,pref[,1]),2]
      
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
