#install packages
install.packages("XML")
install.packages("SnowballC")
install.packages("tm")

# Load the package required to read XML files.
library(XML)
library(SnowballC)  
library(tm)

#set directory to your current directory which exist the xml file
#setwd(your_directory)

#import XML file
data.xml <- xmlParse(file = "GENIA_term_3.02/GENIAcorpus3.02.xml")

# Exract the root node form the xml file (from )
rootNode <- xmlRoot(data.xml)

# Convert the input xml file to a data frame.
#dataframe <- xmlToDataFrame(data)

#Extract lex and sem
genia_data<-xpathSApply(rootNode, "//sentence/cons[@sem]", xmlAttrs)

#Extract contents in the tag 'cons'
contents<-xpathSApply(rootNode, "//sentence/cons[@sem]", xmlValue)

df_genia_data<-data.frame(genia_data)
df_genia_data<-t(df_genia_data)
#Combined contents with lex sem
df_genia_data<-cbind(contents,df_genia_data)
View(df_genia_data[100,])

#Delete row repeat
df_genia_data<-df_genia_data[!duplicated(df_genia_data),]

#Simplified composite sem
for (i in 1:nrow(df_genia_data)) {
  find<-grep("\\(",df_genia_data[i,3])
  if(length(find)!=0){
    tmp<-as.list(strsplit(df_genia_data[i,3]," "))
    indice<-grep("G#",tmp[[1]])
    df_genia_data[i,3]<-tmp[[1]][indice[1]]
  }
}
unique(df_genia_data[,3])



