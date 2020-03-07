## Data Cleaning and Recoding

#For Loading on External Drive - Change this to the location of your package files
.libPaths(LIBPATH)

#Unhash the following comments to install relevant packages for this script
# install.packages(bannerCommenter)
# install.packages(foreign)
# install.packages(psych)
# install.packages(readr)
# install.packages(plotly)
# install.packages(car)
# install.packages("ppcor")
# install.packages(corrplot)
# install.packages("pracma")
# install.packages("qgraph")
# install.packages("parcor")
# install.packages("Matrix")
# install.packages("psych")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("bootnet")
# install.packages("networktools")
# install.packages("EGAnet")
# install.packages("igraph")
# install.packages("rmarkdown")


#Loading Libraries
library(bannerCommenter) #used solely for pretty boxes. Not necessary for analysis or processing
library(foreign)
library(psych)
library(readr)
library(plotly)
library(car)
library("ppcor")
library(corrplot)
library("pracma")
library("qgraph")
library("parcor")
library("Matrix")
library("psych")
library("dplyr")
library("ggplot2")
library("bootnet")
library("networktools")
library("EGAnet")
library("igraph")
library("rmarkdown")
library("tnet")

##----------------------------------------------------------------
## Importing ALSPAC Dataset
##----------------------------------------------------------------

ALSPAC=read.spss(DataLocation,to.data.frame=T,use.value.labels=F) #Reading SPSS File
datALSP = ALSPAC[complete.cases(ALSPAC$cidB2411), ] ## remove missing values from ID variable
datALSP$IID = paste0(datALSP$cidB2411,datALSP$qlet) ## create matching ID (matched ID in .fam file)


###########################################################################
###########################################################################
###                                                                     ###
###                     Data cleaning Time point 1                      ###
###                                                                     ###
###########################################################################
###########################################################################


##default: for running as source
nodelist=read.csv(nodefileT1)

splitby=T1index

print(boxup("Importing Data - T1",centre=T))


#collate all nodes in nodefile (e.g kr123)
allnodes<-as.vector(nodelist$node)

#collate all categories of node (e.g "Hyperactivity")
allitems<-as.vector(nodelist$topic)

#Creating Empty columns to hold each category of node used
Hyperactivity<-c()
Impulsivity<-c()
Inattention<-c()
SeparationAnxiety<-c()
SocialAnxiety<-c()
GAD<-c()
Compulsions<-c()
Mood<-c()
Externalising<-c()

#Node selection
rawdata<-data.frame(select(datALSP,allnodes))


##----------------------------------------------------------------
## Finding, reorganising and relabelling nodes in raw data
##----------------------------------------------------------------

Hcounter=1
IMcounter=1
Incounter=1
SPAcounter=1
SAcounter=1
GADcounter=1
Ccounter=1
Dcounter=1
Ecounter=1

seperator="-"

for (i in nodelist$node){
  number<-match(i,nodelist$node) #Match the index of the node in nodefile to its label
  item<-allitems[number] #find the corresponding label for node
  
  #renameing node
  nodename<-strsplit(i,splitby) #split the node's name to remove the starting value in the sequence e.g. k/r316 -> r316
  nodename<-nodename[[1]][2] #set nodename to be only the last value in the split set
  
  if(item=="Hyperactivity"){Hyperactivity<-c(Hyperactivity,i) #If the item belongs to the Hyperactivity group, add it to the final group list (used in qgraph)
  nodename<-paste0("H",seperator,Hcounter)
  Hcounter=Hcounter+1} #create a new node name for the variable 
  
  else if(item=="Impulsivity"){Impulsivity<-c(Impulsivity,i) #do the same for each other category in the dataset
  nodename<-paste0("IM",seperator,IMcounter)
  IMcounter=IMcounter+1}                     #can probably be done as a function, but I cant be bothered and dont know how  ¯\_(???)_/¯
  
  else if(item=="Inattention"){Inattention<-c(Inattention,i)
  nodename<-paste0("IN",seperator,Incounter)
  Incounter=Incounter+1}
  
  else if(item=="SeparationAnxiety"){SeparationAnxiety<-c(SeparationAnxiety,i)
  nodename<-paste0("SPA",seperator,SPAcounter)
  SPAcounter=SPAcounter+1}
  
  else if(item=="SocialAnxiety"){SocialAnxiety<-c(SocialAnxiety,i)
  nodename<-paste0("SA",seperator,SAcounter)
  SAcounter=SAcounter+1}
  
  else if(item=="GAD"){GAD<-c(GAD,i)
  nodename<-paste0("G",seperator,GADcounter)
  GADcounter=GADcounter+1}
  
  else if(item=="Compulsions"){Compulsions<-c(Compulsions,i)
  nodename<-paste0("C",seperator,Ccounter)
  Ccounter=Ccounter+1}
  
  else if(item=="Mood"){Mood<-c(Mood,i)
  nodename<-paste0("D",seperator,Dcounter)
  Dcounter=Dcounter+1}
  
  else if(item=="Externalising"){Externalising<-c(Externalising,i)
  nodename<-paste0("E",seperator,Ecounter)
  Ecounter=Ecounter+1}
  
  #set the column name of that node to be the new node name.
  colnames(rawdata)[colnames(rawdata)==i]<-nodename
  
}

##----------------------------------------------------------------
## Setting Data to be Zero-coded
##----------------------------------------------------------------

## Note: code is repeated from Rsumrawdata() function earlier
## Note2: The Same method is applied to all columns as only variables using likhert scales were selected 

for(i in colnames(rawdata)){
  rawdata[i][rawdata[i]==1] <- 0 #If value in rawdata is equal to X, set it equal to Y 
  rawdata[i][rawdata[i]==2] <- 1
  rawdata[i][rawdata[i]==3] <- 2
  rawdata[i][rawdata[i]<0] <- NA
  
}


#Saving processed data
write.csv(rawdata,T1_ALSPAC_Processed)




###########################################################################
###########################################################################
###                                                                     ###
###                     Data cleaning Time point 2                      ###
###                                                                     ###
###########################################################################
###########################################################################

nodelist=read.csv(nodefileT2)

splitby=T2index

print(boxup("Importing Data - T2",centre=T))

#collate all nodes in nodefile (e.g kr123)
allnodes<-as.vector(nodelist$node)

#collate all categories of node (e.g "Hyperactivity")
allitems<-as.vector(nodelist$topic)

#Creating Empty columns to hold each category of node used
Hyperactivity<-c()
Impulsivity<-c()
Inattention<-c()
SeparationAnxiety<-c()
SocialAnxiety<-c()
GAD<-c()
Compulsions<-c()
Mood<-c()
Externalising<-c()

#Node selection
rawdata<-data.frame(select(datALSP,allnodes))

##----------------------------------------------------------------
## Finding, reorganising and relabelling nodes in raw data
##----------------------------------------------------------------

Hcounter=1
IMcounter=1
Incounter=1
SPAcounter=1
SAcounter=1
GADcounter=1
Ccounter=1
Dcounter=1
Ecounter=1

seperator="-"

for (i in nodelist$node){
  number<-match(i,nodelist$node) #Match the index of the node in nodefile to its label
  item<-allitems[number] #find the corresponding label for node
  
  #renameing node
  nodename<-strsplit(i,splitby) #split the node's name to remove the starting value in the sequence e.g. k/r316 -> r316
  nodename<-nodename[[1]][2] #set nodename to be only the last value in the split set
  
  if(item=="Hyperactivity"){Hyperactivity<-c(Hyperactivity,i) #If the item belongs to the Hyperactivity group, add it to the final group list (used in qgraph)
  nodename<-paste0("H",seperator,Hcounter)
  Hcounter=Hcounter+1} #create a new node name for the variable 
  
  else if(item=="Impulsivity"){Impulsivity<-c(Impulsivity,i) #do the same for each other category in the dataset
  nodename<-paste0("IM",seperator,IMcounter)
  IMcounter=IMcounter+1}                     #can probably be done as a function, but I cant be bothered and dont know how  ¯\_(???)_/¯
  
  else if(item=="Inattention"){Inattention<-c(Inattention,i)
  nodename<-paste0("IN",seperator,Incounter)
  Incounter=Incounter+1}
  
  else if(item=="SeparationAnxiety"){SeparationAnxiety<-c(SeparationAnxiety,i)
  nodename<-paste0("SPA",seperator,SPAcounter)
  SPAcounter=SPAcounter+1}
  
  else if(item=="SocialAnxiety"){SocialAnxiety<-c(SocialAnxiety,i)
  nodename<-paste0("SA",seperator,SAcounter)
  SAcounter=SAcounter+1}
  
  else if(item=="GAD"){GAD<-c(GAD,i)
  nodename<-paste0("G",seperator,GADcounter)
  GADcounter=GADcounter+1}
  
  else if(item=="Compulsions"){Compulsions<-c(Compulsions,i)
  nodename<-paste0("C",seperator,Ccounter)
  Ccounter=Ccounter+1}
  
  else if(item=="Mood"){Mood<-c(Mood,i)
  nodename<-paste0("D",seperator,Dcounter)
  Dcounter=Dcounter+1}
  
  else if(item=="Externalising"){Externalising<-c(Externalising,i)
  nodename<-paste0("E",seperator,Ecounter)
  Ecounter=Ecounter+1}
  
  #set the column name of that node to be the new node name.
  colnames(rawdata)[colnames(rawdata)==i]<-nodename
  
}

##----------------------------------------------------------------
## Setting Data to be Zero-coded
##----------------------------------------------------------------

## Note: code is repeated from Rsumrawdata() function earlier
## Note2: The Same method is applied to all columns as only variables using likhert scales were selected 

for(i in colnames(rawdata)){
  rawdata[i][rawdata[i]==1] <- 0 #If value in rawdata is equal to X, set it equal to Y 
  rawdata[i][rawdata[i]==2] <- 1
  rawdata[i][rawdata[i]==3] <- 2
  rawdata[i][rawdata[i]<0] <- NA
}


#Saving processed data
write.csv(rawdata,T2_ALSPAC_Processed)













