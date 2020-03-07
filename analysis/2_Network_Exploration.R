## SCRIPT VERSION FOXTROT

##---------------------------------------------------------------------
## Information on this script:
## This script is intended to conduct a network analysis investigation
## on selected measures of the ALSPAC Birth Cohort Study. The script is
## designed to allow flexibilty in adding/removing variables into and out
## of the network model. A novel feature added to this script is the 
## seperation of network connections by their weight in the network, and
## and the capability to highlight only certain key nodes which bridge
## categories or latent factors.
##---------------------------------------------------------------------
## This script was created as part of a Dissertation project 
## "Exploring the Underlying Structures of Comorbidity in Childhood 
## Psychopathology: A Network Analysis of Symptom Relationships"   
##---------------------------------------------------------------------
## This code was written by the author, as part of a BSc student dissertation
## Please contact the UCL Department of Psychology and Language Sciences
## if you wish you use this code. Unauthorised reproduction is not permitted.
## DISCLAIMER: This code was written as a learning exercise and is still
## in early development. Be sure to cut redundant code where necessary.
##---------------------------------------------------------------------

# RUN ME on a single time point! unhash code to select T2 data

# ---------------------------------------------------------

NF=nodefileT1 #Holds location of file containing ALSPAC nodes and category labels 
#NF=nodefileT2

splitby=T1index #REMEMBER TO CHANGE ME :( 
#splitby=T2index

datalocation=T1_ALSPAC_Processed
#datalocation=T2_ALSPAC_Processed

setwd(outputpath)

##----------------------------------------------------------------
## 1.1 Initialization
##----------------------------------------------------------------

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

# ------------------------------------------------------------------

#reading settings listed above
nodelist<-read.csv(NF)
print("loading from:");print(NF)

#collate all nodes in nodefile (e.g kr123)
allnodes<-as.vector(nodelist$node)

#collate all categories of node (e.g "Hyperactivity")
allitems<-as.vector(nodelist$topic)

#allitems has the group codes for each node - list is created in parallel to the list of nodes
groups<-allitems 

#load parrallel list with individual labels for each node 
Names<-nodelist$label

##...................................
## Importing Datasets
##...................................

rawdata<-read.csv(datalocation)[2:60]

## -------------------------------------------------------------------------------------------- ##
print(boxup("Data Processing",centre=T))

##----------------------------------------------------------------
## 3.1 EBicglasso corrected partial correlation matrix generation
##----------------------------------------------------------------

#As instructed by Epskamp2017: see their paper for further detail on use
results<-estimateNetwork(rawdata,default = "EBICglasso", threshold= T,missing="pairwise") 

##---------------------------------------------------------------------------------
## 3.2 Summary Statstics and Descriptive results: Strength, Betweenness & Closeness
##---------------------------------------------------------------------------------

tNETresults<-tnet::as.tnet(results$graph) #tnet is used as a way to easily match the node column names to their centrality values:
#their presentation of information in their output is more readable

#Calculating node average Betweenness
Betweeness<-as.data.frame(tnet::betweenness_w(tNETresults))#Betweenness calculation
rownames(Betweeness)<-results[["labels"]]

#Calculating node average Closeness
Closeness<-as.data.frame(tnet::closeness_w(tNETresults))#Closeness calculation
rownames(Closeness)<-results[["labels"]]

#Calculating the sum scores for each column (node) within the data.
frequencytable<-colSums(rawdata,na.rm=T)      #NOTE:The name Frequency table is *wrong* it is actually a weighted sum score per node
frequencytable<-as.data.frame(frequencytable)

#Calculating Descriptive Stats
print(boxup("Rplot Network Descriptive Statsitics",centre=T))

NetworkBetweenAvg<-sum(Betweeness$betweenness)/results$nNode
NetworkBetweenSD<-sd(Betweeness$betweenness)

NetworkCloseAvg<-sum(Closeness$closeness)/results$nNode
NetworkcloseSD<-sd(Closeness$closeness)

totalNodeStrength<-sum(abs(results$graph))
averageNodeStrength<-totalNodeStrength/results$nNode
SDedgeWeight<-sd(abs(results$graph))

NetworkAverage<-c(NetworkBetweenAvg,NetworkCloseAvg,averageNodeStrength)
NetworkSD<-c(NetworkBetweenSD,NetworkcloseSD,SDedgeWeight)

Descriptives<-c("Betweenness","Closeness","Edge Strength")

NetDescriptives<-as.data.frame(Descriptives)
NetDescriptives<-cbind(NetDescriptives,as.data.frame(NetworkAverage))
NetDescriptives<-cbind(NetDescriptives,as.data.frame(NetworkSD))

#Terminal output
print(NetDescriptives)

#.CSV file output
write.csv(frequencytable,file="Sumscoretable.csv")
write.csv(Betweeness,file="TablenodeBetween.csv")
write.csv(Closeness,file="TablenodeClose.csv")
#Saving correlation matrix output to external file.
write.csv(results$graph,file="cormatrix.csv")
write.csv(NetDescriptives,file="DescriptiveNetwork.csv")

##---------------------------------------------------------------------
## 3.3 Rplot (Network Plot) using matrix results from EstimateNetwork()
##---------------------------------------------------------------------

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## NOTE: Script crashes here if no plot is already generated     ::
##       with the error "Cannot copy from null device".          ::
##       to fix this, have (any) plot open before running script ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
netqg<-qgraph(results$graph)

dev.copy(png,'Rplot.png',width=16, height=10, units="in",res=500)

netqg<-qgraph(results$graph,layout="spring",
              labels = colnames(data), 
              groups = groups,
              palette="rainbow",
              nodeNames = Names, 
              legend.mode = "style2",
              legend.cex = 0.171,
              details=T, minimum=0.1)
dev.off() 

summary(results)


##---------------------------------------------------------------------
## 3.4 Estimating centrality indicies and plotting Centrality graphs
##---------------------------------------------------------------------

Central<-centrality(results)

####Network plotting and Centrality measure graph plotting#####
dev.copy(png,'Strengthplot.png',width=8, height=10, units="in",res=500)
centralityplotS<-centralityPlot(netqg,include =c("Strength"),orderBy = "Strength") #plotting of centrality indices is centered around 1: 
dev.off()                                                                          #should find a way to make this plotting consistent with the presentation of betweeenness scores per node, but thats not an issue for now

dev.copy(png,'Closenessplot.png',width=8, height=10, units="in",res=500)
centralityplotC<-centralityPlot(netqg,include =c("Closeness"),orderBy = "Closeness")
dev.off()

dev.copy(png,'Betweennessplot.png',width=8, height=10, units="in",res=500)
centralityplotB<-centralityPlot(netqg,include =c("Betweenness"),orderBy = "Betweenness")
dev.off()


##---------------------------------------------------------------------
## 3.5 Exploratory graph Analysis creation and plotting
##---------------------------------------------------------------------
print(boxup("Exploratory Graph Analysis (EGA)",centre=T))

#creating EGA.graph from rawdata
EGA.graph<-EGA(data=rawdata,model="glasso",plot.EGA =F)

summary(EGA.graph) #should ideally save the output of this, but also not sure how. Its not very important though... The graph says enough.

#EGA.graph output is different from estimatenetwork (default lavcor:: or EBICglasso) network as it uses a walktrap algothrim to emphasise difference between
#node clusters. You cannot force both networks to be the same because the EGA network output has gone through an additional clustering (processsing) step.
#I can, however, superimpose the groups identified by the walktrap clustering algorithm onto the original qgraph plot, which is what I do below:

##..........................................................
## Converting EGA grouping labels to labels usable by qgraph
##..........................................................

Lgroups<-c()
for(i in EGA.graph$wc){
  i<-toString(i)
  i<-paste("factor",i)
  Lgroups=c(Lgroups,i)
}
Lgroups


#Plotting EGA plot with DSM grouping
dev.copy(png,'EGAplot-DSM Grouping.png',width=16, height=10, units="in",res=500) #EGA DOES estimate latent factors: see documentation by Golio & Epskamp 2017 for further information
EGA.Qgraph<-qgraph(EGA.graph$network,layout="spring",groups=groups,details=T,palette="rainbow",minimum=0.1)
dev.off()

#Plotting EGA plot with latent grouping
dev.copy(png,'EGAplot-Latent Grouping.png',width=16, height=10, units="in",res=500)
EGA.Lgraph<-qgraph(EGA.graph$network,layout="spring",groups=Lgroups,details=T,palette="rainbow",minimum=0.1)
dev.off()


source('E:/MyRSpace/2019_Dissertation/analysis/settings/flowerplot.R')

H=colnames(results$graph)[1:5]
IM=colnames(results$graph)[6:9]
IN=colnames(results$graph)[10:18]
SPA=colnames(results$graph)[19:24]

SA=colnames(results$graph)[25:30]
G=colnames(results$graph)[31:37]
C=colnames(results$graph)[38:45]
D=colnames(results$graph)[46:50]
E=colnames(results$graph)[51:59]

allgroups=list(H,IM,IN,SPA,SA,G,C,D,E)



print(open_box("Generating Individual Plots per Category..."))

# fun little counter, ignore me
fx="0"

# One connect plot is generated per disorder category, visualising only the connections in that disorder category to symptoms from other disorders.
# All other connections are greyed out. 
for(disorder in allgroups){
  title="flowerplot"
  title<-paste0(title,disorder[1],".png");title
  
  dev.copy(png,title,width=20, height=10, units="in",res=500) 
  flowerplot(results$graph,disorder,graphlayout="groups",qgraphgroups = groups,overridecolour=TRUE,cutinput = 0.2,graphtitle=disorder)
  dev.off()
  
  print(fx)
  fx=paste0(fx,"0")
}

# Generating a summary of betweenness values for comparison with above plots 
Betweensummary<-NULL
for(category in allgroups){
  groupmean<-mean(Betweeness[category,2])
  groupsd<-sd(Betweeness[category,2])
  
  groupsummary<-c(groupmean,groupsd)
  
  frame<-data.frame(category = strsplit(category[1],"-")[[1]][1],mean = groupmean,sd=groupsd)
  Betweensummary<-rbind(Betweensummary,frame)
}

write.csv(Betweensummary,"Betweensummary.csv")

break

###########################################################################
###########################################################################
###                                                                     ###
###     SECTION 5 PARTIAL CORRELATION ESTIMATION AND OTHER ANALYSIS     ###
###                                                                     ###
###########################################################################
###########################################################################
print(boxup("Calculating Between Disorder Category Correlations",centre=T))

##-------------------------------------------------------------------------
## 3.5 Recoding variables and collating scores across disorder categories
##-------------------------------------------------------------------------

Hy<-Rsumrawdata(Hyperactivity,datALSP) #function takes all the nodes listed in Hyperactivity and adds their columns together to create a sum score
Im<-Rsumrawdata(Impulsivity,datALSP)   #same with the rest of the other nodes
In<-Rsumrawdata(Inattention,datALSP)
Sep<-Rsumrawdata(SeparationAnxiety,datALSP)
SA<-Rsumrawdata(SocialAnxiety,datALSP)
GA<-Rsumrawdata(GAD,datALSP)
Co<-Rsumrawdata(Compulsions,datALSP)
Mo<-Rsumrawdata(Mood,datALSP)
Ex<-Rsumrawdata(Externalising,datALSP)

#binding into big dataframe containing all elements of the dataset 
data<-cbind(Hy,Im,In,Sep,SA,Co,GA,Mo,Ex) 

##.....................................................
## Analysis: Generation Correlation matrix with heatmap 
##.....................................................

#creating correlation matrix using lavcor - this is used because the lavaan:: package is used in estimatenetwork() and caluclates a polycholric correlation matrix.
data<-as.data.frame(data)
Lcor<-lavaan::lavCor(data)

dev.copy(png,'corplot.png',width=8, height=10, units="in",res=500)
corrplot(Lcor,method="color", type = "upper", addCoef.col="black", outline=F, diag=T, 
         col=colorRampPalette(c("deepskyblue1","white","indianred3"))(200))
dev.off()


ExternalisingGroups<-c("E","H","IM","IN")
InternalisingGroups<-c("D","SPA","SA","G")

for(i in Frame[,5:6]){print(i)}

