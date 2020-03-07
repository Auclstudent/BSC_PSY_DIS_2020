# function for categorising nodes by edge weight strength and colouring them

flowerplot<-function(graph,                          # Accepts a correlation matrix as an input
                     SelectedNode=NULL               # Selectnode accepts a position within the correlation matrix
                     ,seperator="-"                  # A seperating character between the node name and node number is needed for this function to work.
                     ,minInput=0.1                   # Minimum edge weight to be shown on graph. Output passed straight to qgraph()
                     ,SelectALL=F,                   # SelectALL = T overrides SelectedNode = NULL, and will highlight all significant bridging connections within the network
                     cutinput=0.2,                   # cutoff edge weight below which edges are fadeded to white. Output passed straight to qgraph()
                     graphlayout="spring",           # layout of network. Output passed straight to qgraph    
                     qgraphgroups=sigGrouping,       # qgraph groups is called upon to identify different levels of significance in a flowerplot. By default this will set to the levels of significance identified by the flowerplot() function. 
                     overridecolour=F,               
                     graphtitle=""
                     ,unrelated_edge_colour="white",
                     n){                  
  
  if(is.null(SelectedNode)){SelectALL=T}
  
  ##-------------------------------------------------------------------------
  ## 4.1 Finding significant (between cluster) connections within the network
  ##-------------------------------------------------------------------------
  
  #finding mean weight of the network. Note: mean that it gives still != mean from summary(results)
  meanEweight<-mean(abs(graph));meanEweight 
  
  sdEweight<-sd(graph)
  
  benchmark1<-meanEweight + sdEweight    
  benchmark2<-meanEweight + sdEweight*2
  benchmark3<-meanEweight + sdEweight*3
  
  #creating empty table so my for() loop doesn't spaz out.
  Frame=NULL
  
  
  ##........................................................................................
  ## for() loop iterating and classifying each node in network
  ## Each node is matched against the 3 benchmarks identifyed above and labelled accordingly
  ##........................................................................................
  
  for(value in graph){
    if(abs(value)<benchmark1){
      HitBenchmark=F
    }else if(abs(value)>benchmark3){
      HitBenchmark=3
    }else if(abs(value)>benchmark2){
      HitBenchmark=2
    }else if(abs(value)>benchmark1){
      HitBenchmark=1
    }
    
    if(HitBenchmark!=F){
      index<-which(value==graph,arr.ind=T)
      node1=rownames(index)[1]
      node2=rownames(index)[2]
      
      node1=strsplit(node1,seperator,fixed=T)
      node2=strsplit(node2,seperator,fixed=T)
      
      Cat1<-node1[[1]][1]
      Cat2<-node2[[1]][1]
      
      Insideconnection<-strcmp(node1[[1]][1],node2[[1]][1])
      
      if(Insideconnection==F){
        
        nodes<-dimnames(index)[[1]]
        
        Keynode<-c(value,nodes,Cat1,Cat2)
        
        Keynodeframe<-data.frame(Weight = Keynode[1],Node1 = Keynode[2],Node2 = Keynode[3],HitBenchmark = HitBenchmark,Cat1,Cat2)
        
        Frame<-rbind(Frame,Keynodeframe)
      }
    }
  }
  
  print(Frame)
  
  if(is.null(Frame)){print("WARNING: no significant between-category connections found!"); break}
  
  ##........................................................................................
  ## The for() loop generates a Data frame including every node within the correlation matrix
  ## Filters are then applied to remove duplicate entries and are seperated into different lists
  ##........................................................................................
  
  #print(boxup("Unique cross-category connections in network ",centre=T))
  
  Bench1Nodes<-filter(Frame,HitBenchmark==1)
  Bench2Nodes<-filter(Frame,HitBenchmark==2)
  Bench3Nodes<-filter(Frame,HitBenchmark==3)
  
  uniqueBench1<-unique(Bench1Nodes[2:4])
  uniqueBench2<-unique(Bench2Nodes[2:4])
  uniqueBench3<-unique(Bench3Nodes[2:4])
  
  uniquelist1<-c(as.vector(unique(uniqueBench1$Node1)),as.vector(unique(uniqueBench1$Node2)))
  uniquelist2<-c(as.vector(unique(uniqueBench2$Node1)),as.vector(unique(uniqueBench2$Node2)))
  uniquelist3<-c(as.vector(unique(uniqueBench3$Node1)),as.vector(unique(uniqueBench3$Node2)))
  
  
  ##-------------------------------------------------------------------------
  ## 4.2 Creating Node labels for qgraph and Network plotting & recording
  ##-------------------------------------------------------------------------
  
  sigGrouping<-c()
  
  total3SD=0
  total2SD=0
  total1SD=0
  totalnotsig=0
  
  #A grouping label is created for each node in the network, and concatenated into a list which is used by qgraph
  for(i in colnames(graph)){
    if((i %in% uniquelist3)){
      sigGrouping=c(sigGrouping,"Has connection 3SDs above mean")
      total3SD=total3SD+1
    }else if((i %in% uniquelist2)){
      sigGrouping=c(sigGrouping,"Has connection 2SDs above mean")
      total2SD=total2SD+1
    }else if((i %in% uniquelist1)){
      sigGrouping=c(sigGrouping,"Has connection 1SD above mean")
      total1SD=total1SD+1
    }else{
      sigGrouping=c(sigGrouping,"Not Significant")
      totalnotsig=0
    }
  } 
  
  #Matching colours to number of groups: white should always be listed last. 
  if(total1SD>0){
    colour = c("#FFC300","#DAF7A6")
    if(total2SD>0){
      colour = c("#FFC300","#FF5733","#DAF7A6")
      if(total3SD>0){
        colour= c("#FFC300","#FF5733","#C70039","#DAF7A6")
      }
    }
  }
  
  ##........................................................................................
  ## HOW THIS WORKS: EDGE LABELLING/HIGHLIGHTING
  ##
  ## Frame is read in a row by row fashion. The names of node1,node2 are used as positions 
  ## in the row/column of the correlation matrix. The order of variables in the correlation 
  ## matrix is always generated in the order which variables are put into R (the order of nodes 
  ## in nodelist.csv). For example, the second node in nodelist.csv will have a X position of 
  ## 2 and a Y position of 2. Therefore, we can use the order of the nodes in this list to 
  ## find and label edges which have been flagged by the function above. 
  ##........................................................................................
  
  #Creating default colour matrix the same size as the correlation matrix: Each position on the matrix corresponds to an edge
  colourmatrix = replicate(length(colnames(graph)),replicate(length(rownames(graph)),unrelated_edge_colour))
  colourmatrix<-as.matrix(colourmatrix)
  
  #Getting Dimension variables of correlation matrix 
  rowNames<-rownames(graph)
  columnNames<-colnames(graph)
  
  #Generating connectplot frame
  Frame<-as.matrix(Frame)
  Frame<-unique(Frame)
  FrameLength<-dim(Frame)[1]
  
  NodeFrame<-NULL
  
  for(row in 1:FrameLength){
    row<-Frame[row,]
    
    if(SelectALL==T){
      colInd<-match(row[3],columnNames)
      rowInd<-match(row[2],rowNames)
      
      Eweight<-graph[colInd,rowInd]
      if(Eweight<0){
        colourmatrix[colInd,rowInd]<-"#B22222"
        colourmatrix[rowInd,colInd]<-"#B22222"
      }else if(Eweight>0){
        colourmatrix[colInd,rowInd]<-"#008000"
        colourmatrix[rowInd,colInd]<-"#008000"
      }
      
    }else if(SelectALL==F){
      if(row[3] %in% SelectedNode|row[2] %in% SelectedNode){
        colInd<-match(row[3],columnNames)
        rowInd<-match(row[2],rowNames)
        
        Eweight<-graph[colInd,rowInd]
        
        Connection<-paste0(row[2],"-",row[3])
      
        partial.correlation<-as.numeric(row[1])
        test.statistic = partial.correlation*sqrt((n-3)/(1-partial.correlation^2))
        p.value = 2*pt(abs(test.statistic), n-3, lower.tail = F)
        
        Node<-as.matrix(c(Connection,partial.correlation,test.statistic,p.value))
        
        NodeFrame<-rbind(NodeFrame,t(Node))
        colnames(NodeFrame)<-c("Connection","Partial","Ttest","p")
        
        if(Eweight<0){
          colourmatrix[colInd,rowInd]<-"#B22222"
          colourmatrix[rowInd,colInd]<-"#B22222"
        }else if(Eweight>0){
          colourmatrix[colInd,rowInd]<-"#008000"
          colourmatrix[rowInd,colInd]<-"#008000"
        }
      }
    }
  }
  
  if(overridecolour==T){colour=NULL}
  
  
  
  qgraph(graph,
         layout=graphlayout,
         labels=colnames(graph),
         groups = qgraphgroups,
         color = colour,
         details=T,
         minimum=minInput,
         edge.color=colourmatrix,
         #nodeNames = Names, 
         #legend.mode = "style1",
         #legend.cex = 0.28,
         title.cex= 0.5,
         cut = cutinput,
         #title= qgraphtitle,
         width = 6,
         height = 5)
  
  return(NodeFrame)
  
}