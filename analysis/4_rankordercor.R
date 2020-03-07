data<-read.csv("rankordercentrality.csv")

plot(x=data$Rank1.B,y=data$Rank2.B)
plot(x=data$Rank1.E,y=data$Rank2.E)

cor.test(data$Rank1.B,data$Rank2.B,method='spearman',exact=F)

cor.test(data$Rank1.E,data$Rank2.E,method='spearman',exact=F)
