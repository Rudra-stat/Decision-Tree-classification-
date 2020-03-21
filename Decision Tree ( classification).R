library(xlsx)
# SEDANCARS.XLSX
df=read.xlsx(file.choose(),1,header = T)
df=df[,!apply(is.na(df),2,all)]
str(df)
### first 20 observarions of data
data.frame("Household Number"=1:20,"Annual Income(In Lakhs)"=df$Annual_Income,"Household Area (00s ft2)"=df$Household_Area,"Ownership of sedan car"=df$Ownership,check.names = F)
par(mar=c(5.1,5.1,5.1,5.1))
range(df$Annual_Income)    
range(df$Household_Area)
plot(df$Annual_Income,df$Household_Area,las=1,xlab="Annual Income(In Lakhs)",ylab="Household Area (00s ft2)",xlim=c(2,12),ylim=c(13,25),pch=c(21,19)[as.numeric(df$Ownership)])
legend("bottomright",inset = 0.005,c("Owner","Nonowner"),pch=c(19,21),cex = 0.7,x.intersp = 0.5,y.intersp = .5)

# First slpit
abline(h=18.8)
## find the mid values
head(sort(df$Annual_Income),-1)+diff(sort(df$Annual_Income)/2)
head(sort(df$Household_Area),-1)+diff(sort(df$Household_Area)/2)

## For categorical variables
# set of categories is divided into two subsets
# Gini impurity index and entropy measure
## plot of gini vs p1 (proportioon of observations in class 1)
## for a two-class case
P1=seq(0,1,0.1)
gini<-NULL
for( i in 1:length(P1)) {
  gini[i]=1-(P1[i]^2+(1-P1[i])^2)
}
plot(P1,gini,ylab = "Gini index",type = "l")

## plot of entropy vs p1 (proportioon of observations in class 1)
## for a two-class case
P1=seq(0,1,0.1)
entropy<-NULL
for( i in 1:length(P1)) {
  entropy[i]=-(P1[i]*log2(P1[i])+(1-P1[i])*log2(1-P1[i]))
}
plot(spline(P1,entropy),xlab="P1",ylab = "Entropy Measure",type = "l")


## First split in sedancar example
summary(df$Ownership)

# Impurity before split
giorg=1-(10/20)^2-(10/20)^2
emorg<--(10/20)*log2(10/20)-(0/20)*log2(10/20)
# upper rectangle
giniurec=1-(7/10)^2-(3/10)^2
emurec=-(7/10)*log2(7/10)-(3/10)*log2(3/10)
ginilrec=giniurec
enlrec=emurec
ginisplit1=(10/20)*giniurec+(10/20)*ginilrec
emsplit1=(10/20)*emurec+(10/20)*enlrec

ginidelta=ginisplit1-giorg
endelta=emsplit1-emorg

# second split
segments(7,0,7,18.8)

# Final stage
segments(5.8,18.8,5.8,26)
segments(5.8,19.5,13,19.5)
segments(0,18.2,7,18.2)

str(df)
library(rpart)
?rpart
?rpart.control
# Method="class" for a classification tree 
# Method= "anova" for a regression Tree

mod=rpart(Ownership~.,method = "class",data = df,control = rpart.control(cp=0,minsplit = 2,minbucket = 1,maxcompete = 0,maxsurrogate = 0,xval = 0),parms = list(split="gini"))
par(mar=c(0,0,0,00),oma=c(0,0,0,0),xpd=NA)
plot(mod,uniform=T,branch=0.3,compress=T,margin=0.1,nspace=1)
text(mod,splits = T,use.n = T,all = F,minlength = 0,cex=0.8)


## Usign inbuild package
library(rpart.plot)
?rpart.plot
prp(mod,type = 1,extra = 1,under = T,varlen = 0,cex = 0.7,compress = T,Margin = 0,digits = 0,split.cex = 0.8,under.cex = 0.8,nn=T,nn.cex = 0.6)

##First split
modsu=snip.rpart(mod,toss = c(3,6:7,12:13,24:25))
prp(modsu,type = 1,extra = 1,under = T,varlen = 0,cex = 0.7,compress = T,Margin = 0,digits = 0,split.cex = 0.8,under.cex = 0.8,nn=T,nn.cex = 0.6)

modsub=snip.rpart(mod,toss = c(6:7,12:13,24:25))
prp(modsub,type = 1,extra = 1,under = T,varlen = 0,cex = 0.7,compress = T,Margin = 0,digits = 0,split.cex = 0.8,under.cex = 0.8,nn=T,nn.cex = 0.6)

## First three split
modsub2=snip.rpart(mod,toss = c(12:13,24:25))
prp(modsub2,type = 1,extra = 1,under = T,varlen = 0,cex = 0.7,compress = T,Margin = 0,digits = 0,split.cex = 0.8,under.cex = 0.8,nn=T,nn.cex = 0.6)

### 
attributes(mod)
?rpart.object ## see here details of attributes.
summary(mod)
