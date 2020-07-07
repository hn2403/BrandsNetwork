# This is R code for my project "Brands’ Network and Its Impacts on Foot Traffic in Chicago During the Shelter-In-Place Order"
# Author: Hieu Nguyen

library("dplyr")
library(stringr)
library(tidyr)
library(purrr)
library("ggplot2")
library("reshape2")
source("kIC.R")
library("textir")
require(data.table)

ch_week1 <- read.csv("/Users/hieunguyen/Desktop/Spring 2020/BUSN 41201/Data/Covid19/Weekly Pattern/ch_week1.csv")

# remove NA observations
ch_week1 <- na.omit(ch_week1)

# Exploratory Data Analyses
# Check for outliers
par(mfrow=c(1,2))
boxplot(ch_week1$distance_from_home, ylab="distance_from_home",
        col="red")

ch_week1 <- ch_week1 %>%
  filter(distance_from_home < 600000) # only look at point-of-interests with the distance_from_home<600km

boxplot(log(ch_week1$distance_from_home), ylab="log(distance_from_home)",
        main="distance_from_home < 600km",col="red")

# Aggregate the data by Zip code
ch_week1_eda <- ch_week1 %>%
  group_by(postal_code) %>%
  summarise(num=n(),
            all_visits=sum(raw_visit_counts),
            all_visitors=sum(raw_visitor_counts),
            avg_dist_from_home=mean(distance_from_home),
            avg_median_dwell=mean(median_dwell))

# Plot the histograms
par(mfrow=c(2,3))
hist(ch_week1_eda$num, main = "Histogram of Point-of-Interest", 
     xlab = "count of Point-of-Interest", breaks = 50, 
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$all_visits,  main = "Histogram of Visits", 
     xlab = "number of visits", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$all_visitors,  main = "Histogram of Vsitors", 
     xlab = "number of vsitors", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$avg_dist_from_home,  main = "Histogram of average distance from home", 
     xlab = "average distance from home", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(ch_week1_eda$avg_median_dwell,  main = "Histogram of average median dwell", 
     xlab = "average median dwell", breaks = 50,
     col= c("red","yellow","green","pink"))

knitr::kable(summary(ch_week1_eda[c(3,4,5,6)]))


# Plot the histograms after log transformations 
par(mfrow=c(2,3))
hist(ch_week1_eda$num, main = "Histogram of Point-of-Interest", 
     xlab = "count of Point-of-Interest", breaks = 50, 
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$all_visits),  main = "Histogram of Visits", 
     xlab = "log(number of visits)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$all_visitors),  main = "Histogram of Vsitors", 
     xlab = "log(number of vsitors)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$avg_dist_from_home),  main = "Histogram of average distance from home", 
     xlab = "log(average distance from home)", breaks = 50,
     col = c("red","yellow","green","pink"))
hist(log(ch_week1_eda$avg_median_dwell),  main = "Histogram of average median dwell", 
     xlab = "log(average median dwell)", breaks = 50,
     col= c("red","yellow","green","pink"))





# 5.1
## False Discovery Rate Analysis
library(dummies)

P <- as.data.frame(dummy(ch_week1$brands, sep ="_")[,-1])
p = dim(P)[2]
library(parallel)

margreg <- function(p){
  fit <- lm(visits~p)
  sf <- summary(fit)
  return(sf$coef[2,4]) 
}


cl <- makeCluster(detectCores())

visits <- ch_week1$raw_visit_counts

clusterExport(cl,"visits") 

# Run the regressions in parallel
mrgpvals <- unlist(parLapply(cl,P,margreg))
rm(P)
length(mrgpvals)
hist(mrgpvals, breaks = 20)

# How many statistically significant p-values?
sum(as.numeric(mrgpvals<=0.05))
sum(as.numeric(mrgpvals<=0.01))


# Plot the FDR cutoff
source('fdr.R')
q = 0.01
cutoff = fdr_cut(mrgpvals,q,plotit=TRUE)
cutoff
abline(h=cutoff,lty=2,col=3,lwd=3)
abline(0,q/p,col=2,lwd=2)
abline(0,1/p)


num_discoveries <- sum(as.numeric(mrgpvals<=cutoff))
print(num_discoveries)
print(q*num_discoveries)


mrgpvals_ordered<-mrgpvals[order(mrgpvals,decreasing=F)]
names(mrgpvals_ordered[1:10])



#5.2 Construct the brands'network
# First clean up the variables
brands_data <- ch_week1 %>%
  filter(brands!="") 

brands_data_sub <-brands_data %>%
  group_by(brands) %>%
  summarise(num=n())
keeps <- c("safegraph_place_id", "location_name", "brands", "postal_code", "related_same_day_brand")


brands_network <- subset(brands_data, select = keeps)
brands_network$related_same_day_brand <- brands_network$related_same_day_brand %>%
  str_sub(2,-2) 


brands_network$related_same_day_brand <- gsub(":\\d+","",brands_network$related_same_day_brand)
brands_network$related_same_day_brand <- gsub('"',"",brands_network$related_same_day_brand)

library(splitstackshape)
brands_network <- cSplit(brands_network, "related_same_day_brand", sep=",")


brands_network <- melt(brands_network, id.vars = colnames(brands_network)[1:4],
                       measure.vars = colnames(brands_network)[5:dim(brands_network)[2]], 
                       na.rm = TRUE, variable.name = "source", value.name = "related_same_day_brand")


# Network graph 
library(igraph)

netw2 <- brands_network[,c(3,6)]
netw2<- unique(netw2)


edgemat2 <- cbind(netw2$brands,netw2$related_same_day_brand)
ziplink2 <- graph.edgelist(edgemat2)


## set some color atributes (V() gives back the 'vertices' = nodes)

V(ziplink2)$color = "pink"

V(ziplink2)$frame.color = 0

V(ziplink2)$label.color = "black"

plot(ziplink2, edge.arrow.width=0, edge.curved=FALSE, vertex.label=NA, vertex.frame.color=0, vertex.size=2, edge.arrow.width=.75)

# Neighborhoods for Walmart at orders 1
nei1<-graph.neighborhood(ziplink2, 1, V(ziplink2)["Walmart"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["Walmart"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=2, edge.arrow.width=.75,
     main="Neighborhoods for Walmart at order 1")
unique(V(nei1))

# Neighborhoods for Walmart at orders 2
nei2<-graph.neighborhood(ziplink2, 2, V(ziplink2)["Walmart"])[[1]]
V(nei2)$color <- "green"
sav <- V(nei1)$name
V(nei2)[sav]$color <- "gold"
V(nei2)["Walmart"]$color <- "red"
plot(nei2, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=2, edge.arrow.width=.75,
     main="Neighborhoods for Walmart at order 2")
unique(V(nei2))

# Neighborhoods for Walmart at orders 3
nei3<-graph.neighborhood(ziplink2, 3, V(ziplink2)["Walmart"])[[1]]
V(nei3)$color <- "blue"
V(nei3)[sav]$color <- "gold"
sav2 <- V(nei2)$name
V(nei3)[sav2]$color <- "green"
V(nei3)["Walmart"]$color <- "red"
plot(nei3, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Walmart at order 3")
unique(V(nei3))


# Starbucks
nei1<-graph.neighborhood(ziplink2, 1, V(ziplink2)["Starbucks"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["Starbucks"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=2, edge.arrow.width=.75,
     main="Neighborhoods for Starbucks at order 1")
unique(V(nei1))


# Morton's The Steakhouse
nei1<-graph.neighborhood(ziplink2, 1, V(ziplink2)["Morton's The Steakhouse"])[[1]]
V(nei1)$color <- "gold"
V(nei1)["Morton's The Steakhouse"]$color <- "red"
plot(nei1, edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA,vertex.frame.color=0, vertex.size=6, edge.arrow.width=.75,
     main="Neighborhoods for Morton's The Steakhouse at order 1")
unique(V(nei1)) 

# Calculating Degree and Betweenness of the Nodes 
brands_degree <- degree(ziplink2)
order_degree <- order(brands_degree, decreasing = T) 
brands_degree[order_degree[1:10]]
brands_degree[tail(order_degree,10)]

mb<-betweenness(ziplink2)
order_mb <- order(mb, decreasing = T) 
mb[order_mb[1:10]]
mb[tail(order_mb,10)]

keeps <- c("safegraph_place_id", "location_name", "brands", 'raw_visit_counts', 'raw_visitor_counts', 'raw_visit_counts', 'raw_visitor_counts', 'distance_from_home', 'median_dwell', "postal_code")

# Plot the histogram of degrees
brands_analysis <- subset(brands_data, select = keeps)
brands_degree <- as.data.frame(brands_degree)
brands_analysis$degree <-brands_degree[brands_analysis$brands,]

brands_analysis$degree[is.na(brands_analysis$degree)] = 0 # -> set unconnected brand's degree to 0 (isolated)
hist(brands_analysis$degree, main = "Histogram of degree of the brands" , xlab = "Brand", ylab = "Degree" ,col = c("red", "yellow", "green", "pink"))
hist(log(1+brands_analysis$degree), main = "Histogram of degree of the brands" , xlab = "Brand", ylab = "Degree" ,col = c("red", "yellow", "green", "pink"))

# Plot the histogram of betweenness
mb <- as.data.frame(mb)
brands_analysis$betweenness <-mb[brands_analysis$brands,]
brands_analysis$betweenness[is.na(brands_analysis$betweenness)] = 0 # -> set unconnected brand's degree to 0 (isolated)
hist(brands_analysis$betweenness, main = "Histogram of betweeness of the brands" , xlab = "Brand", ylab = "Degree" ,col = c("red", "yellow", "green", "pink"), breaks = 50)
hist(log(1+brands_analysis$betweenness), main = "Histogram of betweeness of the brands" , xlab = "Brand", ylab = "Degree" ,col = c("red", "yellow", "green", "pink"))



# 5.3 Lasso analysis

# Do OLS first
reg1 <- glm(raw_visit_counts ~ degree, data = brands_analysis)
summary(reg1)
reg2 <- glm(raw_visitor_counts ~ degree, data = brands_analysis)
summary(reg2)
reg3 <- glm(median_dwell ~ degree, data = brands_analysis)
summary(reg3)
reg4 <- glm(distance_from_home ~ degree, data = brands_analysis)
summary(reg4)

library(gamlr)
class(brands_analysis$postal_code)

# Construct the design matrices first
postal_code<-as.data.frame(as.factor(brands_analysis$postal_code))
postal_code2 <- cbind(brands_analysis$degree, postal_code )
colnames(postal_code2) =c("degree","postal_code")
x_cat<-sparse.model.matrix(~degree*postal_code, data=postal_code2)[,-1]
dim(x_cat)


# Naive LASSO
y1 <- brands_analysis$raw_visit_counts
length(y1)
lasso1<-gamlr(x_cat, y=y1,
              standardize=TRUE,lambda.min.ratio=1e-3)

coef(lasso1)
plot(lasso1)

y2 <- brands_analysis$raw_visitor_counts
length(y2)
lasso2<-gamlr(x_cat, y=y2,
              standardize=TRUE,lambda.min.ratio=1e-3)

coef(lasso2)
plot(lasso2)

y3 <- brands_analysis$median_dwell
length(y3)
lasso3<-gamlr(x_cat, y=y3,
              standardize=TRUE,lambda.min.ratio=1e-3)

coef(lasso3)
plot(lasso3)

#Causal/Double Lasso

# FIRST STAGE:

# do LASSO of treatment on confounders

d <- brands_analysis$degree
colnames(postal_code)='postal_code'
x<- sparse.model.matrix(~., data = postal_code)[,-1]
dim(x_cat)

treat <- gamlr(x,d,lambda.min.ratio=1e-4)

plot(treat)

# we isolate dhat (the part of treatment that we can predict with x's)
dhat <- predict(treat, x, type="response") 
plot(dhat,d,bty="n",pch=21,bg=8) 

# R^2
cor(drop(dhat),d)^2

# SECOND STAGE:
causal <- gamlr(cbind(d,dhat,x),y1,free=2,lmr=1e-4)

coef(causal)["d",] 

plot(causal)

## BOOTSTRAP 

n <- nrow(x)

## Bootstrapping our lasso causal estimator is easy

gamb <- c() # empty gamma

for(b in 1:200){
  ## create a matrix of resampled indices
  
  ib <- sample(1:n, n, replace=TRUE)
  
  ## create the resampled data
  
  xb <- x[ib,]
  
  db <- d[ib]
  
  yb <- y1[ib]
  
  ## run the treatment regression
  
  treatb <- gamlr(xb,db,lambda.min.ratio=1e-3)
  
  dhatb <- predict(treatb, xb, type="response")
  
  fitb <- gamlr(cbind(db,dhatb,xb),yb,free=2)
  
  gamb <- c(gamb,coef(fitb)["db",])
  

}

summary(gamb) 
sd(gamb)
hist(gamb)

hist(gamb, col="grey70", xlab="Degree Treatment Effect", main="",breaks=15) 
abline(v=coef(causal)["d",],lwd=2, col="red")
abline(v=quantile(gamb,0.025),col=3,lwd=2)
abline(v=quantile(gamb,0.975),col=3,lwd=2)



