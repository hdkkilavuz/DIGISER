library(tidyr)
library(ggcorrplot)
library(ggpubr)
library(dplyr)
library(pc)

setwd("C:/Users/William/OneDrive - NTNU/Documents/FYSMAT/Applied statistics/DigiserData")
df <- read.csv("indices.csv", head=T, sep = ";")
df <- df[,-1] #remove first index


# Dividing data into the four different levels
lvl0 <- df %>%
  select(names(df)[nchar(names(df))>7 & nchar(names(df))<9])

lvl1 <- df %>%
  select(names(df)[nchar(names(df))>5 & nchar(names(df))<7])

lvl2 <- df %>%
  select(names(df)[nchar(names(df))>2 & nchar(names(df))<5])

lvl3 <- df %>%
  select(names(df)[nchar(names(df))>1 & nchar(names(df))<3])


#Correlation on each level
corr0 <- round(cor(lvl0),1) 
corr1 <- round(cor(lvl1),1)
corr2 <- round(cor(lvl2),1)
corr3 <- round(cor(lvl3),1)

#corrolation plots on each level
c1 <- ggcorrplot(corr0)
c2 <- ggcorrplot(corr1)
c3 <- ggcorrplot(corr2)
c4 <- ggcorrplot(corr3)

ggarrange(c1,c2,c3,c4) #(zoom to see properly)


#PCA test on level 1
pc_1 <- princomp(lvl1)
summary(pc_1)

par(mfrow=c(1,1))
plot(cumsum(pc_1$sdev^2)/sum(pc_1$sde^2), type='b', axes=F, xlab='# of components', ylab='Total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(lvl1),labels=1:ncol(lvl1),las=2)
