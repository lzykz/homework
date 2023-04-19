library(caret)
library(tidyverse)
library(vegan)
library(adespatial)
library(gclus)
library(cluster)
library(FD)
library(factoextra)
source("coldiss.R")
#############################
#task1 Read in the doubs into  R and delete the site 8 which has no fishes.
data(doubs,package="ade4")
doubs
spe<-doubs$fish
spe<-spe[-8, ]
env<-doubs$env
env<-env[-8, ]

env_fish<-doubs$fish %>%
  mutate(total_fish = rowSums(doubs$fish[1:27]))

env_fish<-env_fish[-8, ]
#Which site has the most species (and how many species)
#max min which.max which.min
#  89   3        29         1
env_fish %>% summarise(max = max(total_fish), min = min(total_fish),
                       which.max = which.max(total_fish), 
                       which.min = which.min(total_fish))
#Which species is the most widespread (i.e., found in the most sites)
# Lece 25
colSums( env_fish != 0)


##########################################################
#task2
#R模式聚类鱼类群落
spe.t<-t(spe) #用t函数转置矩阵
# 先卡方转化后计算欧氏距离
spe.t.chi <- decostand(spe.t, "chi.square")#卡方转化
spe.t.D16 <- dist(spe.t.chi) 
spe.t.chi.single <- hclust(spe.t.D16, method="single")
spe.t.chi.complete <- hclust(spe.t.D16,method = "complete")
plot(spe.t.chi.single)
plot(spe.t.chi.complete)
coldiss(spe.t.D16, diag=TRUE)
#thth，cogo,Teso一群;Satr,Phph,Neba一群；Ruru,Lece,Lele,Spbi,Chto一群
#Alal,Chna,Baba,Gogo一群,剩下的一群
######################################################################
#Q模式聚合地点
spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm)#弦距离
spe.hel<-decostand(spe,"hel")
spe.dh<-vegdist(spe.hel)#hellinger距离
spe.ch.single <- hclust(spe.ch, method="single")#单连接聚合聚类
spe.ch.complete<-hclust(spe.ch, method ="complete")
plot(spe.ch.single)
plot(spe.ch.complete)
coldiss(spe.ch, diag=TRUE)
#对env进行Q模式聚合
env.norm <- decostand(env, "normalize")
env.ch <- vegdist(env.norm)#弦距离
env.ch.single <- hclust(env.ch, method="single")#单连接聚合聚类
env.ch.complete<-hclust(env.ch, method ="complete")
plot(env.ch.single)
plot(env.ch.complete)
coldiss(env.ch, diag=TRUE)
#对env的site聚类，大致与鱼类群落聚类相似，所有的鱼类群落均与地点对应
################################################
#task3
#根据DCA1的Axis Lengths值进行选择，>4.0选CCA；在3.0-4.0之间，选RDA和CCA都可；
#如果<3.0, 选择RDA分析即可。
decorana(spe)
RDA <- rda(spe,env,scale = T) 
plot(RDA)
#物种数据 Hellinger 预转化（处理包含很多 0 值的群落物种数据时，推荐使用）
spe_hel <- decostand(spe, method = 'hellinger')
rda_tb <- rda(spe_hel~., env, scale = FALSE)
plot(rda_tb)
summary(rda_tb)
#除了pH之外，其他因素都显著影响鱼类分布



