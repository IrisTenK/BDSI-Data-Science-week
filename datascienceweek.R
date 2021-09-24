#########################
# Datascienceweek UT
# september 2021
# auteur: Anouk Veldhuis
#########################

library(readr)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(stats)
library(MASS)

# setwd("C:/Anouk/ZGT/datascienceweekUT-Q32021")

test <- read.csv("test.csv", header = T, sep = ",")
train <- read.csv("train.csv", header = T, sep = ",")

train2 <- train
# binair maken prijs (dit was voor de lol)
# train2$SalePricebi <- ifelse(train2$SalePrice>181000,1,0)
# trainset met alleen binaire prijs
# train2 <- train2[,c(1:80,82)]
train3 <- train[,c(2,3,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,35,37,38,39,
                    40,41,42,44,45,46,47,48,49,50,51,52,53,54,55,56,57,62,63,66,67,68,69,70,71,72,76,77,78,79,80,81)]

fit_train <- glm(SalePrice ~ .,data = train3)
summary(fit_train)
# anova(fit_train)
stepAIC(fit_train)

# checken hoeveel unieke variabelen elke kolom heeft
lapply(train2[c(1:80)], unique)
# checken hoeveel NA waardes de kolommen hebben
sapply(train2[c(2:80)], function(x) sum(length(which(is.na(x))))) 
sapply(train[c(2:86)], function(x) sum(length(which(is.na(x))))) 

# kolommen aanpassen
# electrical heeft 1 NA, die maken we nu standaard
train$Electrical[is.na(train$Electrical)] <- "SBrkr"
# pool en andere MiscFeatures samenvoegen binaire waarde
train$MiscCombined <- ifelse((train$PoolArea==0&train$MiscVal==0),0,1)
# Alley binair maken (zijn er 91 met Alley, de rest niet)
# We kunnen Street wel droppen, is 6 met gravel en 1454 met pavement dus die zou ik uberhaupt skippen
train$Alley <- ifelse(is.na(train$Alley),0,1)
# mssubclass jaren samenvoegen en een character meegeven (ik heb echt weinig groepen overgelaten, we zouden er nog wat extra toe kunnen voegen)
train$MSSubClass <- ifelse(train$MSSubClass==20|train$MSSubClass==30|train$MSSubClass==40|train$MSSubClass==120,"A",
                           ifelse(train$MSSubClass==45|train$MSSubClass==50|train$MSSubClass==150,"B",
                                  ifelse(train$MSSubClass==60|train$MSSubClass==70|train$MSSubClass==160,"C",
                                         ifelse(train$MSSubClass==75,"D",
                                                ifelse(train$MSSubClass==80|train$MSSubClass==85|train$MSSubClass==180,"E",
                                                       ifelse(train$MSSubClass==90,"F",
                                                              ifelse(train$MSSubClass==190,"G","H")))))))
# aantal keer zien dat elke waarde in de kolom voorkomt
table(train$MSSubClass)
# fence omzetten
train$Fence[is.na(train$Fence)] <- "NoFence"
train$Fence <- ifelse(train$Fence=="NoFence",0,1)
# Fireplace aanpassen (0 = geen fireplace, 1 = 1 maar slechte/average kwaliteit en minder, 2 = 1 maar goede kwaliteit/veel, 3 = 2 of 3 maar slechte/average kwaliteit en minder, 4 = 2 of 3 en goede kwaliteit/veel)
# LEt op: nieuwe kolom zonder de S op het einde
train$Fireplace <- ifelse(train$Fireplaces==0,0,
                          ifelse(train$Fireplaces==1&(train$FireplaceQu=="Po"|train$FireplaceQu=="TA"|train$FireplaceQu=="Fa"),1,
                                 ifelse(train$Fireplaces==1&(train$FireplaceQu=="Ex"|train$FireplaceQu=="Gd"),2,
                                        ifelse((train$Fireplaces==2|train$Fireplaces==3)&(train$FireplaceQu=="Po"|train$FireplaceQu=="TA"|train$FireplaceQu=="Fa"),3,
                                               ifelse((train$Fireplaces==2|train$Fireplaces==3)&(train$FireplaceQu=="Ex"|train$FireplaceQu=="Gd"),4,5)))))

# Als ik dat histogram bekijk dan denk ik dat we ook alleen naar aantal kunnen kijken, maar dat is simpel gedaan.
hist(train$Fireplace)

# samenvoegen Garageonderdelen
# ik heb aantal autos gedeeld door oppervlakte omdat ik gok dat een ruimere garage meer waarde creeert. Eens?
train$capaciteit <- ifelse((train$GarageCars==0|train$GarageArea==0),0,round(train$GarageArea/train$GarageCars, digits = 0))
# ik heb een puntensysteem gemaakt waarbij de punten voor elk onderdeel opgeteld worden
train$GarageCond[is.na(train$GarageCond)] <- "No"
train$GarageCond <- ifelse(train$GarageCond=="No",0,
                          ifelse(train$GarageCond=="Po"|train$GarageCond=="Fa",1,
                                 ifelse(train$GarageCond=="TA",2,
                                        ifelse(train$GarageCond=="Ex"|train$GarageCond=="Gd",3,4))))
table(train$GarageCond)

train$GarageFinish[is.na(train$GarageFinish)] <- "No"
train$GarageFinish <- ifelse(train$GarageFinish=="No",0,
                             ifelse(train$GarageFinish=="Unf",1,
                                    ifelse(train$GarageFinish=="Rfn",2,
                                           ifelse(train$GarageFinish=="Fin",3,4))))
table(train$GarageFinish)

train$GarageQual[is.na(train$GarageQual)] <- "No"
train$GarageQual <- ifelse(train$GarageQual=="No",0,
                           ifelse(train$GarageQual=="Po"|train$GarageQual=="Fa",1,
                                  ifelse(train$GarageQual=="TA",2,
                                         ifelse(train$GarageQual=="Ex"|train$GarageQual=="Gd",3,4))))
table(train$GarageQual)

train$garagekwali <- train$GarageCond+train$GarageFinish+train$GarageQual
train$garagekwali <- ifelse(train$garagekwali==0,train$garagekwali,train$garagekwali-2)
table(train$garagekwali)
hist(train$garagekwali)

# garage <- filter(train, train$YearBuilt==train$GarageYrBlt)
# train$garagejaar <- train$YearBuilt-train$GarageYrBlt
# masonry <- filter(train, is.na(train$MasVnrType))

# korte test
train4 <- train[,c(2,3,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,35,37,38,39,
                   40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,62,63,66,67,68,69,70,71,72,76,77,78,79,80,81,82,83,84,85,86)]

fit_train <- glm(SalePrice ~ .,data = train4)
summary(fit_train)
# anova(fit_train)
stepAIC(fit_train)

