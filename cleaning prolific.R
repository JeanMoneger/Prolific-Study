
###########################
###### Data Merging  ######
###########################

library(plyr)    
#filenames <- list.files(path = "~/Desktop/DATA PROLI/", pattern = "*", full.names=TRUE)  #Location of the files will vary from one user to the user: do indicate the path to the test data
filenames <- list.files(path = "DATA PROLI/", pattern = "*", full.names=TRUE)  #Location of the files will vary from one user to the user: do indicate the path to the test data
import.list1 <- ldply(filenames, read.csv)
write.csv(import.list1, "dataFILE.csv")
data <- read.csv("dataFILE.csv") #Once again, path will change from one user to the other: locate the file "dataFILE.csv" on your computer and use this path


###########################
###### Data cleaning ######
###########################
install.packages("dplyr")
library(dplyr)

#144
#194
#230
#271
####





#Let's keep only the columns we are interested in:
#data<-data[,c(6,8, 11, 13, 14, 17,  18, 19, 24, 30, 35, 39, 44, 46, 47, 52, 73, 72, 65, 66)] ###AJOUTE LE TYPE ET EMOTION
data<-data[,c(8,9, 13, 15, 16,  17, 18, 20, 21, 26, 32, 33, 37,41, 42, 48, 49, 75, 74, 68, 67, 66, 64, 76)] ###AJOUTE LE TYPE ET EMOTION






#Discard non-words
data<-subset(data, data$type!="non")

# For each target in the LDT, replace RT > 2 with 2:

for (i in 1:nrow(data)) {
  if(data$key_resp_7.rt[i] > 2 & !is.na(data$key_resp_7.rt[i])) 
  {data$key_resp_7.rt[i] <- 2}
} 




#Remove participants with more than 33% of false scores

#Remove participants associated to more than 33% wrong answers
data$numberError<-as.numeric(data$key_resp_7.corr) 
data <- data%>% group_by(date) %>% mutate(Meanerror = mean(key_resp_7.corr, na.rm = T))
data<-subset(data, data$Meanerror >= 0.66)
# Four participants removed : 
#which(table(data$date)==0)

#Our analyses will be conducted on CORRECT Word answers
#Remove wrong answers : we put all the correct answers in a different data frame
dataRT<- subset(data, data$key_resp_7.corr!=0 )
#dataRT<- subset(data, data$key_resp_9.keys=="y" )


#Compute average NVS & NGS scores
# data2<-subset(data, data$slider_2.rt > 1.5) # Remove trials associated to answers less than 1.5 s.

#### ALTERNATIVELY, IT MIGHT BE SMARTER TO REPLACE THOSE TRIALS WITH NA IN ORDER TO KEEP ORDER IN THE COMING STEPS, CONSIDER USING/
 data2 <-data
 data2[, "slider_2.rt"][data2[, "slider_2.rt"] <1.5] <- NA
 
 
data2<-data2%>%group_by(date, Echelle) %>% mutate(MeanNVSNGS = mean(slider_2.response, na.rm=T)) 
# 2 participants removed if method 1 (subset) is used
# which(table(data2$date)==0)



#####WARNING : participants removed
###     2020-10-22_11h06.32.922
###     2020-10-22_11h03.12.421 #But why?
###     2020-10-22_11h02.12.941
###     2020-10-22_10h58.02.161
###     2020-10-22_10h55.45.886
###     2020-10-22_10h52.48.818
###     2020-10-22_10h48.24.644
###     2020-10-22_10h48.58.053
#which(table(DataNGS$date)==0)

#which(table(DataNVS$date)==0)

#which(table(DataGuilt$date)==0)

#which(table(DataShame$date)==0)






#Compute average Self Compassion score
#Reverse-code reversed items :
data$trials_4.thisIndex<-as.character(data$trials_4.thisIndex)

for(i in 1:nrow(data)){ # Compute reverse scores
  if((data$trials_4.thisIndex[i]=="0" 
      | data$trials_4.thisIndex[i]=="10"
      |data$trials_4.thisIndex[i]== "7"
      |data$trials_4.thisIndex[i]== "8"
      |data$trials_4.thisIndex[i]== "11"
      | data$trials_4.thisIndex[i]=="3") & !is.na(data$trials_4.thisIndex[i])){
    data$slider_3.response[i] <- 6-data$slider_3.response[i]
  }
}

####################################
####################################
#data3<-subset(data, data$slider_3.rt > 1.5)
data3 <-data
data3[, "slider_3.rt"][data3[, "slider_3.rt"] <1.5] <- NA
# which(table(data3$date)==0) #1 participant removed

# By sub scale
# Self-Kindness Items: 2, 6
# Note: It removes participants answering in < 1.5 seconds on both items
dataSelfKindness<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "1" | trials_4.thisIndex== "5") %>% mutate(MeanSelfKind = mean(slider_3.response, na.rm = T)) # %>% distinct(date)

# Self-Judgment Items: 11, 12
# Note: It removes participants answering in < 1.5 seconds on both items
dataSelfJudg<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "10" | trials_4.thisIndex== "11") %>%mutate(MeanSelfJudg = mean(slider_3.response, na.rm = T))# %>% distinct(date)

# Common Humanity Items: 5, 10
# Note: It removes participants answering in < 1.5 seconds on both items
dataCommHum<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "4" | trials_4.thisIndex== "9") %>%mutate(MeanCommHum = mean(slider_3.response, na.rm = T))# %>% distinct(date)

# Isolation Items: 4, 8
# Note: It removes participants answering in < 1.5 seconds on both items
dataIsol<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "3" | trials_4.thisIndex== "7") %>%mutate(MeanIsol = mean(slider_3.response, na.rm = T))# %>% distinct(date)

# Mindfulness Items: 3, 7
# Note: It removes participants answering in < 1.5 seconds on both items
dataMindf<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "2" | trials_4.thisIndex== "6") %>%mutate(MeanMindf = mean(slider_3.response, na.rm = T)) #%>% distinct(date)

# Over-identified Items: 1, 9
# Note: It removes participants answering in < 1.5 seconds on both items
dataOverIden<-data3%>%group_by(date) %>%  filter(trials_4.thisIndex == "0" | trials_4.thisIndex== "8") %>%mutate(MeanOverIden = mean(slider_3.response, na.rm = T))# %>% distinct(date)

data3<-data3%>%group_by(date) %>% mutate(MeanSCS = mean(slider_3.response, na.rm = T)) 



#Compute GASP Scores
#data4<-subset(data, data$slider.rt > 0)
data4<-data
data4[, "slider.rt"][data4[, "slider.rt"] <1.5] <- NA

data4<-data4%>%group_by(date,EMOTION) %>% mutate(MeanGuiltShame = mean(slider.response, na.rm = T)) 

# Compute average words RT
dataRT<-data%>%group_by(date,type) %>% mutate(MeanRTtype = mean(key_resp_7.rt, na.rm = T)) 


#Pass the data to a wide format

DataFail<-dataRT%>%filter(type == "echec")%>%distinct(MeanRTtype)
DataNeut<-dataRT%>%filter(type == "neutre")%>%distinct(MeanRTtype)
DataSuccess<-dataRT%>%filter(type == "reussite")%>%distinct(MeanRTtype)
DataEscape<-dataRT%>%filter(type == "escape")%>%distinct(MeanRTtype)
DataWide<-DataSuccess
DataWide$EscapeRT<-DataEscape$MeanRTtype
DataWide$FailureRT<-DataFail$MeanRTtype
DataWide$NeutralRT<-DataNeut$MeanRTtype
DataWide$SuccessRT<-DataSuccess$MeanRTtype
ProlID<-(dataRT$Prolific.ID.)



DataNVS<-data2%>%filter(Echelle=="NVS")%>%distinct(MeanNVSNGS)
which(table(DataNVS$date)==0)


which(table(DataWide$date)==0)

#DataWide<-DataWide[-which(DataWide$date=="2020-10-22_11h06.32.922"|DataWide$date=="2020-10-22_11h10.21.295"),]

DataWide$Vnarc<-DataNVS$MeanNVSNGS  

DataNGS<-data2%>%filter(Echelle=="NGS")%>%distinct(MeanNVSNGS)
which(table(DataNGS$date)==0)
which(table(DataWide$date)==0)

#DataWide<-DataWide[-which(DataWide$date=="2020-10-22_10h48.24.644"|DataWide$date=="2020-10-22_11h02.12.941"|DataWide$date=="2020-10-22_11h03.12.421"),]


DataWide$Gnarc<-DataNGS$MeanNVSNGS 

DataGuilt<-data4%>%filter(EMOTION == "GUILT")%>%distinct(MeanGuiltShame)
DataShame<-data4%>%filter(EMOTION == "SHAME")%>%distinct(MeanGuiltShame)

which(table(DataGuilt$date)==0)
#DataGuilt<-DataGuilt[-which(DataGuilt$date=="2020-10-22_10h48.24.644"|DataGuilt$date=="2020-10-22_11h02.12.941"|DataGuilt$date=="2020-10-22_11h03.12.421"|DataGuilt$date=="2020-10-22_11h06.32.922"|DataGuilt$date=="2020-10-22_11h10.21.295"),]
which(table(DataShame$date)==0)
#DataShame<-DataShame[-which(DataShame$date=="2020-10-22_10h48.24.644"|DataShame$date=="2020-10-22_11h02.12.941"|DataShame$date=="2020-10-22_11h03.12.421"|DataShame$date=="2020-10-22_11h06.32.922"|DataShame$date=="2020-10-22_11h10.21.295"),]

DataWide$ShameP<-DataShame$MeanGuiltShame
DataWide$GuiltP<-DataGuilt$MeanGuiltShame



DataSC<-data3%>%distinct(MeanSCS)

#DataSC<-as.data.frame(levels(as.factor(data3$MeanSCS)))
which(table(DataSC$date)==0)
which(table(DataWide$date)==0)
#DataSC<-DataSC[-which(DataSC$date=="2020-10-22_10h48.24.644"|DataSC$date=="2020-10-22_11h02.12.941"|DataSC$date=="2020-10-22_11h03.12.421"|DataSC$date=="2020-10-22_11h06.32.922"),]




DataWide$SelfComp<-DataSC$MeanSCS

dataSelfJudg<-dataSelfJudg%>%distinct(MeanSelfJudg)
dataSelfKindness<-dataSelfKindness%>%distinct(MeanSelfKind)
dataOverIden<-dataOverIden%>%distinct(MeanOverIden)
dataIsol<-dataIsol%>%distinct(MeanIsol)
dataCommHum<-dataCommHum%>%distinct(MeanCommHum)
dataMindf<-dataMindf%>%distinct(MeanMindf)

DataWide$MeanSelfJudg<-dataSelfJudg$MeanSelfJudg
DataWide$MeanSelfKind<-dataSelfKindness$MeanSelfKind
DataWide$MeanOveriden<-dataOverIden$MeanOverIden
  DataWide$MeanIsol<-dataIsol$MeanIsol
  DataWide$MeanCommhum<-dataCommHum$MeanCommHum
  DataWide$MeanMindf<-dataMindf$MeanMindf




#####Ces participants ont au moins UN questionnaire exclusivement associé à des TR < 1.5s      
#data<-data[-which(data$date=="2020-10-22_10h48.24.644"|
#                    data$date=="2020-10-22_10h48.58.053"|
#                    data$date=="2020-10-22_10h52.48.818"|
#                    data$date=="2020-10-22_10h55.45.886"|
#                    data$date=="2020-10-22_10h58.02.161"|
#                    data$date=="2020-10-22_11h02.12.941"|
#                    data$date=="2020-10-22_11h03.12.421"|
#                    data$date=="2020-10-22_11h06.32.922"|
#                    data$date=="2020-10-22_11h10.21.295"),]
dataPart<-data%>%filter(EMOTION =="GUILT")%>%distinct(participant)
dataGenre<-data%>%filter(EMOTION =="GUILT")%>%distinct(Gender..Male.Female.Other..)
dataAge<-data%>%filter(EMOTION =="GUILT")%>%distinct(Age.)
DataProl<-data%>% filter(EMOTION =="GUILT")%>%distinct(Prolific.ID.)
DataName<-data%>% filter(EMOTION =="GUILT")%>%distinct(key_resp_9.keys)
DataWide$participant<-dataPart$participant 
DataWide$ProlifID<- DataProl$Prolific.ID.
DataWide$Genre<-dataGenre$Gender..Male.Female.Other..
DataWide$Age<-dataAge$Age.

DataName<-data[which(data$key_resp_9.keys=="y"|data$key_resp_9.keys=="n"),]
DataWide$CheckName<-DataName$key_resp_9.keys


#ADD EXPERIMENTAL CONDITION : if participant number is even --> Experimental / If odd --> Control
DataWide$Cond<- NA
# A vérifier
for(i in 1:nrow(DataWide)){
  if( (DataWide$participant[i]%%2) == 0) {DataWide$Cond[i]<-"Name"}
  else{DataWide$Cond[i]<-"control"}
}


#Scale 
#DataWide$NeutralRT<- scale(as.numeric(DataWide$NeutralRT)) # Neutral RT
#DataWide$SuccessRT<- scale(as.numeric(DataWide$SuccessRT)) #Success RT
#DataWide$FailureRT<- scale(as.numeric(DataWide$FailureRT)) ##Failure RT
#DataWide$EscapeRT<- scale(as.numeric(DataWide$EscapeRT))  # Escape RT
DataWide$ShameP<- scale(as.numeric(DataWide$ShameP))  # Shame proneness
DataWide$GuiltP<- scale(as.numeric(DataWide$GuiltP)) # Guilt proneness
DataWide$Vnarc<-scale(as.numeric(DataWide$Vnarc))  # Vulnerable Narcissism
DataWide$Gnarc<-scale(as.numeric(DataWide$Gnarc))  #Grandiose Narcissism
DataWide$SelfComp<- scale(as.numeric(DataWide$SelfComp)) #Self compassion


#Only for participants indicating their real names (N = 310)
DataName<-data[which(data$key_resp_9.keys=="y"),]


#Code gender
table(DataWide$Genre)
DataWide$Genre<-recode(DataWide$Genre, 
                       f= "FEMALE", 
                       Fe= "FEMALE", 
                       Female= "FEMALE", 
                       female= "FEMALE", 
                       fem = "FEMALE",
                       Fem = "FEMALE",
                       'FALSE' = "FEMALE",
                       m="MALE",
                       male="MALE",
                       Male = "MALE",
                       M = "MALE",
                       o = "OTHER",
                       O= "OTHER",
                       Other="OTHER",  .default = levels(DataWide$Genre))

write.csv(DataWide, "ProliData.csv")




Set<-read.csv("ProliData.csv")
library(MASS)
library(reghelper)
str(Set)

Set<-Set %>%
  mutate_if(is.numeric, scale)



summary(lm(Set$FailureRT~Set$Cond + Set$GuiltP + Set$ShameP + Set$Gnarc + Set$Vnarc + Set$Cond:Set$GuiltP +Set$Cond:Set$ShameP + Set$Cond:Set$Gnarc + Set$Cond:Set$Vnarc + Set$NeutralRT))

SetName<-subset(Set, Set$CheckName=="y")
which(SetName$EscapeRT> median(SetName$EscapeRT)+2.5*mad(SetName$EscapeRT))
SetMad<-SetName[-which(SetName$EscapeRT> median(SetName$EscapeRT)+2.5*mad(SetName$EscapeRT)),]
model<-(lm(EscapeRT~Cond*Vnarc+NeutralRT, data = SetMad))
simple_slopes(model)
library(ggplot2)

SetMad<-SetName[-which(SetName$EscapeRT> median(SetName$EscapeRT)+2.5*mad(SetName$EscapeRT)),]
SetMadF<-SetName[-which(SetName$FailureRT> median(SetName$FailureRT)+2.5*mad(SetName$FailureRT)),]
SetMadS<-SetName[-which(SetName$SuccessRT> median(SetName$SuccessRT)+2.5*mad(SetName$SuccessRT)),]


gg <- ggplot(SetMadF, aes(x=Vnarc, y=ShameP)) + 
  geom_point(aes(col=Cond)) + 
  geom_smooth(aes(col=Cond),method="lm", se=F) + 
  labs(y="Shame", 
       x="V Narc score")


summary(lm((EscapeRT)~Cond*GuiltP+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*GuiltP+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*GuiltP+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*ShameP+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*ShameP+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*ShameP+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*Vnarc+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*Vnarc+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*Vnarc+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*Gnarc+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*Gnarc+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*Gnarc+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*SelfComp+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*SelfComp+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*SelfComp+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanCommhum+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanCommhum+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanCommhum+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanMindf+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanMindf+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanMindf+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanSelfKind+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanSelfKind+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanSelfKind+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanIsol+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanIsol+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanIsol+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanSelfJudg+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanSelfJudg+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanSelfJudg+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*MeanOveriden+NeutralRT+Age, data=SetMad))
summary(lm((FailureRT)~Cond*MeanOveriden+NeutralRT+Age, data=SetMadF))
summary(lm((SuccessRT)~Cond*MeanOveriden+NeutralRT+Age, data=SetMadS))

summary(lm((EscapeRT)~Cond*GuiltP+NeutralRT+Age + ShameP, data=SetMad))
summary(lm((FailureRT)~Cond*GuiltP+NeutralRT+Age+ ShameP, data=SetMadF))
summary(lm((SuccessRT)~Cond*GuiltP+NeutralRT+Age+ ShameP, data=SetMadS))

summary(lm((EscapeRT)~Cond*ShameP+NeutralRT+Age + GuiltP, data=SetMad))
summary(lm((FailureRT)~Cond*ShameP+NeutralRT+Age+ GuiltP, data=SetMadF))
summary(lm((SuccessRT)~Cond*ShameP+NeutralRT+Age+ GuiltP, data=SetMadS))

summary(lm((EscapeRT)~Cond*Vnarc+NeutralRT+Age+ ShameP, data=SetMad))
summary(lm((FailureRT)~Cond*Vnarc+NeutralRT+Age+ ShameP, data=SetMadF))
summary(lm((SuccessRT)~Cond*Vnarc+NeutralRT+Age+ ShameP, data=SetMadS))

summary(lm((EscapeRT)~Cond*Gnarc+NeutralRT+Age+ ShameP, data=SetMad))
summary(lm((FailureRT)~Cond*Gnarc+NeutralRT+Age+ ShameP, data=SetMadF))
summary(lm((SuccessRT)~Cond*Gnarc+NeutralRT+Age+ ShameP, data=SetMadS))

summary(lm((EscapeRT)~Cond*SelfComp+NeutralRT+Age+ ShameP, data=SetMad))
summary(lm((FailureRT)~Cond*SelfComp+NeutralRT+Age+ ShameP, data=SetMadF))
summary(lm((SuccessRT)~Cond*SelfComp+NeutralRT+Age+ ShameP, data=SetMadS))




summary(lm(Vnarc~ShameP+GuiltP, data=Set))
summary(lm(Gnarc~ShameP+GuiltP, data=Set))

summary(lm(Vnarc~ShameP+GuiltP+SelfComp, data=Set))
summary(lm(Gnarc~ShameP+GuiltP+SelfComp, data=Set))

summary(lm(Vnarc~MeanCommhum+MeanOveriden+MeanSelfJudg+MeanSelfKind+MeanIsol+MeanMindf, data=Set))
summary(lm(Gnarc~MeanCommhum+MeanOveriden+MeanSelfJudg+MeanSelfKind+MeanIsol+MeanMindf, data=Set))

gg <- ggplot(SetMadF, aes(x=Vnarc, y=FailureRT)) + 
  geom_point(aes(col=Cond)) + 
  geom_smooth(aes(col=Cond),method="lm", se=F) + 
  labs(y="FailureRT", 
       x="Vnarc")
