library("png")
library("vegan")
setwd("C:/Users/abira/OneDrive/Desktop/R dir for semester project")
library(readxl)
Data <- read_excel("Semester_Project_2021_data_AWedit.xlsx")
View(Data)
indices <- Data[,c("Treatment","Total_number_of_mosquitoes","Total_number_of_untargeted_dipterans","Total_number_of_lepidopterans")]

colors = terrain.colors(6)[5:1]
## boxplot ##
w<-boxplot(Total_number_of_mosquitoes~Treatment, data=indices, boxwex=0.5, col=colors, 
        cex.axis=0.5, ylab="Total_number_of_mosquitoes")

 summary(w)

##ggplot trial session
library(ggplot2)
ggplot(indices, aes(x=Treatment, y=Total_number_of_mosquitoes)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)

## boxplot using ggplot:Total number of mosquitoes##
a<-ggplot(indices, aes(x=Treatment, y=Total_number_of_mosquitoes, fill=Treatment)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
summary(a)

## boxplot using ggplot: Total number of unwanted dipterans ##
ggplot(indices, aes(x=Treatment, y=Total_number_of_untargeted_dipterans, fill=Treatment)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") + 
  scale_fill_brewer(palette="Dark2")

## boxplot using ggplot:Total number of lepidopterans ##
ggplot(indices, aes(x=Treatment, y=Total_number_of_lepidopterans, fill=Treatment)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

## Mosquito abundance at each location assuming that equal number of traptypes used in each sites lowers the bias ##
indices_1 <- Data[,c("Total_number_of_mosquitoes","Location_ID")]
View(indices_1)
ggplot(indices_1, aes(x=Location_ID, y=Total_number_of_mosquitoes, fill=Location_ID)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
## no. of Untargeted Dipterans at each location ##
indices_2 <- Data[,c("Total_number_of_untargeted_dipterans","Location_ID")]
View(indices_2)
ggplot(indices_2, aes(x=Location_ID, y=Total_number_of_untargeted_dipterans, fill=Location_ID)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")

## no.of Lepidopterans at each marked location ##
indices_3<- Data[,c("Total_number_of_lepidopterans","Location_ID")]
View(indices_3)
ggplot(indices_3, aes(x=Location_ID, y=Total_number_of_lepidopterans, fill=Location_ID)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")

## difference in total number of mosquitoes collected using each treatments ##
indices$Total_number_of_mosquitoes=rowSums(Total_number_of_mosquitoes.matrix)
t.test(Total_number_of_mosquitoes~Treatment,data=indices,alternative="two-sided")
ggplot(indices, aes(x=Treatment, y=Total_number_of_mosquitoes, fill=Treatment)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
indices2$Rarefied2 <- c(rarefy(abundance.matrix2[1:39,], sample=50))
indices2=indices2[,-c(9)]


indices$abundance=rowSums(abundance.matrix)
t.test(abundance~Habitat,data=indices,alternative="greater")
boxplot(abundance~Habitat, data=indices, boxwex=0.5, col=colors,
        cex.axis=1.5, ylab="Abundance",cex.lab=1.5)
indices2$Rarefied2 <- c(rarefy(abundance.matrix2[1:39,], sample=50))
indices2=indices2[,-c(9)]
summary(indices_1)
summary(Data)

library(vegan)

## testing abundance difference among habitats using each treatments ##

mod.abd0 <- lm(Total_number_of_mosquitoes~Treatment,data=indices)
anova(mod.abd0)
anv.abd0<-aov(Total_number_of_mosquitoes~Treatment,data=indices)
summary(anv.abd0)
tukey.abd0<-TukeyHSD(anv.abd) 
summary(tukey.abd0)
par(mar=c(3,6.5,3,2))
plot(tukey.abd0,las=1,cex.axis=0.85)


## Testing unwanted dipteran abundance while using distinct chemical treatments ##
mod.abd1 <- lm(Total_number_of_untargeted_dipterans~Treatment,data=indices)
anova(mod.abd1)
anv.abd1<-aov(Total_number_of_untargeted_dipterans~Treatment,data=indices)
summary(anv.abd1)
tukey.abd1<-TukeyHSD(anv.abd1)
par(mar=c(3,6.5,3,2))
plot(tukey.abd1,las=1,cex.axis=0.85)

#Testing lepidopteran abundance while using distinct chemical treatments ##
mod.abd2 <- lm(Total_number_of_lepidopterans~Treatment,data=indices)
anova(mod.abd2)
anv.abd2<-aov(Total_number_of_lepidopterans~Treatment,data=indices)
summary(anv.abd2)
tukey.abd2<-TukeyHSD(anv.abd2)
par(mar=c(3,6.5,3,2))
plot(tukey.abd2,las=1,cex.axis=0.85)

## Dunnet test : mosquitoes v/s treatment ##
library(DescTools)
mod.abd <- lm(Total_number_of_mosquitoes~Treatment,data=indices)
anova(mod.abd)
anv.abd<-aov(Total_number_of_mosquitoes~Treatment,data=indices)
summary(anv.abd)
indices$Treatment<- as.factor(indices$Treatment)
DunnettTest(Total_number_of_mosquitoes~Treatment,data = indices,control="UV",conf.level=0.9)
par(mar=c(3,6.5,3,2))
plot(tukey.abd,las=1,cex.axis=0.85)



citation("DescTools")
citation("vegan")
citation("ggplot2")
summary(Data)

mosq<-read.csv("Mosq_AB.csv")
View(Mosq)

## Modeling the mosquito abundance/trap night with the average temperature of the preceding day ##
modT<-glm(Mosq_tot~Temp,data=mosq,family="poisson")
summary(modT)
newmosq=data.frame(temp=mosq$Temp)
newmosq$EmodTR<-predict(modT,type="response")
plot(Mosq_tot~Temp,data=mosq)

## Modeling the mosquito abundance/trap night with the average humidity of the preceding day ##
modH<-glm(Mosq_tot~Hum,data=mosq,family="poisson")
summary(modH)
newmosqH=data.frame(hum=mosq$Hum)
newmosqH$EmodHR<-predict(modH,type="response")
plot(Mosq_tot~Hum,data=mosq)

library(dunn.test)

kruskal.test(Total_number_of_lepidopterans~Treatment,data=indices)
dunn.test(indices$Total_number_of_lepidopterans,indices$Treatment,altp = TRUE)


library(readxl)
Mosq_AB_1 <- read_excel("Mosq_AB_1.xls", 
                          +     sheet = "Sheet1")
View(Mosq_AB_1)

modT<-glm(Mosq_tot~Temp,data=mosq,family="poisson")
summary(modT)
newmosq=data.frame(temp=mosq$Temp)
newmosq$EmodTR<-predict(modT,type="response")
plot(Mosq_tot~Temp,data=Mosq_AB_1)
lines(EmodTR~temp,data=nmosq)

## number of mosquitoes trapped v/s temperature glm
write.csv(newmosq, "newmosq.csv")
nmosq <- read.csv("newmosq.csv",header = TRUE)
plot(y,x)
x <- Mosq_AB_1$Mosq_tot
y <- Mosq_AB_1$Temp
plot(Mosq_totCTemp,data=Mosq_AB_1)
lines(EmodTR~temp,data=nmosq)

# number of mosquitoes trapped v/s humidity glm

modH<-glm(Mosq_tot~Hum,data=mosq,family="poisson")
summary(modH)
newmosqH=data.frame(hum=mosq$Hum)
newmosqH$EmodHR<-predict(modH,type="response")
plot(Mosq_tot~Hum,data=mosq)
write.csv(newmosqH,"newmosqH.csv")
hmosq<- read.csv("newmosqH.csv",header = T)
plot(Mosq_tot~Hum, data=Mosq_AB_1)
lines(EmodHR~hum, data=hmosq )
