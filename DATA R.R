setwd("C:/Users/abira/OneDrive/Desktop/rehma work")
library(readxl)
library(readxl)
SS<- read_excel("Mastersheet.xlsx", 
                          sheet = "Sheet2")
View(SS)


###############LDL Direct#######################
### calculate mean
data("SS")
MSS<- mean(SS$LDLDirect...8)
print(MSS)

#### standard error of the mean

NSS <- length(SS$LDLDirect...8)
print(NSS)
SdSS <- sd(SS$LDLDirect...8)
SeSS <- SdSS/sqrt(NSS)
print(SeSS)

### t-score CI bb

alpha = 0.01
degrees.freedom = NSS - 1
t.score = qt(p= alpha/2, df=degrees.freedom, lower.tail = F)
print(t.score)

### margin error
 margin.error <- t.score*SeSS
lower.bound <- MSS- margin.error 
upper.bound <- MSS+ margin.error
print(c(lower.bound,upper.bound))


#################### LDLM ##################

### calculate mean 
data("SS")
MSM<- mean(SS$LDLM)
print(MSM)

#### standard error of the mean

NSM <- length(SS$LDLM)
print(NSM)
SdSM <- sd(SS$LDLM)
SeSM <- SdSM/sqrt(NSM)
print(SeSM)

### t-score CI 

alpha = 0.01
degrees.freedom = NSM - 1
t.score = qt(p= alpha/2, df=degrees.freedom, lower.tail = F)
print(t.score)

### margin error
margin.error <- t.score*SeSM
lower.bound <- MSM- margin.error 
upper.bound <- MSM+ margin.error
print(c(lower.bound,upper.bound))

################## LDL F #####################
MSF<- mean(SS$LDLF)
print(MSF)

#### standard error of the mean

NSF <- length(SS$LDLF)
print(NSF)
SdSF <- sd(SS$LDLF)
SeSF <- SdSF/sqrt(NSS)
print(SeSF)

### t-score CI 

alpha = 0.01
degrees.freedom = NSS - 1
t.score = qt(p= alpha/2, df=degrees.freedom, lower.tail = F)
print(t.score)

### margin error
margin.error <- t.score*SeSF
lower.bound <- MSF- margin.error 
upper.bound <- MSF+ margin.error
print(c(lower.bound,upper.bound))




################# LDLS ################
MSS<- mean(SS$LDLS)
print(MSS)

#### standard error of the mean

NSS <- length(SS$LDLS)
print(NSS)
SdSS <- sd(SS$LDLS)
SeSS <- SdSS/sqrt(NSS)
print(SeSS)

### t-score CI 

alpha = 0.01
degrees.freedom = NSS - 1
t.score = qt(p= alpha/2, df=degrees.freedom, lower.tail = F)
print(t.score)

### margin error
margin.error <- t.score*SeSS
lower.bound <- MSS- margin.error 
upper.bound <- MSS+ margin.error
print(c(lower.bound,upper.bound))

######################## Accuracy Plot###############

######### LDL cholesterol ###########


library("png")
library("vegan")
library(wesanderson)
library(ggplot2)
library("png")
library("vegan")
library(scales)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plot3D)
library(RColorBrewer)
library(ggplot2)

library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)


library(readxl)
library(readxl)
AC<- read_excel("Mastersheet.xlsx", 
                          sheet = "Sheet5")
View(AC)

unique(sort(AC$LDLDirect))

AC$LDLDirect <- factor(AC$LDLDirect, levels = c('<40', "40-79","80-119", "120-159", "160-199", "200-299", "300-399", ">400"))



library(extrafont)
ACP <-ggplot(AC, aes(fill=Methods, x=factor(LDLDirect, level=c('<40', "40-79","80-119", "120-159", "160-199", "200-299", "300-399", ">400")),y=Accuracy))+
  geom_bar(stat = "identity",position = "dodge", width = 0.8)
  
ACP <-ggplot(AC, aes(fill=Methods, x=LDLDirect,y=Accuracy))+
  geom_bar(stat = "identity",position = "dodge", width = 0.8)
ACP+ ylab("Accuracy, %")+
  xlab(" LDL Cholesterol, mg/dL")+
  
  scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#1A237E","#1976D2","#B3E5fC"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,100,by=10))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold",size = 9),
        panel.background = element_rect(fill = "#FFFFFF",colour = "black",size = 1),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))



############# TG ##############

#setwd("C:/Users/abira/OneDrive/Desktop/Reshma work")

library("png")
library("vegan")
library(wesanderson)
library(ggplot2)
library("png")
library("vegan")
library(scales)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plot3D)
library(RColorBrewer)
library(ggplot2)

library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)


library(readxl)
AC<- 
View(AC)

unique(sort(AC$TG))

AC$TG <- factor(AC$TG, levels = c( "300-399","400-499", "500-599", ">599"))



library(extrafont)
ACPL <-ggplot(AC, aes(fill=Method, x=factor(TG, level=c("300-399","400-499", "500-599", ">599")),y=Accuracy))+
  geom_bar(stat = "identity",position = "dodge", width = 0.8)

#ACPL <-ggplot(AC, aes(fill=Methods, x=TG,y=Accuracy))+
 # geom_bar(stat = "identity",position = "dodge", width = 0.8)

ACPL+ ylab("Accuracy, %")+
  xlab("Triglycerides, mg/dL")+
  scale_color_viridis(discrete = TRUE)+
  theme_ipsum()+
  #scale_fill_brewer(palette = "PuRd")+
  scale_fill_manual(values = c("#1A237E","#1976D2","#B3E5fC"))+
  #scale_fill_manual(values = c("#FCEAE6","#F0A58F","#EA7369","#EB548C","#DB4CB2","#AF4BCE","#7D3AC1"))+
  scale_y_continuous(breaks = seq(0,100,by=10))+
  #scale_fill_discrete("Species",labels=c(expression(italic("Myrmica aimonissabaudiae")),expression(italic("Myrmica cachmiriensis")),expression(italic("Myrmica hecate")),expression(italic("Myrmica inezae")),expression(italic("Myrmica sp.")),expression(italic("Myrmica wardi")),"Non-myrmica"))+
  theme(axis.title.x.bottom = element_text(color="#142459",hjust = 0.5, size=10, face="bold",family = "TT Times New Roman"),
        axis.title.y = element_text(color="#142459", size=10,hjust = 0.5, face="bold",family ="TT Times New Roman"),
        axis.text.y = element_text(size=9,face = "bold",color = "black", angle=0,family ="TT Times New Roman" ),
        axis.text.x = element_text(color = "black", face = "bold",size = 9),
        panel.background = element_rect(fill = "#FFFFFF",colour = "black",size = 1),
        legend.title = element_text(color="#142459",size = 10,face = "bold"),
        legend.title.align = 0.5,
        legend.background = (element_rect(colour = "#142459")),
        legend.text = element_text(color ="#000000",size = 7.3,face = "bold", family = "TT Times New Roman"))

