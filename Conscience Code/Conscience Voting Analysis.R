require(ggplot2)
require(forecast)
require(changepoint)
require(data.table)
require(segmented)
require(strucchange)
require(bcpa)
require(stargazer)
require(tidyverse)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(readxl)
require(tseries)
require(tsbox)
require(smooth)
ConscienceVotesDataLegacy <- read_excel("D:/FHSnotes/Thesis/ConscienceVotesDataLegacy.xlsx")
ConscienceVotesDataLegacy[is.na(ConscienceVotesDataLegacy)]<-0   #remove NA

ac=data.frame("Date"=c(ConscienceVotesDataLegacy$Date),
              "Nat"=c(ConscienceVotesDataLegacy$Nat)*2,
              "NatN"=c(ConscienceVotesDataLegacy$NatN),
              "Lab"=c(ConscienceVotesDataLegacy$Lab)*2,
              "LabN"=c(ConscienceVotesDataLegacy$LabN),
              "All"=c(ConscienceVotesDataLegacy$All)*2,
              "Gre"=c(ConscienceVotesDataLegacy$Gre)*2,
              "AllN"=c(ConscienceVotesDataLegacy$AllN),
              "GreN"=c(ConscienceVotesDataLegacy$GreN),
              "NZF"=c(ConscienceVotesDataLegacy$NZF)*2,
              "NZFN"=c(ConscienceVotesDataLegacy$NZFN),
              "Natlist"=c(ConscienceVotesDataLegacy$Natlist)*2,
              "NatlistN"=c(ConscienceVotesDataLegacy$NatlistN),
              "Lablist"=c(ConscienceVotesDataLegacy$Lablist)*2,
              "LablistN"=c(ConscienceVotesDataLegacy$LablistN),
              "Natel"=c(ConscienceVotesDataLegacy$Natel)*2,
              "NatelN"=c(ConscienceVotesDataLegacy$NatelN),
              "Label"=c(ConscienceVotesDataLegacy$Label)*2,
              "LabelN"=c(ConscienceVotesDataLegacy$LabelN) #Extracts data relevant for this analysis. Doubles raw cohesion scores for Rice (1925) index
)
AvgCohesion = data.frame(
  "Date"=c(ac$Date),
  "ParlAvg"=(ac$Nat*ac$NatN+ac$Lab*ac$LabN+ac$NZF*ac$NZFN+ac$All*ac$AllN+ac$Gre*ac$GreN)/(ac$NatN+ac$LabN+ac$NZFN+ac$AllN+ac$GreN),
  "NLAvg"=(ac$Nat*ac$NatN+ac$Lab*ac$LabN)/(ac$NatN+ac$LabN),
  "ListAvg"=c(NA[0:24],((ac$Natlist*ac$NatlistN+ac$Lablist*ac$LablistN+ac$NZF*ac$NZFN+ac$All*ac$AllN+ac$Gre*ac$GreN)/(ac$NatlistN+ac$LablistN+ac$NZFN+ac$AllN+ac$GreN))[25:67]),
  "ElAvg"=c(NA[0:24],((ac$Natel*ac$NatelN+ac$Label*ac$LabelN)/(ac$NatelN+ac$LabelN))[25:67])
)  #ParlAvg is the Rice index of all partes, combined and weighted based on party size for each parliament. NLAvg is the same, but only for the National and Labour parties. ListAvg and ElAvg are the same but only post-1996, and for list and electorate MPs respectively.
AvgCohesion$Date<-c(as.character(ConscienceVotesDataLegacy$Date))
AvgCohesion$Date<-as.Date(c(AvgCohesion$Date),"%d.%m.%y") #Puts the date into date format
AvgCohesion$ParlAvg<-c((-0.5+AvgCohesion$ParlAvg)*ConscienceVotesDataLegacy$Close+0.5)#Converts SRicej to SCRicej

PA=data.frame(
  "Date"=c(AvgCohesion$Date),
  "Datenum"=as.numeric(AvgCohesion$Date),
  "PA"=c(AvgCohesion$ParlAvg) #Creates a stripped down dataset of parliamentary average Rice index and the date as a numeric variable
)

averages <- PA %>%
  select(Date, Avg = PA) %>%
  mutate(Avg_ma01 = rollmean(Avg, k = 3, fill = NA),
         Avg_ma02 = rollmean(Avg, k = 6, fill = NA),
         Avg_ma03 = rollmean(Avg, k = 12, fill = NA)
  ) #Uses rollmean to create new dataframe of rolling averages of 3, 6 and 12 month intervals respectively. 
averages %>%
  gather(metric, value, Avg:Avg_ma03) %>%
  ggplot(aes(Date, value, color = metric)) +
  geom_line() #Plots the three options for rolling averages, alongside the original data.
PAvg2<-data.frame(
  "Date"=averages$Date,
  "Avg"=averages$Avg_ma02
) #Dataframe for the 6 month rolling average data
PAvg2<-drop_na(PAvg2) #Dropping the months 'lost' though the rolling average function

SegPA=data.frame(
  "Date"=c(AvgCohesion$Date),
  "PA1"=c(AvgCohesion$ParlAvg[0:24],NA[25:67]),
  "PA2"=c(NA[0:24],AvgCohesion$ParlAvg[25:67])  #Creates a segmented dataset split at the time of the electoral reform. Only used for visualisation.
)    
rownames(PAvg2)<- NULL
SegPAvg=data.frame(
  "Date"=c(PAvg2$Date),
  "PA1"=c(PAvg2$Avg[0:22],NA[23:62]),
  "PA2"=c(NA[0:22],PAvg2$Avg[23:62])  #Creates a segmented dataset split at the time of the electoral reform. Only used for visualisation.
)
segpaplot<-ggplot(SegPA,aes(x=Date)) +
  geom_line(aes(y=PA1),color="darkgreen") +
  geom_line(aes(y=PA1),data=SegPAvg,color="darkgreen",size=1) +
  geom_line(aes(y=PA2),color="darkred") +
  geom_line(aes(y=PA2),data=SegPAvg,color="darkred",size=1) +
  geom_smooth(method='lm',formula=y~x,aes(y=PA1),color="darkgreen",linetype="dashed") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA2),color="darkred",linetype="dashed") +
  geom_vline(xintercept=405*24,color="black") +
  ylim(0, 1) +
  ggtitle("Legislative party cohesion at conscience votes in New Zealand, 1983-2013: Segmented") +
  xlab("Date") +
  ylab("SCRicekj")    #Plots the segmented dataset
segpaplot
paplot<-ggplot(PA,aes(x=Date)) +
  geom_line(aes(y=PA),color="darkgreen") +
  geom_line(aes(y=Avg),data=PAvg2,color="darkgreen",size=1) +
  geom_vline(xintercept=405*24,color="black") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA),color="darkgreen",linetype="dashed") +
  ylim(0, 1) +
  ggtitle("Legislative party cohesion at conscience votes in New Zealand, 1983-2013") +
  xlab("Date") +
  ylab("SCRicekj")   #Plots stipped down dataset
paplot
PAel=data.frame(
  "Date"=c(AvgCohesion$Date),
  "Datenum"=as.numeric(AvgCohesion$Date),
  "PA"=c(AvgCohesion$ParlAvg[1:24],AvgCohesion$ElAvg[25:67]) #Creates a stripped down dataset of parliamentary average Rice index and the date as a numeric variable
)
SegPAel=data.frame(
  "Date"=c(AvgCohesion$Date),
  "PA1"=c(AvgCohesion$ParlAvg[0:24],NA[25:67]),
  "PA2"=c(NA[0:24],AvgCohesion$ElAvg[25:67])  #Creates a segmented dataset split at the time of the electoral reform. Only used for visualisation.
)    
PAls=data.frame(
  "Date"=c(AvgCohesion$Date),
  "Datenum"=as.numeric(AvgCohesion$Date),
  "PA"=c(AvgCohesion$ParlAvg[1:24],AvgCohesion$ListAvg[25:67]) #Creates a stripped down dataset of parliamentary average Rice index and the date as a numeric variable
)
SegPAls=data.frame(
  "Date"=c(AvgCohesion$Date),
  "PA1"=c(AvgCohesion$ParlAvg[0:24],NA[25:67]),
  "PA2"=c(NA[0:24],AvgCohesion$ListAvg[25:67])  #Creates a segmented dataset split at the time of the electoral reform. Only used for visualisation.
)    

elpaplot<-ggplot(SegPAel,aes(x=Date)) +
  geom_line(aes(y=PA1),color="darkgreen") +
  geom_line(aes(y=PA2),color="darkred") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA1),color="darkgreen",linetype="dashed") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA2),color="darkred",linetype="dashed") +
  geom_vline(xintercept=405*24,color="black") +
  ylim(0, 1) +
  ggtitle("Legislative party cohesion at conscience votes in New Zealand, 1983-2013: Segmented (Electorate Only)") +
  xlab("Date") +
  ylab("SCRicej")    #Plots the segmented dataset
lspaplot<-ggplot(SegPAls,aes(x=Date)) +
  geom_line(aes(y=PA1),color="darkgreen") +
  geom_line(aes(y=PA2),color="darkred") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA1),color="darkgreen",linetype="dashed") +
  geom_smooth(method='lm',formula=y~x,aes(y=PA2),color="darkred",linetype="dashed") +
  geom_vline(xintercept=405*24,color="black") +
  ylim(0, 1) +
  ggtitle("Legislative party cohesion at conscience votes in New Zealand, 1983-2013: Segmented (List Only)") +
  xlab("Date") +
  ylab("SCRicej")    #Plots the segmented dataset

#VISUALISATIONS
paplot
segpaplot
elpaplot
lspaplot

withinSD<-function(psi){
  StdDev<-psi[3]
  breakpointused<-psi[2]
  difference<-breakpointused-as.numeric(Election1996)
  if(difference<StdDev){
    cat("Break predicted at",as.character(as.Date(breakpointused)))
    cat("\nWithin Std Dev of Electoral Reform at",as.character(Election1996))
  } else {
    cat("Break predicted at",as.character(as.Date(breakpointused)))
    cat("\nNot Within Std Dev of Electoral Reform at",as.character(Election1996))
  }
}

PAvglm <- lm(Avg~Date, data=PAvg2)
summary(PAvglm)       #Linear regression for the rolling average data. Still not significant.
as.numeric(as.Date("1996-10-12")) #Numeric value for the date of the 1996 election for reference
Election1996<-as.Date("1996-10-12")
PAvgseg <- segmented(PAvglm, seg.Z = ~ Date, psi = Election1996, control=seg.control(it.max=1, n.boot=1, min.step=100000, fix.npsi=TRUE, seed=Election1996))
summary(PAvgseg) #Segmented regression for the rolling average data. Significant at the P<0.001 level.
PAvgseg$psi
withinSD(PAvgseg$psi)
PAvgsegfit <- fitted(PAvgseg)
PAvgsegmod <- data.frame(Date = PAvg2$Date, Avg = PAvgsegfit)
ggplot(PAvgsegmod, aes(x = Date, y = Avg)) + geom_line() + geom_line(data=PAvg2,aes(x=Date,y=Avg),colour="tomato")
#Plots the breakpoint with the rolling average output by the segmented package.

pscore.test(PAvglm,~Date,k=10,values=Election1996)
start<-as.numeric(PAvg2$Date[1])
end<-as.numeric(PAvg2$Date[62])
range<-end-start
newpsi<-c(0,0.49368*range+start,PAvgseg$psi[3])
withinSD(newpsi)
PAvgseg$psi[3]-(predbreak-PAvgseg$psi[1]) #The date approximated by the pscore.test function is within 1 standard deviation of the election of 1996

earlybp<-as.numeric(PAvg2$Date[11]) #date of a possible alternative, earlier breakpoint as a % of full timescale
earlybp
laterbp<-PA$Datenum[41] #date of a possible alternative, later breakpoint as a % of full timescale
laterbp
latestbp<-PA$Datenum[54]#date of a possible alternative, later breakpoint as a % of full timescale
latestbp


PAvgsegEY <- segmented(PAvglm, seg.Z = ~ Date, psi = earlybp, control=seg.control(it.max=1, n.boot=1, min.step=100000, fix.npsi=TRUE, seed=earlybp))
summary(PAvgsegEY) #Segmented regression for the rolling average data. Significant at the P<0.001 level.
PAvgsegEY$psi
withinSD(PAvgsegEY$psi) #The date of the electoral reform is within 1 Std. Error of the detected breakpoint
as.Date(earlybp)

PAvgsegLR <- segmented(PAvglm, seg.Z = ~ Date, psi = laterbp, control=seg.control(it.max=1, n.boot=1, min.step=100000, fix.npsi=TRUE, seed=laterbp))
summary(PAvgsegLR) #Segmented regression for the rolling average data. Significant at the P<0.001 level.
PAvgsegLR$psi
withinSD(PAvgsegLR$psi) #The date of the electoral reform is within 1 Std. Error of the detected breakpoint
as.Date(laterbp)

PAvgsegLT <- segmented(PAvglm, seg.Z = ~ Date, psi = latestbp, control=seg.control(it.max=1, n.boot=1, min.step=100000, fix.npsi=TRUE, seed=latestbp))
summary(PAvgsegLT) #Segmented regression for the rolling average data. Significant at the P<0.001 level.
PAvgsegLT$psi
withinSD(PAvgsegLT$psi) #The date of the electoral reform is not within 1 Std. Error of the detected breakpoint
as.Date(latestbp)

summary(PAvglm)
stargazer(PAvglm,title="Results",align=TRUE,keep.stat=c("n","rsq"),dep.var.labels=c("SCRicekj"),covariate.labels=c("Date"),digits=1,no.space=TRUE,out="PAvglm.html")


ac2<-slice(ac, 25:67)
ac2

p2<-data.frame(
  "Date"=AvgCohesion$Date,
  "List"=AvgCohesion$ListAvg,
  "Elec"=AvgCohesion$ElAvg
)
p2<-slice(p2,25:67)
ggplot(p2,aes(x=Date)) +
  geom_line(aes(y=List),color="darkgreen") +
  geom_line(aes(y=Elec),color="darkred") +
  geom_smooth(method='lm',formula=y~x,aes(y=List),color="darkgreen") +
  geom_smooth(method='lm',formula=y~x,aes(y=Elec),color="darkred")

p4<-data.frame(
  "Date"=AvgCohesion$Date,
  "SMDP"=c(AvgCohesion$ParlAvg[1:24],NA[25:67]),
  "List"=AvgCohesion$ListAvg,
  "Elec"=AvgCohesion$ElAvg
)
p5<-gather(p4,'SMDP','Elec','List',key="Type",value="Cohesion")
p3<-gather(p2,'List','Elec',key="Type",value="Cohesion")
p3

ggplot(p3,aes(x=Type, y=Cohesion, fill=Type)) +
  stat_boxplot(geom = "errorbar", width=0.2, position = position_dodge(0)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=6,position = position_dodge(0), width=0.2, alpha=2/3) +
  geom_dotplot(aes(x=Type, y=Cohesion), 
               binaxis='y', stackdir="center", method="dotdensity",
               dotsize=0.66,binwidth=0.03, alpha=2/3) + 
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(x="Type of MP", y="SCRicej", title="Voting Cohesion of List vs Electorate MPs at Conscience Votes in Parliament") +
  scale_x_discrete(labels=c("Elec" = "Electorate MPs", "List" = "List MPs")) +
  theme(legend.position="none")

require(forcats)
plot2 <- p5 %>%
mutate(Type = fct_relevel(Type, "SMDP","Elec","List")) %>%
ggplot(aes(x=Type, y=Cohesion, fill=Type)) +
  stat_boxplot(geom = "errorbar", width=0.2, position = position_dodge(0)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=6,position = position_dodge(0), width=0.2, alpha=2/3) +
  geom_dotplot(aes(x=Type, y=Cohesion), 
               binaxis='y', stackdir="center", method="dotdensity",
               dotsize=0.66,binwidth=0.03, alpha=2/3) + 
  scale_fill_manual(values = c("darkgreen", "indianred", "darkred")) +
  labs(x="Type of MP", y="SCRicej", title="Voting Cohesion of List vs Electorate MPs at Conscience Votes in Parliament") +
  scale_x_discrete(labels=c("SMDP" = '40th-44th Parliament (SMDP MP)', "Elec" = "45th-50th Parliament (MMP Electorate MP)", "List" = "45th-50th Parliament (MMP List MP)")) +
  theme(legend.position="none")
plot2

p6<-p5
type=c()
for(t in c(1:dim(p6)[1])){
  if(p6$Type[t]=="SMDP"){
    type[t]<-"SMDP"
  } else {
    type[t]<-"MMP"
  }
}
type
p6$Type<-type
p44el<-p5[p5$Type%in%c("SMDP","Elec"),]
p44el<-p44el[!is.na(p44el$Cohesion),]
p44ls<-p5[p5$Type%in%c("SMDP","List"),]
p44ls<-p44ls[!is.na(p44ls$Cohesion),]

wlsel<-wilcox.test(Cohesion~Type, data=p3,paired = TRUE, var.equal = FALSE)
w44el<-wilcox.test(Cohesion~Type, data=p44el, var.equal = FALSE)
w44ls<-wilcox.test(Cohesion~Type, data=p44ls, var.equal = FALSE)
w4445<-wilcox.test(Cohesion~Type, data=p6, var.equal = FALSE)
CVlsel<-qsignrank(0.01,dim(p3)[1]/2)
CV44el<-qwilcox(0.65,dim(p44el[p44el$Type=="SMDP",])[1],dim(p44el[p44el$Type=="Elec",])[1])
CV44ls<-qwilcox(0.05,dim(p44ls[p44ls$Type=="SMDP",])[1],dim(p44ls[p44ls$Type=="List",])[1])
CV4445<-qwilcox(0.01,dim(p6[p6$Type=="SMDP" & !is.na(p5$Cohesion),])[1],dim(p6[p6$Type=="MMP" & !is.na(p5$Cohesion),])[1])
wlsel[3]
w44el[3]
w44ls[3]
w4445[3]
wlsel[c(1,3)]
CVlsel
w44el[c(1,3)]
CV44el
w44ls[c(1,3)]
CV44ls
w4445[c(1,3)]
CV4445
dim(p44ls[p44ls$Type=="SMDP",])[1]
dim(p44ls[p44ls$Type=="List",])[1]

dim(p44el[p44el$Type=="SMDP",])[1]
dim(p44el[p44el$Type=="Elec",])[1]

p5$Cohesion[!is.na(p5$Cohesion)][1:134]
