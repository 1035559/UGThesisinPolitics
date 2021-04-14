speeches44 <- read.csv("C:/Users/matth/Downloads/master44.csv")
speeches45 <- read.csv("C:/Users/matth/Downloads/master45.csv")
listseats <- read.csv("C:/Users/matth/Downloads/ListElectorate.csv")
whipping <- read.csv("C:/Users/matth/Downloads/WhipPrediction.csv")
parliaments<- read.csv("C:/Users/matth/Downloads/44to45.csv")
library(dplyr)
library(ggplot2)
require(moonBook)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
require(stringr)
require(RColorBrewer)
require(stargazer)
require(wordcloud)
require(wordcloud2)
require(austin)
require(tm)
require(SnowballC)
require(quanteda)
require(quanteda.textmodels)
require(corpus)

Healthcare=c((rep("health",times=86)),(rep("public",times=35)),(rep("services",times=30)),(rep("hospital",times=22)),(rep("service",times=18)),(rep("crown",times=16)),(rep("enterprise",times=16)),(rep("system",times=15)),(rep("advice",times=11)),(rep("regional",times=10)))
Justice=c((rep("person",times=19)),(rep("police",times=17)),(rep("court",times=17)),(rep("justice",times=12)),(rep("evidence",times=10)),(rep("force",times=9)),(rep("concern",times=8)),(rep("information",times=8)),(rep("inquiry",times=7)),(rep("crime",times=7)))
International=c((rep("country",times=37)),(rep("world",times=16)),(rep("unite",times=14)),(rep("nation",times=10)),(rep("years",times=10)),(rep("international",times=10)),(rep("great",times=9)),(rep("zealander",times=8)),(rep("trade",times=8)),(rep("place",times=7)))
Education=c((rep("child",times=36)),(rep("school",times=29)),(rep("education",times=28)),(rep("family",times=27)),(rep("social",times=19)),(rep("woman",times=19)),(rep("young",times=15)),(rep("teacher",times=14)),(rep("programme",times=14)),(rep("housing",times=13)))
Fiscal=c((rep("company",times=19)),(rep("industry",times=19)),(rep("state",times=15)),(rep("public",times=11)),(rep("radio",times=11)),(rep("board",times=10)),(rep("provide",times=9)),(rep("control",times=9)),(rep("management",times=9)),(rep("investment",times=9)))
Electoral=c((rep("election",times=17)),(rep("happen",times=14)),(rep("thing",times=14)),(rep("political",times=13)),(rep("reason",times=12)),(rep("things",times=11)),(rep("wrong",times=11)),(rep("hear",times=10)),(rep("tonight",times=9)),(rep("stand",times=9)))
Civic=c((rep("auckland",times=27)),(rep("local",times=27)),(rep("council",times=23)),(rep("community",times=14)),(rep("trust",times=13)),(rep("concern",times=10)),(rep("island",times=10)),(rep("water",times=10)),(rep("regional",times=9)),(rep("important",times=9)))
Monetary=c((rep("percent",times=35)),(rep("million",times=20)),(rep("budget",times=20)),(rep("increase",times=19)),(rep("years",times=19)),(rep("country",times=17)),(rep("employment",times=16)),(rep("economic",times=15)),(rep("zealander",times=14)),(rep("money",times=14)))

Healthcare2=c("health","service","services","public","commission","system","hospital","provide","ministry","state")
Healthcare3=c(51,29,29,25,15,15,12,12,10,10)
Justice2=c("person","police","court","justice","security","evidence","crime","power","case","officer")
Justice3=c(22,18,18,11,10,8,8,8,7,7)
Electoral2=c("country","election","coalition","today","political","zealander","stand","thing","force","peter")
Electoral3=c(32,14,12,11,10,10,8,8,8,7)
Fiscal2=c("business","industry","company","board","country","market","state","price","trade","farmer")
Fiscal3=c(25,25,22,15,14,13,10,10,9,9)
Education2=c("child","school","family","education","community","social","young","benefit","student","programme")
Education3=c(26,23,22,21,18,18,16,15,13,13)
International2=c("maori","crown","claim","affairs","colleague","property","terms","island","settlement","treaty")
International3=c(39,11,10,10,10,9,9,9,8,8)
Civic2=c("auckland","local","council","trust","problem","transport","concern","public","authority","vehicle")
Civic3=c(24,22,13,12,10,9,9,9,9,8)
Monetary2=c("percent","money","increase","million","income","budget","scheme","years","zealander","economic")
Monetary3=c(29,22,21,21,19,16,15,14,12,11)

wordcloud(Healthcare, fixed.asp=TRUE)
wordcloud(Healthcare2,Healthcare3,fixed.asp=TRUE)
wordcloud(Justice, fixed.asp=TRUE)
wordcloud(Justice2,Justice3,fixed.asp=TRUE)
wordcloud(International, fixed.asp=TRUE)
wordcloud(International2,International3,fixed.asp=TRUE)
wordcloud(Education, fixed.asp=TRUE)
wordcloud(Education2,Education3,fixed.asp=TRUE)
wordcloud(Fiscal, fixed.asp=TRUE)
wordcloud(Fiscal2,Fiscal3,fixed.asp=TRUE)
wordcloud(Electoral, fixed.asp=TRUE)
wordcloud(Electoral2,Electoral3,fixed.asp=TRUE)
wordcloud(Civic, fixed.asp=TRUE)
wordcloud(Civic2,Civic3,fixed.asp=TRUE)
wordcloud(Monetary, fixed.asp=TRUE)
wordcloud(Monetary2,Monetary3,fixed.asp=TRUE)

ls<-data.frame(
  "MP"=listseats$MP,
  "mn"=listseats$mean,
  "l"= as.numeric(listseats$listt),
  "ll"=listseats$listt
)

ls
slsp3<-ggplot(ls, aes(x=ll, y=mn, fill=ll)) + 
  stat_boxplot(geom = "errorbar", width=0.2, position = position_dodge(0)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=6,position = position_dodge(0), width=0.2, alpha=3/5) +
  geom_dotplot(aes(x=ll, y=mn), 
               binaxis='y', stackdir="center", method="dotdensity",
               dotsize=1,binwidth=0.015, alpha=3/5) + 
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(x="MP Type", y="% Topics Where MP Predicted to Be Whipped", title="Whipping Prediction for Electorate vs List MPs") +
  scale_x_discrete(labels=c("False" = "Electorate MP", "True" = "List MP")) +
  scale_y_continuous(labels=c("0.0" = "0", "0.2" = "20", "0.4"="40", "0.6"="60")) +
  theme(legend.position="none")
slsp3

parliaments$Parliament <- as.factor(parliaments$Parliament)
parlp3<-ggplot(parliaments, aes(x=Parliament, y=mean, fill=Parliament)) + 
  stat_boxplot(geom = "errorbar", width=0.2, position = position_dodge(0)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=6,position = position_dodge(0), width=0.2, alpha=2/3) +
  geom_dotplot(aes(x=Parliament, y=mean), 
               binaxis='y', stackdir="center", method="dotdensity",
               dotsize=0.66,binwidth=0.015, alpha=2/3) + 
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(x="Parliament of MP", y="Speech Partisanship Score", title="Speech Partisanship Score for Before and After Electoral Reform") +
  scale_x_discrete(labels=c("44" = "44th Parliament (SMDP)", "45" = "45th Parliament (MMP)")) +
  scale_y_continuous(labels=c("0.0" = "0", "0.2" = "20", "0.4"="40", "0.6"="60")) +
  theme(legend.position="none")
parlp3

ps<-filter(parliaments,Parliament=="44")
ps
p1<-data.frame(
  "MP"=ps$MP,
  "mean"=ps$mean,
  "type"=ps$Parliament
)
p2<-data.frame(
  "MP"=ls$MP,
  "mean"=ls$mn,
  "type"=ls$ll
)
p2$type=as.factor(p2$type)
p<-bind_rows(p1,p2)
p
parlp4<-ggplot(p, aes(x=type, y=mean, fill=type)) + 
  stat_boxplot(geom = "errorbar", width=0.2, position = position_dodge(0)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=6,position = position_dodge(0), width=0.2, alpha=2/3) +
  geom_dotplot(aes(x=type, y=mean), 
               binaxis='y', stackdir="center", method="dotdensity",
               dotsize=0.66,binwidth=0.015, alpha=2/3) + 
  scale_fill_manual(values = c("darkgreen", "indianred", "darkred")) +
  labs(x="Parliament & Type of MP", y="Speech Partisanship Score", title="Speech Partisanship Score for Three Types of MP") +
  scale_x_discrete(labels=c("44" = "44th Parliament (SMDP MP)", "False" = "45th Parliament (MMP Electorate MP)", "True" = "45th Parliament (MMP List MP)")) +
  scale_y_continuous(labels=c("0.0" = "0", "0.2" = "20", "0.4"="40", "0.6"="60")) +
  theme(legend.position="none")
parlp4
ls
parliaments

plsel<-filter(p,(type=="True" | type=="False"))
plsel
p44el<-filter(p,(type=="44" | type=="False"))
p44el
p44ls<-filter(p,(type=="44" | type=="True"))
p44ls
p4445<-p
type=c()
for(t in c(1:dim(p)[1])){
  if(p$type[t]==44){
    type[t]<-44
  } else {
    type[t]<-45
  }
}
type
p4445$type<-type
t.test(mean~type, data=p2, var.equal = FALSE)
wlsel<-wilcox.test(mean~type, data=plsel,var.equal = FALSE)
w44el<-wilcox.test(mean~type, data=p44el,var.equal = FALSE)
w44ls<-wilcox.test(mean~type, data=p44ls,var.equal = FALSE)
w4445<-wilcox.test(mean~type, data=p4445,var.equal = FALSE)
CVlsel<-qwilcox(0.05,dim(plsel[plsel$type=="False",])[1],dim(plsel[plsel$type=="True",])[1])
CV44el<-qwilcox(0.01,dim(p44el[p44el$type=="False",])[1],dim(p44el[p44el$type=="44",])[1])
CV44ls<-qwilcox(0.01,dim(p44ls[p44ls$type=="True",])[1],dim(p44ls[p44ls$type=="44",])[1])
CV4445<-qwilcox(0.01,dim(p4445[p4445$type=="45",])[1],dim(p4445[p4445$type=="44",])[1])
wlsel[3]
w44el[3]
w44ls[3]
w4445[3]
wlsel[c(1,3)]
w44el[c(1,3)]
w44ls[c(1,3)]
w4445[c(1,3)]
CVlsel
CV44el
CV44ls
CV4445

p4445$mean

dim(p44ls[p44ls$type=="True",])[1]
dim(p44ls[p44ls$type=="44",])[1]
