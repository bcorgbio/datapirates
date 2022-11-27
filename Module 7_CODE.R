library(tidyverse)
library(dplyr)
library(ggplot2)
library(MuMIn)

ang <- seq(45,168.75,length.out = 12) #elbow angle
f <- list.files("Project 8 data/",full.names=TRUE)
dat.l <- list()

for(i in f){
  met.dat<- unlist(strsplit(i,"_")) 
  who <- met.dat[2] 
  angle <- met.dat[3]
  activity <- gsub(".csv","",met.dat[4]) 
  dat.l[[i]]<- read_csv(i,col_names=FALSE)%>%
    mutate(who=who,angle=angle,activity=activity)
}
dat <- do.call(rbind,dat.l)
dat$X1<-gsub("Reading: ","",as.character(dat$X1))
dat$X1 <- gsub("lbs","",as.character(dat$X1))
dat$X1 <- as.numeric(as.character(dat$X1))

max_value <- dat%>%
  group_by(who,angle,activity)%>%
  slice(which.max(X1))

fmax <- dat%>%
  group_by(activity,who)%>%
  slice(which.max(X1))
colnames(fmax) <- c("FMAX","who","angle_max","activity")

normalized_table <- merge(max_value,fmax,by="who")%>%  
  filter(activity.x==activity.y)
normalized_table$fnorm <- (normalized_table$X1/normalized_table$FMAX)
fnorms <- c(normalized_table$X1/normalized_table$FMAX)

df1 <- normalized_table%>%
  select(who,angle,activity.x,fnorm)
df1$angle <- as.numeric(as.character(df1$angle))
df1_fatigue <- df1[df1$activity.x=="fatigue",]
df1_control <- df1[df1$activity.x=="control",]


poly.m2 <- lm(fnorm~poly(angle,2),df1_control) #second order
poly.m3 <- lm(fnorm~poly(angle,3),df1_control) #third order
poly.m4 <- lm(fnorm~poly(angle,4),df1_control) #fourth order

AICc(poly.m2,poly.m3,poly.m4) #the fourth model fits the best

x.pred <- seq(45,157.5,length.out = 1000) #define 1000 angles from our range

normF.pred <- predict(poly.m4,newdata = data.frame(angle=x.pred)) #predict the force using 1000 angles

qplot(angle,fnorms)+geom_point(aes(x=x.pred,y=normF.pred),col="red")+geom_point(aes(x=x.pred[which.max(normF.pred)],y=normF.pred[which.max(normF.pred)]),size=5,col="blue")
                                                                                                  
x.pred[which.max(normF.pred)]
