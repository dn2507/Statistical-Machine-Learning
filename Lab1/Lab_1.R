setwd("~/Desktop/Lab1")
college<-read.csv("college.csv")
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
attach(college)
plot(college$Outstate ~ college$Private)
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(Elite)
summary(college)
plot(college$Outstate~college$Elite)
par(mfrow=c(2,2))
hist(college$Top10perc)
hist(college$Top25perc)
hist(college$Grad.Rate)
hist(college$PhD)
range(college$Enroll)
mean(Enroll)
sd(Enroll)
collegeSub<-college[-c(100:200),]
fix(collegeSub)
summary(collegeSub)
range(collegeSub$Enroll)
mean(collegeSub$Enroll)
sd(collegeSub$Enroll)

#Qualitative predictors : Private. College name 
#Quantitative predictorsApps	Accept	Enroll	Top10perc	Top25perc	F.Undergrad	P.Undergrad	Outstate	Room.Board	Books	Personal	PhD	Terminal	S.F.Ratio	perc.alumni	Expend	Grad.Rate