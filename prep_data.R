library(psych)
library(plyr)
setwd("~/Dropbox/r-code/SUS/")
dat=read.csv("medstats_250813.csv")
dat=read.csv("medstats_280813.csv")
names(dat)

extractQ = function(x){
	temp = as.factor(as.character(x[-1]))
	return(temp)
}
extractQ2 = function(x){
	temp = as.character(x[-1])
	return(temp)
}
fac2num = function(x){
	return(as.numeric(as.character(x)))
}

# Remove duplicates and test responses
dat = subset(dat, a!="")
dat = dat[!duplicated(dat$a),]

# Correlates
oe=extractQ(dat$Q1.1)
oe=fac2num(factor(oe, labels=c(NA,0,6,2,4,3,1,5)))

difficulty=extractQ(dat$Q1.2)
difficulty=fac2num(factor(difficulty, labels=c(NA,0,6,3,2,4,1,5)))

redo=extractQ(dat$Q1.3)
redo=6-fac2num(factor(redo, labels=c(NA,0,6,2,4,3,1,5)))

learnsat=extractQ(dat$Q1.4)
learnsat=fac2num(factor(learnsat, labels=c(NA,0,6,3,2,4,1,5)))

cor(cbind(oe,difficulty,redo,learnsat), use="pair")

browser=cbind(extractQ2(dat$Q59_1_TEXT), extractQ2(dat$Q76_1_TEXT), extractQ2(dat$Q62_1_TEXT))
os=cbind(extractQ2(dat$Q59_3_TEXT), extractQ2(dat$Q76_3_TEXT), extractQ2(dat$Q62_3_TEXT))
res=cbind(extractQ2(dat$Q59_4_TEXT), extractQ2(dat$Q76_4_TEXT), extractQ2(dat$Q62_4_TEXT))
for(i in 1:nrow(browser)){
	browser[i,1] = ifelse( sum(browser[i,]!="")==1 , browser[i,(browser[i,]!="")], "")
	os[i,1] = ifelse( sum(os[i,]!="")==1 , os[i,(os[i,]!="")], "")
	res[i,1] = ifelse( sum(res[i,]!="")==1 , res[i,(res[i,]!="")], "")
}
browser=browser[,1]
os=os[,1]
res=res[,1]

# try to get timing which didn't export right

# How easy or difficult was it to learn how to use the system?
q1=extractQ(dat$Q45)
q1=6-fac2num(factor(q1, labels=c(NA,6,0,5,1,3,4,2)))
# How much more is there to learn about the system?
q2=extractQ(dat$Q46)
q2=4-fac2num(factor(q2, labels=c(NA,4,1,3,2,0)))
# How confident were you using the system?
q3=extractQ(dat$Q47)
q3=fac2num(factor(q3, labels=c(NA,4,2,0,1,3)))
# How easy or difficult is it to use the system?
q4=extractQ(dat$Q50)
q4=6-fac2num(factor(q4, labels=c(NA,6,0,5,1,3,4,2)))
# How consistent is the system?
q5=extractQ(dat$Q51)
q5=fac2num(factor(q5, labels=c(NA,4,2,0,1,3)))

sus2=data.frame(q1,q2,q3,q4,q5)

sus2_all=data.frame(sus2, browser, os, res, oe, difficulty, redo, learnsat)[rowSums(is.na(sus2))==0,]
write.csv(sus2_all, "sus2_data.csv", row.names=F)



# REV SUS
# I needed to learn very few things before I could get going with this system
# I think that I would not need any support of a technical person to be able to use this system
# I did not feel very confident using the system
# I found the system very manageable to use
# I would imagine that most people would learn to use this system very slowly
# I found the system appropriately simple
# I thought the system was hard to use
# I found the various functions in this system were not well integrated
# I did not think there was too much inconsistency in this system
# I do not think that I would like to use this system frequently
labAD = function(x, rev=0){
	x=as.character(x[-1])
	x[x=="Strongly disagree (1)"]=0
	x[x=="(2)"]=1
	x[x=="(3)"]=2
	x[x=="(4)"]=3
	x[x=="Strongly agree (5)"]=4
	x=abs((-4*rev)+as.numeric(x))
	return(x)
}
revsus=subset(dat, select=Q75_2:Q75_15)
revsus[1,]
rev_sus=data.frame(
	p1=labAD(revsus[,1]),
	p2=labAD(revsus[,2]),
	p3=labAD(revsus[,3],1),
	p4=labAD(revsus[,4]),
	p5=labAD(revsus[,5],1),
	p6=labAD(revsus[,6]),
	p7=labAD(revsus[,7],1),
	p8=labAD(revsus[,8],1),
	p9=labAD(revsus[,9]),
	p10=labAD(revsus[,10],1))

rev_sus_all=data.frame(rev_sus, browser, os, res, oe, difficulty, redo, learnsat)[rowSums(is.na(rev_sus))==0,]
write.csv(rev_sus_all, "rev_sus_data.csv", row.names=F)



# Original SUS
# I needed to learn a lot of things before I could get going with this system
# I think that I would need the support of a technical person to be able to use this system
# I felt very confident using the system
# I found the system very cumbersome to use
# I would imagine that most people would learn to use this system very quickly
# I found the system unnecessarily complex
# I thought the system was easy to use
# I found the various functions in this system were well integrated
# I thought there was too much inconsistency in this system
# I think that I would like to use this system frequently

orisus=subset(dat, select=Q59_2:Q59_15)
orisus[1,]
ori_sus=data.frame(
	p1=labAD(orisus[,1],1),
	p2=labAD(orisus[,2],1),
	p3=labAD(orisus[,3]),
	p4=labAD(orisus[,4],1),
	p5=labAD(orisus[,5]),
	p6=labAD(orisus[,6],1),
	p7=labAD(orisus[,7]),
	p8=labAD(orisus[,8]),
	p9=labAD(orisus[,9],1),
	p10=labAD(orisus[,10]))

ori_sus_all=data.frame(ori_sus, browser, os, res, oe, difficulty, redo, learnsat)[rowSums(is.na(ori_sus))==0,]
write.csv(ori_sus_all, "ori_sus_data.csv", row.names=F)
