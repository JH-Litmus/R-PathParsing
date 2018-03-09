install.packages(c('moonBook', 'xlsx', 'psych', 'Hmisc', 'nparcomp'))

library(dunn.test)
library(xlsx)
library(psych)
library(Hmisc)
library(nparcomp)
require(moonBook)

mydata <- read.csv("SWE2.csv", header=TRUE, sep=",")
mydataop = subset(mydata, OP2==1)

fit <- lm(SWER2~SWER2, data=mydataop)
summary(fit)

plot(SWER1~pSUVR, data=mydataop)
abline(fit, col='blue')


l <- mytable(subtype~., data=mydataop, max.ylev = 5, digits = 2, method = 2, show.all = FALSE)
mycsv.mytable(l,file="f:\\resultOPbysubtype.csv")

dunn.test (mydataop$sSUVR, mydataop$subtype, method="bonferroni", kw=TRUE,
           label=TRUE,wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05)

j <- summary(nparcomp(dSUVR~subtype,data=mydataop))
print(j)

mytable(subtype~., data=mydataop, method=3)
mycsv.mytable(k,file="test03.csv")

cor.test(mydataop$SWER1, mydataop$pSUV,method = "spearman")
cor.test(mydata[,k], mydata[,k+1],method = "spearman")
cor(mydata)
print(corr.test(mydataop[2:23], method = "spearman"),short=FALSE)
getOption("max.print")
options(max.print=999999)


mydata[,2]

remove.packages(Hmisc)
rcorr(mydataop$pSUV, mydataop$sSUV, type="spearman")
corr.te

library(ztable)
require(ztable)
k <- mytable(subtype~pSUV, data=mydataop, method=3)
options(ztable.type="xlsx")
ztable(k,type="latex")
