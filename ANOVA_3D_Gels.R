####ANOVA-and-Tukey-on-3D matrix####library(ggplot2); library(gridExtra)
setwd("/Users/Vogler/Desktop")
name = "ObjectDensityPerDayAndSample_ANOVA"
pdf(file= paste(name, ".pdf", sep=""), width=7, height=7 )

library(gridExtra);library(ggplot2); library(plyr); library("multcomp")
R <- read.table("/Users/Vogler/Desktop/3D_Culture_ANOVA_Test.csv", header=TRUE, sep=",")
R1<-subset(R, R[, "Parameters"] %in% c("0RGD0MMP","WithRGD0MMP", "0RGDWithMMP", "WithRGDWithMMP"))
ggplot(R1, aes(x=Object.Number, fill=Probe))+geom_density(alpha=.3)
aov.out <- aov(Object.Number ~ Probe*DIV2+Error(ID/DIV2), data=R1)
res <- summary(aov.out)
capture.output(res, file=paste(name, "_pValues.txt", sep=""))
cat("\n",file=paste(name, "_pValues.txt", sep=""), append = TRUE)
input = "#######New ANOVA withour Error_correction###################################################"
capture.output(input,file=paste(name, "_pValues.txt", sep=""), append=TRUE )
#aov.out2 <- aov(Object.Number ~ Probe*DIV2, data=R1)
aov.out2 <- aov(Object.Number ~ Probe*DIV2+Error(ID/DIV2), data=R1)
res2 <- summary(aov.out2)
capture.output(res2,file=paste(name, "_pValues.txt", sep=""), append=TRUE )
cat("\n",file=paste(name, "_pValues.txt", sep=""), append = TRUE)
input2 = "#######TUKEY after ANOVA withour Error_correction###################################################"
capture.output(input2,file=paste(name, "_pValues.txt", sep=""), append=TRUE )
tuk <- TukeyHSD(aov.out2)
capture.output(tuk,file=paste(name, "_pValues.txt", sep=""), append=TRUE )
library(car)
qqPlot(lm(Object.Number ~ Probe, data=R), simulate=TRUE, main="Q-Q Plot (normal distributed?)", labels=FALSE)


dev.off()


