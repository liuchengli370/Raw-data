
library(rms)
library(pROC)
library(RColorBrewer)
group <- read.csv("group.csv",header=T,row.names = 1,check.names = F,stringsAsFactors = F)
exp <- read.csv("CAMK4.csv",header=T,row.names = 1,check.names = F,stringsAsFactors = F)

exp <-as.data.frame(exp)
f_lrm <-lrm(group ~ ., data=exp, x=TRUE, y=TRUE, maxit=1000)

ddist <- datadist(exp)
options(datadist='ddist')

nomogram <- nomogram(f_lrm,fun=function(x)1/(1+exp(-x)), 
                     fun.at = c(0.1,0.5,0.9),
                     funlabel = "Probability of Case", 
                     lp=F,  
                     conf.int = F, 
                     abbrev = F
)

pdf(file="01_nomogram.pdf" ,width=10,height=7) 

plot(nomogram, lplabel = "Linear Predictor", xfrac = 0.25, varname.label = TRUE, varname.label.sep = "=",
     ia.space = 0.2, tck = NA, tcl = -0.2, lmgp = 0.3, points.label = "Points", total.points.label = "Total Points",
     total.sep.page = FALSE, cap.labels = FALSE, cex.var = 0.6, cex.axis = 0.6, lwd = 5,
     label.every = 1, col.grid = gray(c(0.8, 0.95)))

dev.off()

exp_logic <- data.frame(group = group, exp)
f_lrm <-lrm(group ~ ., data=exp_logic, x=TRUE, y=TRUE, maxit=1000)

set.seed(123)
cal <- calibrate(f_lrm)
pdf(file="03_calibrate.pdf",width=5,height=5)
par(mgp = c(2.5, 1, 0), mar = c(5.5, 4, 1, 1))
plot(cal)
dev.off()

prob <-  predict(f_lrm, exp_logic[, -1])
pred_LR <- data.frame(prob, group = group, stringsAsFactors = F)

pdf(file="02_roc.pdf" ,width=4.5,height=4.5) 
LR.roc <- plot.roc(pred_LR$group,pred_LR$prob,ylim=c(0,1),xlim=c(1,0),
                   smooth=F,
                   ci=TRUE,
                   main="",
                   lwd=2, 
                   legacy.axes=T,
                   print.auc.col="red",
                   print.auc.x=0.7,
                   print.auc.y=0.3,
                   print.auc = T)
dev.off()

