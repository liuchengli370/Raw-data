
library("ROCR")
library("pROC")
library("ggplot2")
data=read.csv("roc_test.csv", header=T,  check.names=F, row.names=1)
rocobj <- roc(group ~ ., data = data)
rocobj_legend <- c()
for (l in 1:length(rocobj)) {
  auc <- auc(rocobj[[l]])
  legend <- paste0(colnames(data)[l+1],"_AUC: ",round(auc,3))
  rocobj_legend <- c(rocobj_legend,legend)
}
rocobj_legend


pdf(file = paste0("roc_test", ".pdf"), height = 6, width = 6, onefile = FALSE)
ggroc(rocobj,size = 1.1,legacy.axes=T) + 
  labs(x = "False positiverate", y = "True positiverate",)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed",size = 1)+
  theme(legend.title=element_blank(),
        legend.position = c(0.7,0.25),
        legend.text = element_text(size = 10, margin = margin (b = 8, unit = "pt")),
        text = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.title = element_text(size = 10, hjust = 0.5, colour = "black"),
        axis.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA, size = 1.5))+
  scale_color_discrete(breaks = colnames(data)[-1],labels = rocobj_legend)
dev.off()