
library(pheatmap)
A <- read.csv("pheatmap.csv",header = T,row.names = 1)
annotation_col = data.frame(group = c(rep("Tumor",369),rep("Control",50)))
row.names(annotation_col) <- colnames(A)
A<-scale(A,center=TRUE, scale=FALSE)
#A<-log2(A+1)
bk <- c(seq(-0.5,-0.1,by=0.01),seq(0,0.5,by=0.01))
a<-pheatmap(A,cluster_rows = T,cluster_cols = F,
            color=colorRampPalette(c("navy","white","firebrick3"))(100),
            show_colnames = F ,border_color = NA,scale = "row",show_rownames =T,
            annotation_col = annotation_col,breaks = bk)
pdf('pheatmap.pdf',width = 6,height = 6)
a
dev.off()
