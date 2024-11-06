
library(psych)
library(stringr)
library(pheatmap)
df<-read.csv("result.csv",header=T,row.names = 1,check.names = F,stringsAsFactors = F)
df2<-read.csv("biomarker.csv",header=T,row.names = 1,check.names = F,stringsAsFactors = F)
a<-corr.test(df,df2,method = "spearman",adjust = "fdr")
cmt<-a$r
pmt<-a$p
if (!is.null(pmt)){
  ssmt <- pmt< 0.01
  pmt[ssmt] <-'**'
  smt <- pmt >0.01& pmt <0.05
  pmt[smt] <- '*'
  pmt[!ssmt&!smt]<- ''
} else {
  pmt <- F
}
a<-pheatmap(cmt,scale = "none",cluster_row = F, cluster_col = F, border=NA,
            display_numbers = pmt,fontsize_number = 12, number_color = "black",
            cellwidth = 20, cellheight =20)

pdf('corheatmap.pdf',width = 8,height = 8)
a
dev.off()