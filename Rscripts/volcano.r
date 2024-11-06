
library(ggplot2)
data <- read.csv("volcano.csv",row.names = 1)
data$label <- c(rownames(data)[1:10],rep(NA,(nrow(data)-10)))
ggplot(data,aes(logFC, -log10(adj.P.Val)))+
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "#999999")+
  geom_vline(xintercept = c(-1,1), linetype = "dashed", color = "#999999")+
  geom_point(aes(size=-log10(adj.P.Val), color= -log10(adj.P.Val)))+
  scale_color_gradientn(values = seq(0,1,0.2),
                        colors = c("#39489f","#39bbec","#f9ed36","#f38466","#b81f25"))+
  scale_size_continuous(range = c(1,3))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = c(0.01,0.7),
        legend.justification = c(0,1)
  )+
  guides(col = guide_colourbar(title = "-Log10_adj.P.Val"),
         size = "none")+
  geom_text(aes(label=label, color = -log10(adj.P.Val)), size = 3, vjust = 1.5, hjust=1)+
  xlab("Log2FC")+
  ylab("-Log10(adj.P.Val)")

ggsave("volcano.pdf", height = 10, width = 12)