
library(readr)
FPKM <- read_delim("FPKM.matrix", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

selected_rows_1 <- FPKM[FPKM$gene_id == "ENSG00000147257.16", ]

selected_rows_2 <- FPKM[FPKM$gene_id == "ENSG00000167244.22", ]

selected_rows_3 <- FPKM[FPKM$gene_id == "ENSG00000141736.14", ]

selected_rows_4 <- FPKM[FPKM$gene_id == "ENSG00000105894.13", ]

selected_rows_5 <- FPKM[FPKM$gene_id == "ENSG00000106483.12", ]

merged_df <- rbind(selected_rows_1, selected_rows_2,selected_rows_3,selected_rows_4,selected_rows_5)

df_transposed <- t(merged_df)
df_transposed <- as.data.frame(df_transposed, stringsAsFactors = FALSE)
colnames(df_transposed) <- df_transposed[1, ]
df_transposed <- df_transposed[-1, ]
a<- rep(c("BA","Normal","BA"),times = c(121,7,50))
df_transposed$type <- a

ggplot(df_transposed, aes(type,ENSG00000147257.16)) + 
  geom_violin(trim = FALSE, position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_bw(base_size = 15) 

is.data.frame(df_transposed)
head(df_transposed)

df_transposed$ENSG00000147257.16 <- as.numeric(df_transposed$ENSG00000147257.16)
df_transposed$ENSG00000167244.22 <- as.numeric(df_transposed$ENSG00000167244.22)
df_transposed$ENSG00000141736.14 <- as.numeric(df_transposed$ENSG00000141736.14)
df_transposed$ENSG00000105894.13 <- as.numeric(df_transposed$ENSG00000105894.13)
df_transposed$ENSG00000106483.12 <- as.numeric(df_transposed$ENSG00000106483.12)

e <- ggplot(df_transposed, aes(x = type, y = df_transposed$ENSG00000106483.12))

e + geom_violin()
e + geom_violin(aes(fill = type), trim = FALSE)+geom_boxplot(width = 0.2)

library(ggpubr)

my_comparisons=list(c("BA","Normal"))

mytheme <- theme(axis.title.x=element_text(face="italic", color="black",size=20,family="serif"),
                 axis.title.y = element_text(colour="black",size=20,face = "bold",family="serif",),
                 axis.text.x = element_text(colour="black",size=16,face="bold",family="serif"),
                 axis.text.y = element_text(colour="black",size=16,face="bold",family="serif"))

p1 <- ggviolin(df_transposed, x="type", y="ENSG00000147257.16", fill = "type", 
         palette = c('lancet'))+ stat_compare_means(comparisons = my_comparisons,label = "p.signif")+labs(y = 'FPKM', x = "GPC3")+mytheme

p2 <- ggviolin(df_transposed, x="type", y="ENSG00000167244.22", fill = "type", 
         palette = c('lancet'))+ stat_compare_means(comparisons = my_comparisons,label = "p.signif")+labs(y = 'FPKM', x = "IGF2")+mytheme

p3 <- ggviolin(df_transposed, x="type", y="ENSG00000141736.14", fill = "type", 
         palette = c('lancet'))+ stat_compare_means(comparisons = my_comparisons,label = "p.signif")+labs(y = 'FPKM', x = "ERBB2")+mytheme

p4 <- ggviolin(df_transposed, x="type", y="ENSG00000105894.13", fill = "type", 
         palette = c('lancet'))+ stat_compare_means(comparisons = my_comparisons,label = "p.signif")+labs(y = 'FPKM', x = "PTN")+mytheme

p5 <- ggviolin(df_transposed, x="type", y="ENSG00000106483.12", fill = "type", 
         palette = c('lancet'))+ stat_compare_means(comparisons = my_comparisons,label = "p.signif")+labs(y = 'FPKM', x = "SFRP4")+mytheme


p_A <- p1+p2+p3+p4+p5

p_A
ggsave('test.jpg', p_A, device = "jpg", dpi = 300, 
       width=16, height=8, unit = "in")



