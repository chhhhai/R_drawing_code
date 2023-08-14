
library('Rsubread')

paste0("SRR10744",251:439)

rm(list=ls())
setwd('C:/Users/Admin/Desktop/featurecounts/')
getwd()
library(DESeq2)
data <- read.table(file = 'C:/Users/Admin/Desktop/featurecounts/counts_1.txt',header = TRUE, row.names = 1,encoding="UTF-8")
sample<-names(data)
# 设置分组信息并构建dds对象，按照geuvadis_phenodata.table给的信息写
condition <- factor(c(rep("BA", 121), rep("Normal", 7), rep("BA", 50)),levels=c("BA","Normal"))
coldata <- data.frame(row.names = colnames(data), condition)
dds <- DESeqDataSetFromMatrix(countData=data, colData=coldata, design=~condition)
dds <- dds[ rowSums(counts(dds)) > 1, ]
# 使用DESeq函数估计离散度，然后差异分析获得res对象
dds_norm <- DESeq(dds)
res1 <- results(dds_norm,contrast=c("condition","BA","Normal")) #计算两组间差异

# 最后设定阈值，筛选差异基因，导出数据(全部数据。包括标准化后的count数)
res1 <- res1[order(res1$padj),] #排序
resdata1 <- merge(as.data.frame(res1), as.data.frame(counts(dds_norm, normalized=TRUE)), by="row.names", sort=FALSE)


deseq2_merge<-rbind(data.frame(group="BA_Normal",resdata1))
write.table(deseq2_merge,file="deseq2_merge.table",row.names=F,quote=F,sep="\t")   #保存未过滤的所有基因结果，方便在后期筛选差异表达基因
print("============== Write diff done  ==============")

diff_gene1 <- subset(res1, padj < 0.05 & (log2FoldChange > 2 | log2FoldChange < -2)) #筛选差异基因，也可在linux筛选

diff1 <- row.names(diff_gene1)   #差异基因ID

up_DEG1 <- subset(resdata1, padj < 0.05 & log2FoldChange > 2)

down_DEG1 <- subset(resdata1, padj < 0.05 & log2FoldChange < -2)

diff_merge<-rbind(data.frame(group="BA_Normal_up",up_DEG1),data.frame(group="BA_Normal_down",down_DEG1))
write.table(diff_merge,file="diff_merge.table",row.names=F,quote=F,sep="\t")
