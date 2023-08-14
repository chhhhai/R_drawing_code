rm(list = ls())

library(parallel) #并行计算  parApply parLapply parSaplly 
cl <- makeCluster(0.75*detectCores())  #设计启用计算机3/4的核
getwd()
## 利用GenomicFeatures包导入gtf处理
library(GenomicFeatures)

txdb <- makeTxDbFromGFF("./gencode.v42.annotation.gtf.gz",
                        format="gtf") 
exons_gene <- exonsBy(txdb, by = "gene") ###提取基因外显子
head(exons_gene)
##计算总外显子长度：用reduce去除掉重叠冗余的部分，,width统计长度，最后计算总长度
exons_gene_lens <- parLapply(cl,exons_gene,function(x){sum(width(reduce(x)))}) 
exons_gene_lens[1:10]

##转换为dataframe
geneid_efflen <- data.frame(geneid=names(exons_gene_lens),
                            efflen=as.numeric(exons_gene_lens))
write.csv(geneid_efflen,file = "./gene_length.csv",row.names = F)

data <- read.table(file = 'C:/Users/Admin/Desktop/featurecounts/counts_1.txt',header = TRUE, row.names = 1,encoding="UTF-8")

## 3.1 加载基因长度信息##############
library(data.table)

gene_length = fread("./gene_length.csv",data.table = F)

index = intersect(gene_length$geneid,row.names(data))

gene_length = gene_length[gene_length$geneid%in%index,]
data = data[gene_length$geneid,]

identical(gene_length$geneid,row.names(data)) #[1] TRUE

#FPKM/RPKM (Fragments/Reads Per Kilobase Million ) 每千个碱基的转录每百万映射读取的Fragments/reads
#RPKM与FPKM分别针对单端与双端测序而言，计算公式是一样的
counts2FPKM <- function(count=count, efflength=efflen){    
  PMSC_counts <- sum(count)/1e6   #counts的每百万缩放因子 (“per million” scaling factor) 深度标准化  
  FPM <- count/PMSC_counts        #每百万reads/Fragments (Reads/Fragments Per Million) 长度标准化  
  FPM/(efflength/1000)
}

countToFpkm <- function(counts, effLen)
{
  N <- colSums(counts)
  exp( log(counts) + log(1e9) - log(effLen) - log(N) )
}

fpkm = counts2FPKM(data, gene_length$efflen)

N <- colSums(data)

write.table(fpkm,file="fpkm.table",row.names=T,quote=F,sep="\t")


data_fpkm = counts2FPKM(count = data,
                        efflength = gene_length$efflen)
# 取log2
data_fpkm_log2 = log2(data_fpkm+1)