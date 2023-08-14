rm(list = ls())
setwd('E:/R/alpha_diversity/jianshu/')
getwd()
setwd('C:/Users/Admin/Desktop/2023_taishi_data_temp/temp/2022_10_qiime/')
setwd('C:/Users/Admin/Desktop/2023_taishi_data_temp/temp/12s_R/')
library(MicrobiotaProcess)
library(phyloseq)
library(tidyverse)
library(RColorBrewer)
otu <- "otu_table.qza"
rep <- "rep-seqs.qza"
tree <- "rooted-tree.qza"
tax <- "taxonomy.qza"
sample <- "group_bake.txt"
#######UPGMA 层级聚类分析
mpse2 <- mp_import_qiime2(otuqza=otu, taxaqza=tax, mapfilename=sample)
mpse2
mpse2 %<>% 
mp_decostand(.abundance=Abundance)
mpse2 %<>% mp_cal_dist(.abundance=hellinger, distmethod="bray")
mpse2 %<>% 
  mp_cal_pcoa(.abundance=hellinger, distmethod="bray")
mpse2
mpse2 %<>%
  mp_adonis(.abundance=hellinger, .formula=~group, distmethod="bray", permutations=9999, action="add")
mpse2 %>% mp_extract_internal_attr(name=adonis)

2023_taishi_data_temp


p1 <- mpse2 %>%
  mp_plot_ord(
    .ord = pcoa, 
    .group = group, 
    .color = group, 
    .size = 1.2,
    .alpha = 1,
    ellipse = TRUE,
    show.legend = FALSE # don't display the legend of stat_ellipse
  ) 

p1

mpse2 %<>%
  mp_cal_clust(
    .abundance = hellinger, 
    distmethod = "bray",
    hclustmethod = "average", # (UPGAE)
    action = "add" # action is used to control which result will be returned
  )
mpse2


sample.clust <- mpse2 %>% mp_extract_internal_attr(name='SampleClust')
sample.clust

library(ggtree)
p <- ggtree(sample.clust) + 
  geom_tippoint(aes(color=group),size=3) +
  geom_tiplab(as_ylab = TRUE) +
  ggplot2::scale_x_continuous(expand=c(0, 0.01))
p
pp <- p+theme(axis.title.x=element_text(face="bold", color="black",size=20,family="serif"),
                                                           axis.title.y = element_text(colour="black",size=20,face="bold",family="serif"),
                                                           axis.text.y = element_text(colour="black",size=16,face="bold",family="serif"),
                                                           legend.text = element_text(colour="black",size=12,face="bold",family="serif"),
                                                           legend.title = element_text(colour="black",size=16,face="bold",family="serif"))
pp

ggsave('UPGMA.jpg', pp, device = "jpg", dpi = 300, 
       width=12, height=8, unit = "in")










rm(list=ls())
