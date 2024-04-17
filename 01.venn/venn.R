library('ggvenn')
rm(list = ls())

library(readr)
cpc <- read_table("C:/Users/83617/Desktop/PNAS_lncRNA/01.venn/01.software_prediction/cpc.list", 
                  col_names = FALSE)

CNCI_noncoding <- read_table("C:/Users/83617/Desktop/PNAS_lncRNA/01.venn/01.software_prediction/CNCI_noncoding.list", 
                  col_names = FALSE)

PLEK <- read_table("C:/Users/83617/Desktop/PNAS_lncRNA/01.venn/01.software_prediction/PLEK.list", 
                             col_names = FALSE)

x <- list(
  CPC2 = cpc$X1,
  CNCI = CNCI_noncoding$X1,
  PLEK = PLEK$X1
)




#由于取交集数据大概率不会等长，需要使用list，然后作图
p1 <- ggvenn(x,columns = c('CPC2','CNCI','PLEK'),
             stroke_size = 0.5, fill_color =c("#DF7A5E",'#2A9D8E','#F3A261'))

p1

setwd('../Desktop/lncRNA_figure/')
getwd()
ggsave('venn_cpc_plek_cnci.jpg', p1, device = "jpg", dpi = 300, 
       width=8, height=8, unit = "in")





library('ggvenn')


x <- list(
  Nr = nr_out$X1,
  Pfam = pfam_coding$X1,
  Swiss_Prot = swiss_out$X1
)

#由于取交集数据大概率不会等长，需要使用list，然后作图
p1 <- ggvenn(x,columns = c('Nr','Pfam','Swiss_Prot'),
             stroke_size = 0.5, fill_color =c("#DF7A5E",'#2A9D8E','#F3A261'))


setwd('../Desktop/lncRNA_figure/')
getwd()
ggsave('venn_Nr_Pfam_Swiss.jpg', p1, device = "jpg", dpi = 300, 
       width=8, height=8, unit = "in")
