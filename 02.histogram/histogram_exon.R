#1.数据预处理
rm(list = ls())

setwd("C:/Users/83617/Desktop/PNAS_lncRNA/lncRNA_feature")

mrna_exon <- read_table("mrna_exon.txt",col_names = FALSE)

lncrna_exon <- read_table("lncrna_exon.txt")


mrna_exon_num <- dplyr::count(mrna_exon,X2)

lncRNA_exon_num <- dplyr::count(lncrna_exon,num_exons)


A <- mrna_exon_num %>% mutate(type="mRNA")
colnames(A)=c("id","num","type")
A %>% filter(id > 20) %>%  summarise(total_num = sum(num))
A1 <-  A %>% filter(id <= 20) 
A1$id <- as.character(A1$id)
test_1 <- add_row(A1,id=">20",num=2030,type="mRNA")


B <- lncRNA_exon_num %>% mutate(type="lncRNA")
colnames(B)=c("id","num","type")
B$id <- as.character(B$id)
test_2 <- add_row(B,id="16",num=0,type="lncRNA")
test_2 <- add_row(test_2,id="17",num=0,type="lncRNA")
test_2 <- add_row(test_2,id="18",num=0,type="lncRNA")
test_2 <- add_row(test_2,id="19",num=0,type="lncRNA")
test_2 <- add_row(test_2,id="20",num=0,type="lncRNA")
test_2 <- add_row(test_2,id=">20",num=0,type="lncRNA")
new_row <- data.frame(
  id = "1",
  num = 0,
  type="lncRNA"
)
test_3 <- rbind(new_row, test_2)

lncRNA_mRNA_exon_num <- rbind(test_1,test_3)

#绘图

lncRNA_mRNA_exon_num$id <- factor(lncRNA_mRNA_exon_num$id,levels =c(seq(1,20),'>20'))
q1<-ggplot(data=lncRNA_mRNA_exon_num, mapping=aes(x = id, y = num,fill=type))+
  geom_bar(stat="identity",position=position_dodge(0.75))+
  scale_fill_manual(values = c("#E73847","#1D3557" ))+
  labs(fill = "")
q1
p1 <- q1+labs(x = 'Exon numbers', y = "Counts")+
  theme_bw()+theme(panel.grid=element_blank())+theme(axis.title.x=element_text(face="bold", color="black",size=16,family="serif"),
                                                     axis.title.y = element_text(colour="black",size=16,face="bold",family="serif"),
                                                     axis.text.x = element_text(colour="black",size=12,face="bold",family="serif"),
                                                     axis.text.y = element_text(colour="black",size=16,face="bold",family="serif"),
                                                     legend.text = element_text(colour="black",size=12,face="bold",family="serif"),
                                                     legend.title = element_text(colour="black",size=16,face="bold",family="serif"))
p1
ggsave('lncrna_mrna_exon.jpg', p1, device = "jpg", dpi = 300, width=10, height=8, unit = "in")
