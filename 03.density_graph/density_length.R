rm(list = ls())

library(readr)
lncrna_length <- read_table("lncrna_length.txt")
mrna_length <- read_table("mrna_length.txt", 
                          col_names = FALSE)

A <- mrna_length[,2:3]
colnames(A) <- c("type","len")

B <- lncrna_length %>% mutate(type="lncRNA") %>% select(type,len)

lncRNA_mRNA_length <-rbind(A,B)

q1 <- ggplot(lncRNA_mRNA_length,aes(x=len,group=type,fill=type))+
  geom_density(alpha=0.7,adjust=1.5)+
  scale_fill_manual(values = c("#E73847","#1D3557" ))+
  labs(fill = "")+
  scale_x_continuous(limits = c(0, 90000))
p1 <- q1+labs(x = 'Length', y = "Density")+
  theme_bw()+theme(panel.grid=element_blank())+theme(axis.title.x=element_text(face="bold", color="black",size=16,family="serif"),
                                                     axis.title.y = element_text(colour="black",size=16,face="bold",family="serif"),
                                                     axis.text.x = element_text(colour="black",size=12,face="bold",family="serif"),
                                                     axis.text.y = element_text(colour="black",size=16,face="bold",family="serif"),
                                                     legend.text = element_text(colour="black",size=12,face="bold",family="serif"),
                                                     legend.title = element_text(colour="black",size=16,face="bold",family="serif"))
p1

ggsave('lncrna_mrna_length.jpg', p1, device = "jpg", dpi = 300, width=10, height=8, unit = "in")