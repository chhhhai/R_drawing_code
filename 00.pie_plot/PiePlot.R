rm(list = ls())
library(readr)
final_lncRNA_classes <- read_delim("C:/Users/83617/Desktop/PNAS_lncRNA/final_lncRNA_classes.txt", 
                                   delim = "\t", escape_double = FALSE, 
                                   trim_ws = TRUE)

genic <- final_lncRNA_classes %>% filter(type=="genic")

intergenic <- final_lncRNA_classes %>% filter(type=="intergenic")

genic_sense_exonic  <- genic  %>% filter(direction=="sense",location=="exonic")

genic_sense_intronic <- genic  %>% filter(direction=="sense",location=="intronic")

genic_antisense_exonic <- genic  %>% filter(direction=="antisense",location=="exonic")

genic_antisense_intronic <- genic  %>% filter(direction=="antisense",location=="intronic")
nrow(genic)
a=nrow(intergenic)/nrow(final_lncRNA_classes)
b=nrow(genic_sense_intronic)/nrow(final_lncRNA_classes)
c=nrow(genic_antisense_exonic)/nrow(final_lncRNA_classes)
d=nrow(genic_antisense_intronic)/nrow(final_lncRNA_classes)

a+b+c+d

mycols <- c( "#CC011F", "#FADADD","#074166", "#8CC5BE")

lncRNA_classfication <- data.frame(type=c("Intergenic","Sense intronic","Antisense exonic","Antisense intronic"),
                                   proportion=c(a,b,c,d))

label_value <- paste('(',round(lncRNA_classfication$proportion*100, 2),'%)',sep = '')

label <- paste(lncRNA_classfication$type, label_value,sep = " ")
lncRNA_classfication$type <- label

plot <- ggplot(lncRNA_classfication, aes(x = "", y = proportion, fill = type)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        strip.text = element_text(size = 16, face = "italic"), legend.text = element_text(size = 16), legend.title = element_blank())
plot
