#upset的使用说明（以水母保守lncRNA为例）

## 1.导入数据
setwd('C:/Users/83617/Desktop/04.upset/')

Alatina_alata_lncRNA_id <- read_table("Alatina_alata_lncRNA_id.txt", 
                                       col_names = FALSE)
Aurelia_aurita_lncRNA_id <- read_table("Aurelia_aurita_lncRNA_id.txt", 
                                       col_names = FALSE)
Calvadosia_cruxmelitensis_lncRNA_id <- read_table("Calvadosia_cruxmelitensis_lncRNA_id.txt", 
                                       col_names = FALSE)
Cassiopea_xamachana_lncRNA_id <- read_table("Cassiopea_xamachana_lncRNA_id.txt", 
                                       col_names = FALSE)
Chrysaora_quinquecirrha_lncRNA_id <- read_table("Chrysaora_quinquecirrha_lncRNA_id.txt", 
                                       col_names = FALSE)
Clytia_hemisphaerica_lncRNA_id <- read_table("Clytia_hemisphaerica_lncRNA_id.txt", 
                                       col_names = FALSE)
Haliclystus_octoradiatus_lncRNA_id <- read_table("Haliclystus_octoradiatus_lncRNA_id.txt", 
                                       col_names = FALSE)
Morbakka_virulenta_lncRNA_id <- read_table("Morbakka_virulenta_lncRNA_id.txt", 
                                       col_names = FALSE)
Nemopilema_nomurai_lncRNA_id <- read_table("Nemopilema_nomurai_lncRNA_id.txt", 
                                       col_names = FALSE)
Rhopilema_esculentum_lncRNA_id <- read_table("Rhopilema_esculentum_lncRNA_id.txt", 
                                       col_names = FALSE)
Sanderia_malayensis_lncRNA_id <- read_table("Sanderia_malayensis_lncRNA_id.txt", 
                                      col_names = FALSE)
Turritopsis_rubra_lncRNA_id <- read_table("Turritopsis_rubra_lncRNA_id.txt", 
                                       col_names = FALSE)
Turritopsis_dohrnii_lncRNA_id <- read_table("Turritopsis_dohrnii_lncRNA_id.txt", 
                                            col_names = FALSE)
Alatina_alata <- Alatina_alata_lncRNA_id$X1
Aurelia_aurita <- Aurelia_aurita_lncRNA_id$X1
Calvadosia_cruxmelitensis <- Calvadosia_cruxmelitensis_lncRNA_id$X1
Cassiopea_xamachana <- Cassiopea_xamachana_lncRNA_id$X1
Chrysaora_quinquecirrha <- Chrysaora_quinquecirrha_lncRNA_id$X1
Clytia_hemisphaerica <- Clytia_hemisphaerica_lncRNA_id$X1
Haliclystus_octoradiatus <- Haliclystus_octoradiatus_lncRNA_id$X1
Morbakka_virulenta <- Morbakka_virulenta_lncRNA_id$X1
Nemopilema_nomurai <- Nemopilema_nomurai_lncRNA_id$X1
Rhopilema_esculentum <- Rhopilema_esculentum_lncRNA_id$X1
Sanderia_malayensis <- Sanderia_malayensis_lncRNA_id$X1
Turritopsis_rubra <- Turritopsis_rubra_lncRNA_id$X1
Turritopsis_dohrnii <- Turritopsis_dohrnii_lncRNA_id$X1

vector_list <- list(Alatina_alata,Aurelia_aurita,Calvadosia_cruxmelitensis,Cassiopea_xamachana,
                    Chrysaora_quinquecirrha,Clytia_hemisphaerica,Haliclystus_octoradiatus,Morbakka_virulenta,
                    Nemopilema_nomurai,Rhopilema_esculentum,Sanderia_malayensis,Turritopsis_rubra,Turritopsis_dohrnii)

common_elements_intersect <- Reduce(intersect, vector_list)

common_elements_union <- Reduce(union, vector_list)

data_1 <- as.data.frame(common_elements_union)
data_1[, 2] <- ifelse(data_1[, 1] %in% Alatina_alata, 1, 0)
data_1[, 3] <- ifelse(data_1[, 1] %in% Aurelia_aurita, 1, 0)
data_1[, 4] <- ifelse(data_1[, 1] %in% Calvadosia_cruxmelitensis, 1, 0)
data_1[, 5] <- ifelse(data_1[, 1] %in% Cassiopea_xamachana, 1, 0)
data_1[, 6] <- ifelse(data_1[, 1] %in% Chrysaora_quinquecirrha, 1, 0)
data_1[, 7] <- ifelse(data_1[, 1] %in% Clytia_hemisphaerica, 1, 0)
data_1[, 8] <- ifelse(data_1[, 1] %in% Haliclystus_octoradiatus, 1, 0)
data_1[, 9] <- ifelse(data_1[, 1] %in% Morbakka_virulenta, 1, 0)
data_1[, 10] <- ifelse(data_1[, 1] %in% Nemopilema_nomurai, 1, 0)
data_1[, 11] <- ifelse(data_1[, 1] %in% Rhopilema_esculentum, 1, 0)
data_1[, 12] <- ifelse(data_1[, 1] %in% Sanderia_malayensis, 1, 0)
data_1[, 13] <- ifelse(data_1[, 1] %in% Turritopsis_rubra, 1, 0)
data_1[, 14] <- ifelse(data_1[, 1] %in% Turritopsis_dohrnii, 1, 0)

species_vector <- c(
  "Alatina_alata",
  "Aurelia_aurita",
  "Calvadosia_cruxmelitensis",
  "Cassiopea_xamachana",
  "Chrysaora_quinquecirrha",
  "Clytia_hemisphaerica",
  "Haliclystus_octoradiatus",
  "Morbakka_virulenta",
  "Nemopilema_nomurai",
  "Rhopilema_esculentum",
  "Sanderia_malayensis",
  "Turritopsis_rubra",
  "Turritopsis_dohrnii"
)
species_vector_1 <- gsub("_", " ", species_vector)

queries<-list(list(query=intersects,
                   params=list(species_vector_1),#指定颜色（指定分组交集）
                   active=T,
                   color="red"))

colnames(data_1) <-c("lncRNA_id",species_vector_1) 
pdf("plot.pdf", height = 7, width = 16)
upset(data_1,nsets =13,nintersects=30,order.by = c("freq"),
      decreasing = c(TRUE,FALSE),queries = queries,
      keep.order = TRUE
)
dev.off()
