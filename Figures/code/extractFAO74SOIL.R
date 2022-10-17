library(GSIF)
library(scales)
library(ggpubr)
library(dplyr)
data(soil.legends)
#Soil classification: soil unit x P retention class #############################
soilclass
#1. main class, topclass, color
fao_legend <- read.csv("../extdata/FAO74_legend.csv", row.names = 1) %>%
  mutate(color = as.character(color)) 
nrow(fao_legend)


soil_comp <- fao_legend %>% 
  dplyr::filter(pH > 0) %>%
  mutate(P_SOL = factor(P_SOL, levels = c("Very Low","Low", "Moderate","High"))) %>% 
  mutate(topclass = as.factor(topclass))  %>%
  group_by(P_SOL, topclass) %>% 
  summarise(count = n()) %>% ungroup %>%
  group_by(P_SOL) %>% 
  mutate(pct = count / sum(count)) %>%
  arrange(P_SOL,-count) %>% as.data.frame()


class_count <- soil_comp %>% 
  group_by(P_SOL) %>% 
  summarise(n = sum(count))


bar_label <- soil_comp$topclass
bar_label[soil_comp$pct < 0.02] <- NA


legend <- fao_legend %>%
  dplyr::filter(name %in% levels(soil_comp$topclass)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

quartz()
ggplot(soil_comp, aes(x = P_SOL, y = pct, fill = topclass, group=pct)) + 
  xlab("Phosphorus Solubility Class\n") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label = bar_label), 
            position = position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$P_SOL, y=c(-0.02,-0.02,-0.02,-0.02), label=paste0("n = ",class_count$n)) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format(), trans = 'reverse') +
  scale_fill_manual(name = "FAO74 Soil Unit Group", values = legend$pal) +
  ggpubr::theme_pubr(legend = "right") +
  theme(text=element_text(size=15),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(face = "bold"))


#Soil classification: Maize LR Most probable soil x P retention class ##########



# Here I can use the trait table.


traits <- read.table("P_traits.txt", header = TRUE) %>%

#add WRB taxonomy generic name
  dplyr::left_join( dplyr::select(GSIF::soil.legends$TAXNWRB, 
                           Number, TAXGWRB = "Generic"),
             by = c(TAXNWRB = "Number")) %>%
#add USDA taxonomy generic name
  dplyr::left_join( dplyr::select(GSIF::soil.legends$TAXOUSDA, 
                           Number, TAXGUSDA = "Generic"),
             by = c(TAXOUSDA = "Number")) %>%
#add FAO74 group name
  dplyr::left_join( dplyr::select(fao_legend, 
                           key, P_SOL,topclass),
             by = c(FAO74 = "key")) 



# WRB plot 

soil_comp <- traits %>%
  group_by( TAXGWRB ) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count))  %>%
  arrange(pct) 




class_count <- soil_comp %>% 
  group_by(topclass) %>% 
  summarise(n = sum(count))

bar_label <- soil_comp$TAXGWRB
bar_label[soil_comp$pct < 0.01] <- NA
soil_pal <- soil.legends$TAXGWRB$COLOR[sort(unique(match(soil_comp$TAXGWRB, 
                                           soil.legends$TAXGWRB$Group)))]

quartz()
ggplot(soil_comp, aes(x = class100, y = pct, fill = TAXGWRB, group=pct)) + 
  ggtitle("Maize Landraces") + xlab("Phosphorus retention class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label = bar_label ), 
            position =position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$class100, y=c(-0.02,-0.02,-0.02,-0.02), label=paste0("n = ",class_count$n)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(name = "WRB class", values = soil_pal) +
  theme_pubr(legend = "right")


#USDA taxonomy
usda_legend <- GSIF::soil.legends$TAXOUSDA %>%
  group_by(Generic) %>%
  slice(1) %>%
  dplyr::select(usda_group = "Generic", usda_color = "COLOR")

soil_comp <- traits %>%
  group_by(class100, TAXGUSDA) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count)) %>%
  left_join(usda_class, by = c(TAXGUSDA ="Generic"))  %>%
  arrange(class100,-count) 

class_count <- soil_comp %>% 
  group_by(class100) %>% 
  summarise(n = sum(count))

legend <- fao_legend %>%
  filter(name %in% levels(FAO74_pixel$name)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

bar_label <- soil_comp$TAXGUSDA
bar_label[soil_comp$pct < 0.02] <- NA

soil_pal <- usda_class$COLOR[sort(match(unique(soil_comp$TAXGUSDA), usda_class$Generic))]

quartz()
ggplot(soil_comp, aes(x = class100, y = pct, fill = TAXGUSDA, group=pct)) + 
  ggtitle("Maize Landraces") + xlab("Phosphorus retention class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label = bar_label), 
            position = position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$class100, y=c(-0.02,-0.02,-0.02,-0.02), label=paste0("n = ",class_count$n)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(name = "USDA class", values= legend$pal) +
  theme_pubr(legend = "right")

# FAO74
soil_comp <- traits   %>%
  mutate(P_SOL = factor(P_SOL, levels = c("Very Low","Low", "Moderate","High"))) %>%
  mutate(topclass = as.factor(droplevels(topclass)))  %>%
  group_by(P_SOL, topclass) %>% 
  summarise(count = n()) %>% ungroup %>%
  group_by(P_SOL) %>% 
  mutate(pct = count / sum(count)) %>%
  arrange(P_SOL,-count) %>% as.data.frame()


# Sorghumm landraces
p <- read.csv("/Volumes/GoogleDrive/My\ Drive/repos/Phosphorus_GEA_paper/Figures/data/grassGEA_phenotypes.csv",row.names = 1 )
p$MU <- raster::extract(
  x = ISRIC2011$FAO74, 
  y = p [, c("lon", "lat")])
p$FAO74 <- ISRIC_AT$mapunit$SOIL1[match(p$MU,ISRIC_AT$mapunit$ID)]
p <- p %>%
  left_join(soilclass, by = "sol") %>% 
  mutate(P_SOL = factor(sol_class, levels = c("VL","Lo" ,"Mo","Hi"))) 
 levels(p$P_SOL) <-  c("Very Low","Low", "Moderate","High")

p <- p %>% left_join(fao_legend %>% dplyr::select(FAO74="key", topclass)) %>%
  dplyr::filter(!is.na(P_SOL))


soil_comp <- p   %>%
  dplyr::filter(sp == "Sorghum bicolor") %>%
  #mutate(P_SOL = factor(P_SOL, levels = c("Very Low","Low", "Moderate","High"))) %>%
  mutate(topclass = as.factor(topclass))  %>%
  group_by(P_SOL, topclass) %>% 
  summarise(count = n()) %>% ungroup %>%
  group_by(P_SOL) %>% 
  mutate(pct = count / sum(count)) %>%
  arrange(P_SOL,-count) %>% as.data.frame()

  

class_count <- soil_comp %>% 
  group_by(P_SOL) %>% 
  summarise(n = sum(count))

legend <- fao_legend %>%
  dplyr::filter(name %in% levels(soil_comp$topclass)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

bar_label <- soil_comp$topclass
bar_label[soil_comp$pct < 0.02] <- NA

quartz()
ggplot(soil_comp, aes(x = P_SOL, y = pct, fill = topclass, group=pct)) + 
  ggtitle("Africa/India Sorghum Landraces\n") +
  xlab("Soil P Solubility class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label = bar_label), 
            position = position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$P_SOL, y=c(-0.02,-0.02,-0.02,-0.02), label=paste0("n = ",class_count$n)) +
  #scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = "FAO74 Soil Unit Group", values = legend$pal) +
  ggpubr::theme_pubr(legend = "right") +
  theme(text=element_text(size=15),
        #axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(face = "bold"))






soil_comp <- p   %>%
  dplyr::filter(sp == "Zea mays") %>%
  #mutate(P_SOL = factor(P_SOL, levels = c("Very Low","Low", "Moderate","High"))) %>%
  mutate(topclass = as.factor(topclass))  %>%
  group_by(P_SOL, topclass) %>% 
  summarise(count = n()) %>% ungroup %>%
  group_by(P_SOL) %>% 
  mutate(pct = count / sum(count)) %>%
  arrange(P_SOL,-count) %>% as.data.frame()



class_count <- soil_comp %>% 
  group_by(P_SOL) %>% 
  summarise(n = sum(count))

legend <- fao_legend %>%
  dplyr::filter(name %in% levels(soil_comp$topclass)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

bar_label <- soil_comp$topclass
bar_label[soil_comp$pct < 0.02] <- NA


quartz()
ggplot(soil_comp, aes(x = P_SOL, y = pct, fill = topclass, group=pct)) + 
  ggtitle("LAC Maize Landraces\n") +
  xlab("Soil P Solubility class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label = bar_label), 
            position = position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$P_SOL, y=c(-0.02,-0.02,-0.02,-0.02), label=paste0("n = ",class_count$n)) +
  #scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = "FAO74 Soil Unit Group", values = legend$pal) +
  ggpubr::theme_pubr(legend = "right") +
  theme(text=element_text(size=15),
        #axis.title.x = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(face = "bold"))

quartz(width=14, height =7)
ggpubr::ggarrange(p_sb, p_zm)
  
  
# by Area
fao_legend <- read.csv("/Users/fvrodriguez/Desktop/FAO74legend.csv")
fao_legend$color <- as.character(fao_legend$color)
rgb2hex <- function(x) { rgb(x[1], x[2], x[3], maxColorValue = 255)}
fao_legend$color <- apply(fao_legend[,c("R","G","B")],1,rgb2hex)
write.csv(fao_legend,file="/Users/fvrodriguez/Desktop/FAO74legend.csv")

quartz()
barplot(rep(1,length(fao_legend$color)), col=fao_legend$color,horiz =TRUE)
?barplot
misc <- c("GL", "RK", "WR")

certain <- as.data.frame(freq(ISRIC2011$FAO74, merge=TRUE)) %>% 
  inner_join(ISRIC_AT$mapunit, by = c(value = "ID")) %>%
  mutate(class100 = case_when( Lo == 100 ~ "Lo",
                               Mo == 100 ~ "Mo",
                               Hi == 100 ~ "Hi",
                               VH == 100 ~ "VH")) %>%
  mutate(class100 = factor(class100, levels = c("Lo","Mo","Hi","VH")))


FAO74_pixel <- certain %>%
  filter(!is.na(class100)) %>%
  dplyr::select(value,SOIL1,count,class100) %>%
  left_join(fao_legend, by = c(SOIL1 = "key")) %>%
  filter(!(toclass100 %in% misc)) %>%
  group_by(class100, toclass100) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count / sum(count)) %>%
  left_join(fao_legend, by = c(toclass100 = "key")) %>%
  dplyr::select(-toclass100.y) %>% 
  arrange(class100,-count) 

FAO74_pixel$name <- droplevels(FAO74_pixel$name)
class_count <- FAO74_pixel %>% 
  group_by(class100) %>% 
  summarise(n = sum(count))


library(ggplot2)
library(ggpubr)

bar_name <- FAO74_pixel$name
bar_name[FAO74_pixel$pct <0.01] <- NA

soil <- fao_legend %>%
  filter(name %in% levels(FAO74_pixel$name)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

soil$lab <- factor( soil$lab,levels = sort(unique(soil$lab)))

quartz()
ggplot(FAO74_pixel, aes(x = class100, y = pct, fill = name, group=pct)) + 
  ggtitle("World soils by area (thousands of pixels)") + xlab("Phosphorus retention class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label =  bar_name), 
            position =position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$class100, y=c(-0.02,-0.02,-0.02,-0.02), 
           label= paste0("n = ",round(class_count$n/1000,0))) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(name = "FAO74 class", values = soil$pal) +
  theme_pubr(legend = "right")

# Maize landraces

ISRIC_P$georef$MU <- raster::extract(
  x = ISRIC2011$FAO74, 
  y = ISRIC_P$georef[, c("locations_longitude", "locations_latitude")])

ISRIC_P$georef$FAO74 <- ISRIC_AT$mapunit$SOIL1[match(ISRIC_P$georef$MU,ISRIC_AT$mapunit$ID)]
certain <-NULL
certain <- ISRIC_P$georef %>%   
  mutate(class100 = case_when(Lo == 100 ~ "Lo",
                              Mo == 100 ~ "Mo",
                              Hi == 100 ~ "Hi",
                              VH == 100 ~ "VH")) %>%
  mutate(class100 = factor(class100, levels = c("Lo","Mo","Hi","VH"))) %>%
  filter(!is.na(class100))



certain <-NULL
certain <- ISRIC_P$georef %>%   
  mutate(class100 = case_when(Lo == 100 ~ "Lo",
                              Mo == 100 ~ "Mo",
                              Hi == 100 ~ "Hi",
                              VH == 100 ~ "VH")) %>%
  mutate(class100 = factor(class100, levels = c("Lo","Mo","Hi","VH"))) %>%
  filter(!is.na(class100))


p %>%
  left_join(fao_legend, by = c(FAO74 = "key"))

maize_FAO74 <- certain %>%
  left_join(fao_legend, by = c(FAO74 = "key")) %>%
  filter(!(toclass100 %in% misc)) %>%
  dplyr::select(id,class100,toclass100) %>%
  group_by(class100, toclass100) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count)) %>%
  left_join(fao_legend, by = c(toclass100 = "key")) %>%
  dplyr::select(-toclass100.y) %>% 
  arrange(class100,-count)

maize_FAO74$name <- droplevels(maize_FAO74$name)
class_count <- maize_FAO74 %>% 
  group_by(class100) %>% 
  summarise(n = sum(count))

bar_name <- maize_FAO74$name
bar_name[maize_FAO74$pct <0.01] <- NA

soil <- fao_legend %>%
  filter(name %in% levels(maize_FAO74$name)) %>%
  dplyr::select(lab = "name", pal ="color") %>%
  arrange(lab)

soil$lab <- factor( soil$lab,levels = sort(unique(soil$lab)))

quartz()
ggplot(maize_FAO74, aes(x = class100, y = pct, fill = name, group=pct)) + 
  ggtitle("Maize Landraces (Romero-Navarro 2017)") + xlab("Phosphorus retention class") +
  ylab("Frequency") +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(aes(label =  bar_name), 
            position =position_stack(vjust = .5)) +
  annotate(geom="text", x=class_count$class100, y=c(-0.02,-0.02,-0.02,-0.02), 
           label= paste0("n = ",class_count$n)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(name = "FAO74 class", values = soil$pal) +
  theme_pubr(legend = "right")
