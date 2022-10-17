require(FactoMineR)
require(factoextra)
require(ggplot2)
require(tidyr)
require(dplyr)
require(MASS)
require(reshape2)
require(cowplot)

traits <- read.csv("../data/grassGEA_phenotypes.csv", row.names = 1)

colnames(traits)
dat <- traits[4:ncol(traits)]

# center and scale the data
for (i in 1:length(colnames(dat))){
  
  if (is.numeric(dat[, i])==TRUE)
    
    dat[, i] <- as.numeric(scale(dat[, i]))
  
  else
    
    dat[, i] <- dat[, i]
  
}
head(dat)

pca1 <- PCA(dat)

pca1$eig
quartz()
plot.PCA(pca1)

# extract pc scores for first two component and add to dat dataframe
dat$pc1 <- pca1$ind$coord[, 1] # indexing the first column

dat$pc2 <- pca1$ind$coord[, 2]  # indexing the second column


pca.vars <- pca1$var$coord %>% data.frame

pca.vars$vars <- rownames(pca.vars)

pca.vars.m <- melt(pca.vars, id.vars = "vars")


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ <- circleFun(c(0,0),2,npoints = 500)


p <- ggplot(data = dat, aes(x = pc1, y = pc2, col = traits$sol)) + geom_point() +
  
  geom_hline(yintercept = 0, lty = 2) +
  
  geom_vline(xintercept = 0, lty = 2) +
  
  geom_point(alpha = 0.8) + xlab ("PC1 (23%)" ) + ylab ("PC2 (17%)" ) +
  
  ggplot2::scale_color_viridis_c() +
  ggplot2::labs(col="sol")
  
  


quartz()
p


p <- ggplot(data = dat, aes(x = pc1, y = pc2, color = traits$sp, shape = traits$sp)) +
  
  geom_hline(yintercept = 0, lty = 2) +
  
  geom_vline(xintercept = 0, lty = 2) +
  
  geom_point(alpha = 0.8) + xlab ("PC1 (23%)" ) + ylab ("PC2 (17%)" )

traits$sol

quartz()
p

pca.vars %>% arrange(desc(Dim.1))  %>% head

pca.vars %>% arrange(desc(Dim.2 ))  %>% head


vars.p <-  ggplot() +
  
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  
  geom_segment(data = pca.vars, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               
               lwd = 1) + 
  
  geom_text(data = pca.vars, 
            
            aes(x = Dim.1*1.15, y =  Dim.2*1.15, 
                
                label = row.names(pca.vars)), 
            
            check_overlap = F, size = 3) +
  
  xlab("PC 1") + 
  
  ylab("PC2") +
  
  coord_equal() +
  
  theme_minimal() +
  
  theme(panel.grid = element_blank(), 
        
        panel.border = element_rect(fill= "transparent"))


quartz()
vars.p




