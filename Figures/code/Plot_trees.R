library(phytools)
library()
packageVersion("phytools")


all.tree<-read.tree("../data/sorghum_tree.nj.txt.nwk")

                      
                      
df <- read.table("../data/sol_VL.tassel",row.names = 1,header = TRUE)
colnames(df)
nrow(df)
head(df)

x <- df[,1]
x <-setNames(x,rownames(df))

tree<-drop.tip(
  all.tree, 
  setdiff(all.tree$tip.label,names(x))
)

any(all.tree$tip.label %in% row.names(x))
all.tree$tip.label[grepl("PI570255",all.tree$tip.label)]
names(x)[grepl("PI570255",names(x))]
length(x)
Ntip(tree)
contMap(tree,x,plot=FALSE)
ace(x, tree)
quartz()
plot(tree)
?ace
which(is.na(x))       
quartz()
plotBranchbyTrait(tree,x,method="tips", show.tip.label = FALSE)

quartz()
plotBranchbyTrait(tree,x,type = "fan", method="tips", show.tip.label = FALSE)

plotBranchbyTrait(tree, x, mode=c())


library("treeio")
library("ggtree")

quartz()
ggtree::ggtree(tree, layout="daylight")
  