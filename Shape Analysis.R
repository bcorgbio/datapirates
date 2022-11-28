library(tidyverse)
install.packages("Momocs")
library(Momocs)
f <- list.files("lep_examples",pattern=".txt",full.names = TRUE)
out <- read_delim(f[1],delim="\t") %>% 
  as.matrix()
out %>% 
  list() %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()
outs.l <- list()
for(i in f){
  outs.l[[i]] <- out <- read_delim(i,delim="\t") %>% 
    as.matrix()
}
outs.l %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()
outs.l <- list()
wing.l <- list()
for(i in f){
  outs.l[[i]] <- read_delim(i,delim="\t") %>% 
    as.matrix()
  wing.l[[i]] <- gsub(".+_(hindwing|forewing).txt","\\1",i)
}
outs <- outs.l %>% 
  Out(fac=list(wing=wing.l %>% unlist)) %>% 
  coo_flipx()
forewings <- outs %>% 
  filter(wing=="forewing")
hindwings <- outs %>% 
  filter(wing=="hindwing")
forewings %>% 
  stack()
hindwings %>% 
  stack()
fore.min <- forewings %>% 
  coo_nb() %>% 
  min()
forewings %>%
  coo_interpolate(fore.min) %>% 
  fgProcrustes() %>% 
  stack()
hind.min <- hindwings %>% 
  coo_nb() %>% 
  min()
hindwings %>% 
  coo_interpolate(hind.min) %>% 
  fgProcrustes() %>% 
  stack()
forewings %>%
  coo_interpolate(fore.min) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) 
hindwings %>% 
  coo_interpolate(hind.min) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE)
forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()
hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()
forewing.pca %>% 
  plot_PCA(title = "forewings")
hindwing.pca %>% 
  plot_PCA(title = "hindwings")
library(ape)
lep.tree <- ape::read.tree("lep_tree2.tre")
plot(lep.tree,cex=0.1)
lep.tree <- ladderize(lep.tree)
plot(lep.tree,cex=0.1)
lep.tree$tip.label <- gsub("_"," ",lep.tree$tip.label)
basename(names(outs))[1:5]
lep.sp <- read_csv("lep_image_data.csv")
head(lep.sp)
head(lep.sp$identifier)
out.data <- tibble(xy.file=basename(names(outs))) %>% 
  mutate(identifier=gsub("XY_|_hindwing|_forewing|.txt","",xy.file)) %>% 
  left_join(lep.sp)
head(out.data)
head(hindwing.pca$x,1)
head(forewing.pca$x,1)
hindwing.pca2 <-  tibble(xy.file=basename(rownames(hindwing.pca$x)),PC1=hindwing.pca$x[,1],PC2=hindwing.pca$x[,2]) %>% 
  left_join(out.data)
forewing.pca2 <-  tibble(xy.file=basename(rownames(forewing.pca$x)),PC1=forewing.pca$x[,1],PC2=forewing.pca$x[,2])%>% 
  left_join(out.data)
drops <- lep.tree$tip.label[!lep.tree$tip.label%in%unique(out.data$species)]
lep.tree2 <- drop.tip(lep.tree,drops)
plot(lep.tree2)
#PC1s
hind.pc1 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull
names(hind.pc1) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)
fore.pc1 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(PC1)
names(fore.pc1) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)
#PC2s
hind.pc2 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)
names(hind.pc2) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>% 
  pull(species)
fore.pc2 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)
names(fore.pc2) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(species)
library(phytools)
forePC1.BM<-brownie.lite(lep.tree2,fore.pc1*10)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc1*10)
forePC2.BM<-brownie.lite(lep.tree2,fore.pc2*10)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc2*10)
forePC1.BM$sig2.single
install.packages("RRphylo")
library(RRphylo)
hindPC1.RR <- RRphylo(tree=lep.tree2,y=hind.pc1)
hindPC1.RR$rates
hindPC1.SS<- search.shift(RR=hindPC1.RR,status.type="clade")
hindPC1.SS$single.clades
plot(lep.tree2)
nodelabels(node = as.numeric(rownames(hindPC1.SS$single.clades)),text = rownames(hindPC1.SS$single.clades))
hindPC1.plot <- plotShift(RR=hindPC1.RR,SS=hindPC1.SS)
hindPC1.plot$plotClades()
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
library(ggtree)
install.packages("wesanderson")
library(wesanderson)
plot_SS <- function(tre=NULL,SS=NULL,taxon.names=NULL){
  pal <- wes_palette("BottleRocket1",n=length(SS$single.clades))
  nodes <- as.numeric(rownames(SS$single.clades))
  sp <- list()
  for(i in nodes){
    sp[[paste0(i)]] <- tibble(nodes=i,species=extract.clade(tre,i)$tip.label)
  }
  col.pal <- tibble(nodes=nodes,col=pal)
  d <- do.call(rbind,sp) %>% left_join(taxon.names) %>% left_join(col.pal)%>% 
    rename(label=species) 
  d2<- d %>% rename(clade=higher_taxon) 
  p <- ggtree(tre) + xlim(NA, 8)+ scale_y_reverse()
  p$data <- p$data %>% left_join(d) %>% left_join(tibble(node=nodes,SS$single.clades) %>% mutate(shift=ifelse(rate.difference>0,"+","-")))
  p <-  p+geom_tiplab(aes(col=higher_taxon),geom="text")+
    geom_cladelab(data=d2,mapping=aes(node=nodes,col=clade,label=clade),offset=2.5)+
    geom_hilight(d2,mapping = aes(node = nodes,fill=clade),alpha = 0.02)+geom_nodepoint(mapping=aes(subset = shift =="-"), size=5, shape=25,fill='blue',color='blue',alpha=0.7)+
    geom_nodepoint(mapping=aes(subset = shift =="+"), size=5, shape=24, fill='red',color='red',alpha=0.7)+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)+
    theme(legend.position = "none")
  res <- tibble(nodes=nodes,SS$single.clades) %>% left_join(d %>% select(nodes,higher_taxon) %>% unique)
  return(list(plot=p,res=res))
}
hindPC1.res <- plot_SS(lep.tree2,hindPC1.SS,hindwing.pca2)
hindPC1.res$plot
hindPC1.res$res
hindPC1.pic <- pic(hind.pc1,phy = lep.tree2)
forePC1.pic <- pic(fore.pc1,phy = lep.tree2)
PC1.pic <- tibble(
  hind=hindPC1.pic,
  fore=forePC1.pic
)
PC1.pic %>% 
  ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")
summary(lm(hind~fore,PC1.pic))
