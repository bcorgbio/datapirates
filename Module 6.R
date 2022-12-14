library(tidyverse)
library(Momocs)
library(ape)
library(phytools)
library(RRphylo)
library(ggtree)
library(wesanderson)
lep.tree <- ladderize(ape::read.tree("lep_tree2.tre"))
lep.tree$tip.label <- gsub("_"," ",lep.tree$tip.label)
lep.sp <- read_csv("lep_image_data.csv")
tax.names <- readRDS("Lep_classification.RDS")
f <- list.files("class_out_data",pattern=".txt",full.names = TRUE)
out.df <- vroom::vroom(f, id = "filename")
out.df <- out.df %>% 
  mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
  na.omit()
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l)))
outs <-  outs.l %>% 
  Out(fac=list(wing=wings)) %>% 
  coo_flipx()
forewings <- outs %>% 
  filter(wing=="forewing")
hindwings <- outs %>% 
  filter(wing=="hindwing")
fore.min <- forewings %>% 
  coo_nb() %>% 
  min()
hind.min <- hindwings %>% 
  coo_nb() %>% 
  min()
forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()
#maybe unnecessary TBD 
forewing.pca %>% 
  plot_PCA(title = "forewings")
hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()
#maybe unnecessary TBD 
hindwing.pca %>% 
  plot_PCA(title = "hindwings")
out.data <- tibble(xy.file=basename(names(outs))) %>% 
  mutate(identifier=gsub("XY_|_hindwing|_forewing|.txt","",xy.file)) %>% 
  left_join(lep.sp)
hindwing.pca2 <-  tibble(xy.file=basename(rownames(hindwing.pca$x)),PC1=hindwing.pca$x[,1],PC2=hindwing.pca$x[,2]) %>% 
  left_join(out.data)
forewing.pca2 <-  tibble(xy.file=basename(rownames(forewing.pca$x)),PC1=forewing.pca$x[,1],PC2=forewing.pca$x[,2])%>% 
  left_join(out.data)
drops <- lep.tree$tip.label[!lep.tree$tip.label%in%unique(out.data$species)]
lep.tree2 <- drop.tip(lep.tree,drops)
#pc1s
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
#liklihood of rates across the tree
forePC1.BM<-brownie.lite(lep.tree2,fore.pc1*10)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc1*10)
forePC2.BM<-brownie.lite(lep.tree2,fore.pc2*10)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc2*10)

#comparing evolutionary rate between first and second PCs across hind and fore wing
forePC1.BM$sig2.single
hindPC1.BM$sig2.single
forePC2.BM$sig2.single
hindPC2.BM$sig2.single

#computing rates along branches
hindPC1.RR <- RRphylo(tree=lep.tree2,y=hind.pc1)
forePC1.RR <- RRphylo(tree=lep.tree2,y=fore.pc1)
hindPC2.RR <- RRphylo(tree=lep.tree2,y=hind.pc2)
forePC2.RR <- RRphylo(tree=lep.tree2,y=fore.pc2)

#test if individual clades evolve at different rates compared to tree
hindPC1.SS <-  search.shift(RR=hindPC1.RR,status.type="clade")
forePC1.SS <- search.shift(RR=forePC1.RR,status.type="clade")
hindPC2.SS <- search.shift(RR=hindPC2.RR,status.type="clade")
forePC2.SS <- search.shift(RR=forePC2.RR,status.type="clade")

#visualizing nodes with exceptional shifts 
hindPC1.plot <- plotShift(RR=hindPC1.RR,SS=hindPC1.SS)
hindPC1.plot$plotClades()
forePC1.plot <- plotShift(RR=forePC1.RR,SS=forePC1.SS)
forePC1.plot$plotClades()
hindPC2.plot <- plotShift(RR=hindPC2.RR,SS=hindPC2.SS)
hindPC2.plot$plotClades()
forePC2.plot <- plotShift(RR=forePC2.RR,SS=forePC1.SS)
forePC2.plot$plotClades()

#kenaley function to extract lineage names of shifts 
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")

plot_SS <- function(tre=NULL,SS=NULL,tax=NULL){
  nodes <- as.numeric(rownames(SS$single.clades))
  pal <- wes_palette("Zissou1",n=length(nodes))
  sp <- list()
  for(i in nodes){
    sp.i <- extract.clade(tre,i)$tip.label
    #print(head(tax))
    sub.names <- lapply(tax,function(x) x[x%in%sp.i]) 
    in.clades <- lapply(sub.names,function(x) length(x)>0) 
    all.of.clade <- lapply(sub.names,function(x) all(sapply(sp.i,function(z) z%in%x))) 
    high.clade <- names(sub.names)[last(which(all.of.clade==T))]
    all.clades <- names(sub.names)[which(in.clades==T)]
    crown <- ""
    if(high.clade!=last(names(sub.names))) crown <- "crown-"
    sub.clades <- NULL
    if(length(grepl("oidea",all.clades))>0) sub.clades <- all.clades[grepl("oidea",all.clades)]
    high.clade2 <- paste0(crown,high.clade,": ",paste0(sub.clades,collapse = "+"))
    sp[[paste0(i)]] <- tibble(n=i,species=sp.i,clade=high.clade2)
  }
  d <- do.call(rbind,sp)%>% 
    rename(label=species) 
  d2 <-  d %>% rename(clade_name=clade) 
  p <- ggtree(tre)+ scale_y_reverse()
  p$data <- p$data %>% left_join(d) %>% left_join(tibble(node=nodes,SS$single.clades) %>% mutate(shift=ifelse(rate.difference>0,"+","-")))
  p <-  p+geom_tiplab(aes(col=clade),geom="text",size=1.2)+
    geom_cladelab(data=d2,mapping=aes(node=n,col=clade_name,label=clade_name),offset=1,size=1.5)+
    geom_hilight(data=d2,mapping = aes(node = n,fill=clade_name),alpha = 0.01)+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)+
    theme(legend.position = "none")+geom_nodepoint(mapping=aes(subset = shift =="-"), size=5, shape=25,fill='blue',color='blue',alpha=0.7)+
    geom_nodepoint(mapping=aes(subset = shift =="+"), size=5, shape=24, fill='red',color='red',alpha=0.7)
  p <- p+xlim(NA,6)
  res <- tibble(n=nodes,SS$single.clades) %>% left_join(d %>% select(n,clade) %>% unique)
  return(list(plot=p,res=res))
}

#using function for our data (will be able to plot using res$plot)
hindPC1.res <- plot_SS(lep.tree2,hindPC1.SS,tax = tax.names)
forePC1.res <- plot_SS(lep.tree2,forePC1.SS,tax = tax.names)
hindPC2.res <- plot_SS(lep.tree2,hindPC2.SS,tax = tax.names)
forePC2.res <- plot_SS(lep.tree2,forePC2.SS,tax = tax.names)

hindPC1.res$plot
forePC1.res$plot
hindPC2.res$plot
forePC2.res$plot

#shape evolution correlation using PIC
hindPC1.pic <- pic(hind.pc1,phy = lep.tree2)
forePC1.pic <- pic(fore.pc1,phy = lep.tree2)
hindPC2.pic <- pic(hind.pc2,phy = lep.tree2)
forePC2.pic <- pic(fore.pc2,phy = lep.tree2)

#plotting PIC analysis 
PC1.pic <- tibble(
  hind=hindPC1.pic,
  fore=forePC1.pic
)
PC2.pic <- tibble(
  hind=hindPC2.pic,
  fore=forePC2.pic
)
PC1.pic %>% 
  ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")
PC2.pic %>% 
  ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")

#looking at r2 values
summary(lm(hind~fore,PC1.pic))
summary(lm(hind~fore,PC2.pic))
