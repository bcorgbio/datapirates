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
