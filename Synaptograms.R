#!/usr/bin/env Rscript
###
### Rscript to plot synaptograms from
### hdf5 file 
###
###
### Jesse Leigh Patsolic 
### 2017 <jpatsolic@jhu.edu>
### S.D.G 
#

require(ggplot2)
require(foreach)
require(rhdf5)
require(reshape2)
require(gplots)
require(raster)
require(gridExtra)
require(data.table)
require(optparse)

option_list <- list(
                 make_option(c("-f", "--file"), 
                             type='character', default=NULL,
                             help = "input cubes file name",
                             metavar="character"),
                 make_option(c("-o", "--out"), 
                             type='character', default=NULL,
                             help = "output file name prefix",
                             metavar="character"),
                 make_option(c("-p", "--params"), 
                             type='character', default=NULL,
                             help = "means file name",
                             metavar="character"),
                 make_option(c("-F", "--Fzero"), 
                             type='character', default=NULL,
                             help = "F0 file name",
                             metavar="character"),
                 make_option(c("-L", "--links"),
                             type='logical', default=FALSE,
                             help = "F0 file name",
                             metavar="character")
                             )

if(FALSE){
  opt <- list()
  opt$file <- "testing.csv.h5"
  opt$out <- "synTest"
  opt$params <- "params.csv"
}

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

#if (is.null(opt$file)){
#    print_help(opt_parser)
#  stop("You have missing arguments.n", call.=FALSE)
#}

input <- opt$file

name <- h5ls(input)$name[grep("cube", h5ls(input)$name)]
loc <- h5read(input, name = 'Locations')
dat <- h5read(input, name = name)
chan <- h5read(input, name = 'Channels')
H5close()

if(!is.null(opt$params)){
  parms <- read.table(opt$params, header = TRUE, sep = ',', row.names = 1, stringsAsFactors=FALSE)
} else {
  parms <- t(data.frame(colors = rep(1,dim(dat)[5]), 
                        means = rep(0,dim(dat)[5]),
                        sds = rep(1,dim(dat)[5])))
  colnames(parms) <- chan
}

type  <- as.character(parms[which(rownames(parms) == "colors"),])
means <- as.numeric(parms[which(rownames(parms) == "means"),])
sds   <- as.numeric(parms[which(rownames(parms) == "sds"),])
          
ctype <- data.frame(chan, type=type, stringsAsFactors = FALSE)

dimnames(dat) <- list(NULL, NULL, NULL, 1:dim(dat)[4], chan)
#if(FALSE){
#  G <- readRDS("maskGaussian_mu0_sigma2123_2123_5-455.rds")
#  plot(raster(G[,,6]))
#  
#  for(j in 1:dim(dat)[5]){
#    for(i in 1:dim(dat)[4]){
#    dat[,,,i,j] <- G * dat[,,,i,j]
#    }
#  }
#}

F0 <- 
  foreach(j = 1:dim(dat)[4]) %:%
  foreach(i = 1:dim(dat)[5]) %do% {
      f0 <- 
        (sum(dat[,,,j,i]) - means[i])/sds[i]
}

names(F0) <- 1:length(F0)
for(i in 1:length(F0)){
  names(F0[[i]]) <- chan
}

F0min <-min(Reduce(c, Reduce(c, F0)), na.rm = TRUE)
F0max <-max(Reduce(c, Reduce(c, F0)), na.rm = TRUE)


rr <- foreach(l = 1:dim(dat)[4]) %do% {
  r <- dat[,,,l,]
  for(i in 1:dim(r)[4]){
    m <- min(r[,,,i])
    M <- max(r[,,,i])
    r[,,,i] <- (r[,,,i] - m)/(M - m)
  }
  mr <- melt(r)
  colnames(mr) <- c("x", "y", "z", "ch", "value")
  mr$z <- mr$z - (max(mr$z) +1)/2
  ch <- mr$ch
  #mr$type <- ctype$type[ch]
  #mr

  F0z <- Reduce('rbind', 
           lapply(names(F0[[l]]), 
             function(n){
             Fz <- mr[mr$ch == n,]
             Fz$F0 <- as.numeric(F0[[l]][n])
             Fz
             }))
  ch <- F0z$ch
  F0z$type <- ctype$type[ch]
  F0z
}


th <- theme(axis.text = element_blank(), axis.ticks = element_blank(),
            axis.title.y = element_blank(), axis.title.x = element_blank(),
            legend.position="none", legend.key.size = unit(2,'lines'),
            panel.spacing = unit(0, "lines"))

#pg <- list()
#for(i in 1:length(rr)){
#  pdat <- rr[[i]]
#  pg[[i]] <- 
#    ggplot(pdat, 
#    aes(x,y, group = ch, fill = value)) +
#    geom_raster() + 
#    scale_y_reverse() + 
#    facet_grid(ch ~ z, labeller = label_both) +
#    #scale_fill_gradient(low = "black", high = "green") + th
#    scale_fill_gradient(low = "black", high = "white") + th
#} 
#
#
##pdf("collman14v2_synaptograms.pdf", width = 12, height = 12)
#for(i in 1:length(pg)){
#  plot(pg[[i]])
#  #Sys.sleep(3)
#}
##dev.off()
#
#
#for(k in 1:length(rr)){
#  mr <- rr[[k]]
#  pex <- 
#  ggplot(mr[mr$type == "ex",], 
#  mr
#}

#mmem <- min(sapply(rr, function(am){ min(am[am$type == "em",]$value) }))
#MMem <- max(sapply(rr, function(am){ max(am[am$type == "em",]$value) }))
#mmex <- min(sapply(rr, function(am){ min(am[am$type == "ex",]$value) }))
#MMex <- max(sapply(rr, function(am){ max(am[am$type == "ex",]$value) }))
#mmin <- min(sapply(rr, function(am){ min(am[am$type == "in",]$value) }))
#MMin <- max(sapply(rr, function(am){ max(am[am$type == "in",]$value) }))
#mmot <- min(sapply(rr, function(am){ min(am[am$type == "ot",]$value) }))
#MMot <- max(sapply(rr, function(am){ max(am[am$type == "ot",]$value) }))


#P <- list()
#for(k in 1:length(rr)){
#  mr <- rr[[k]]
#
#  pem <- 
#  ggplot(mr[mr$type == "em",], 
#    aes(x,y, group = factor(type), fill = value)) +
#    geom_raster() + 
#    scale_y_reverse() + 
#    facet_grid(ch ~ z, labeller = label_both) +
#    #scale_fill_gradient(low = "black", high = "white", limits = c(mmem, MMem)) + th + 
#    scale_fill_gradient(low = "black", high = "white") + th + 
#    theme(legend.position = "none")
#
#
#  pex <- 
#  ggplot(mr[mr$type == "ex",], 
#    aes(x,y, group = factor(type), fill = value)) +
#    geom_raster() + 
#    scale_y_reverse() + 
#    facet_grid(ch ~ z, labeller = label_both) +
#    #scale_fill_gradient(low = "black", high = "green", limits = c(mmex, MMex)) + th
#    scale_fill_gradient(low = "black", high = "green") + th
#  
#  pin <- 
#  ggplot(mr[mr$type == "in",], 
#    aes(x,y, group = factor(type), fill = value)) +
#    geom_raster() + 
#    scale_y_reverse() + 
#    facet_grid(ch ~ z, labeller = label_both) +
#    #scale_fill_gradient(low = "black", high = "red", limits = c(mmin,MMin)) + th
#    scale_fill_gradient(low = "black", high = "red") + th
#  
#  pot <- 
#    ggplot(mr[mr$type == "ot",], 
#    aes(x,y, group = factor(type), fill = value)) +
#    geom_raster() +
#    scale_y_reverse() + 
#    facet_grid(ch ~ z, labeller = label_both) +
#    #scale_fill_gradient(low = "black", high = "blue", limits = c(mmot,MMot)) + th
#    scale_fill_gradient(low = "black", high = "blue") + th
#
#  lay <- rbind(
#        matrix(1,table(type)['ot'],1),
#        matrix(2,table(type)['in'],1), 
#        matrix(3,table(type)['ex'],1), 
#        matrix(4,table(type)['em'],1))
#  
#  
#  ff <-  tempfile() 
#  png(filename = ff)
#  P[[k]] <- grid.arrange(pot, pin, pex, pem, layout_matrix = lay)
#  dev.off()
#  unlink(ff)
#
#  cname <- 
#    paste0(opt$out, sprintf("_synaptogram_x%d_y%d_z%d.png", loc[k,1], loc[k,2], loc[k,3]))
#    
#  b = 1080
#  w = b
#  h = 1.25*b
#  
#  png(cname, width = w, height=h)
#  plot(P[[k]])
#  dev.off()
#}

### Colors from params.csv
for(k in 1:length(rr)){
  mr <- rr[[k]]

  pp <- list()
  ppF0 <- list()
  ut <- unique(type)
  for(ui in 1:length(ut)){
    pp[[ui]] <- 
      ggplot(mr[mr$type == ut[ui],],
       aes(x,y, fill = value)) +
       geom_raster() + 
       scale_y_reverse() + 
       facet_grid(ch ~ z, labeller = label_both) +
       scale_fill_gradient(low = "black", high = ut[ui]) + 
       th + 
       theme(legend.position = "none")

    ppF0[[ui]] <- 
      ggplot(mr[mr$type == ut[ui],],
        aes(x,y, group = type, fill = F0)) +
        geom_raster() + 
        scale_y_reverse() + 
        facet_grid(ch + F0 ~ type, labeller = label_both) +
        scale_fill_gradient2(low = "darkorchid4", 
                                mid = "gray99", 
                                high = "darkorange3",
                                midpoint = 0, limits=c(F0min, F0max)) + th
  }

  lay <- list()
  for(i in 1:length(as.numeric(table(type)))){
      lay[[i]] <- matrix(i,table(type)[i],1)
  }

  lay <- Reduce('rbind', lay)

  lz <- length(range(mr$z)[1]:range(mr$z)[2])
  laysep <- c()
  kj <- 1
  for(i in seq(1,7,2)){
    inner <- c()
    for(j in 1:table(type)[kj]){
      inner <- rbind(inner, c(rep(i, lz),rep(i+1,1)))
    }
  kj <- kj +1
  laysep <- rbind(laysep, inner)
  }

  
  cname <- 
    paste0(opt$out, sprintf("_x%d_y%d_z%d.png", loc[k,1], loc[k,2], loc[k,3]))
    
  b = 100 * (dim(dat)[5] + 1)#1080
  w = b
  h = 1.25*b
  
  png(cname, width = w, height=h)
  #plot(grid.arrange(grobs = pp, layout_matrix = lay))
  grid.arrange(pp[[1]], ppF0[[1]], pp[[2]], ppF0[[2]], pp[[3]], ppF0[[3]], pp[[4]], ppF0[[4]], layout_matrix = laysep)
  dev.off()
  gc()

}
