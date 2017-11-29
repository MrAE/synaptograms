#!usr/bin/env Rscript
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
                 make_option(c("-m", "--means"), 
                             type='character', default=NULL,
                             help = "means file name",
                             metavar="character"),
                 make_option(c("-s", "--sds"), 
                             type='character', default=NULL,
                             help = "sds file name",
                             metavar="character")
                             )

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

#if (is.null(opt$file)){
#    print_help(opt_parser)
#  stop("You have missing arguments.n", call.=FALSE)
#}

input <- opt$file
#means <- read.csv(opt$means)

name <- h5ls(input)$name[grep("cube", h5ls(input)$name)]
loc <- h5read(input, name = 'Locations')
dat <- h5read(input, name = name)
chan <- h5read(input, name = 'Channels')
H5close()

type <- c("ot", "ot", "ot", 
          'in', 'in', 'in','in',
          'ex', 'ex', 'ex', 'ex', 'em')
          
ctype <- data.frame(chan, type=type)

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
  mr$type <- ctype$type[ch]
  mr
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


P <- list()
for(k in 1:length(rr)){
  mr <- rr[[k]]

  pem <- 
  ggplot(mr[mr$type == "em",], 
    aes(x,y, group = factor(type), fill = value)) +
    geom_raster() + 
    scale_y_reverse() + 
    facet_grid(ch ~ z, labeller = label_both) +
    #scale_fill_gradient(low = "black", high = "white", limits = c(mmem, MMem)) + th + 
    scale_fill_gradient(low = "black", high = "white") + th + 
    theme(legend.position = "none")


  pex <- 
  ggplot(mr[mr$type == "ex",], 
    aes(x,y, group = factor(type), fill = value)) +
    geom_raster() + 
    scale_y_reverse() + 
    facet_grid(ch ~ z, labeller = label_both) +
    #scale_fill_gradient(low = "black", high = "green", limits = c(mmex, MMex)) + th
    scale_fill_gradient(low = "black", high = "green") + th
  
  pin <- 
  ggplot(mr[mr$type == "in",], 
    aes(x,y, group = factor(type), fill = value)) +
    geom_raster() + 
    scale_y_reverse() + 
    facet_grid(ch ~ z, labeller = label_both) +
    #scale_fill_gradient(low = "black", high = "red", limits = c(mmin,MMin)) + th
    scale_fill_gradient(low = "black", high = "red") + th
  
  pot <- 
    ggplot(mr[mr$type == "ot",], 
    aes(x,y, group = factor(type), fill = value)) +
    geom_raster() +
    scale_y_reverse() + 
    facet_grid(ch ~ z, labeller = label_both) +
    #scale_fill_gradient(low = "black", high = "blue", limits = c(mmot,MMot)) + th
    scale_fill_gradient(low = "black", high = "blue") + th

  lay <- rbind(
        matrix(1,table(type)['ot'],1),
        matrix(2,table(type)['in'],1), 
        matrix(3,table(type)['ex'],1), 
        matrix(4,table(type)['em'],1))
  
  
  ff <-  tempfile() 
  png(filename = ff)
  P[[k]] <- grid.arrange(pot, pin, pex, pem, layout_matrix = lay)
  dev.off()
  unlink(ff)

  cname <- 
    paste0(opt$out, sprintf("_synaptogram_x%d_y%d_z%d.png", loc[k,1], loc[k,2], loc[k,3]))
    
  b = 1080
  w = b
  h = 1.25*b
  
  png(cname, width = w, height=h)
  plot(P[[k]])
  dev.off()
}

