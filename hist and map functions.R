smi <- (read.csv("so_Michigan_v0.3.csv"))
table(rowSums(is.na(smi)))
smi <- na.omit(smi)
smismall <- smi[,!colnames(smi)%in% c("X", "x", "y")]
rowSums(smismall) 
sumsmi <- rowSums(smismall) 
hist(sumsmi)
#histogram of the frequency of the sum of all the trees
#low values make sense. likely areas next to bodies of water/borders
#high values maybe related to change in survey count, 2 tree-> 4 tree
summary(sumsmi)
smiprop <- smismall / sumsmi
make_hist <- function(x){
  #this function creates a histogram
  hist(smiprop[,x], ylog = TRUE, xlim = c(0,1), main = x, xlab = "proportion")
}
#Fur has low counts
hist(smiprop[,"Fir"], xlab = "proportion")
make_hist("Fir")

#correct manner for making a file for each tree type
paste("figures_", colnames(smiprop), ".png", sep="")


make_hist <- function(x){
  #this function creates a histogram
  png(file= paste("figures.png/hists/", x, ".png", sep=""))
  #this saves the file to figures.png/hists
  hist(smiprop[,x], ylog = TRUE, xlim = c(0,1), main = x, xlab = "proportion")
  dev.off() 
  }
lapply(colnames(smiprop), make_hist)
#correct way to save histograms in a folder!
make_map <- function(x){
  #this function creates a map
  png(file= paste("figures.png/maps/", x, ".png", sep=""))
  plot(smi[,c('x', 'y')], col = 'red', pch=19, cex = 0.5, main = x, xlab = "eastings", ylab = "northing")
  points(smi[smiprop[,x]>0, c('x', 'y')], 
  col = gray(1 - smiprop [smiprop[,x] > 0, x]), 
  pch=19, cex = 0.5)
  trash <- dev.off()
   }
> lapply(colnames(smiprop), make_map)