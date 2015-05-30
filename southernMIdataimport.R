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
