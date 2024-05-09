################################################################################
################################################################################
#########                                                             ##########
#########                     GeneRaMeN dependencies                  ##########
#########                                                             ##########
################################################################################
################################################################################

library(shiny);
library(bslib);
library(DT);
library(dplyr);
library(magrittr);
library(purrr);
library(tidyr);
library(readxl);
library(stringr);
library(ggplot2);
library(ggpubr);
library(ggrepel);
library(RobustRankAggreg);
library(shinyWidgets);
library(shinycssloaders);
library(pheatmap);
# library(BiocManager)
# options(repos = BiocManager::repositories())
library(AnnotationDbi);
library(org.Hs.eg.db);
library(gprofiler2);
# library(EnvStats);
# library(foreach);
# library(doParallel);
library(Rfast);
library(WGCNA);
library(plotly);

################################################################################
######## Student's T test function, modified from Rfast::ttests package ########
################################################################################

ttestsModified <- function(x, y = NULL, ina, paired = FALSE, logged = FALSE, parallel = FALSE, alternative) {
  
  if ( !paired ) {
    
    if ( is.null(y) ) {
      x1 <- x[ ina == 1, ]
      x2 <- x[ ina == 2, ]
      n1 <- sum( ina == 1 )
      n2 <- length(ina) - n1
    } else {
      x1 <- x     ;    n1 <- dim(x1)[1]
      x2 <- y     ;    n2 <- dim(x2)[1]
    }
    
    m1 <- Rfast::colmeans(x1, parallel = parallel)
    m2 <- Rfast::colmeans(x2, parallel = parallel)
    f1 <- Rfast::colVars(x1, parallel = parallel) / n1
    f2 <- Rfast::colVars(x2, parallel = parallel) / n2
    fac <- f1 + f2
    dof <- fac^2 / ( f1^2 / (n1 - 1) + f2^2 / (n2 - 1) )
    stat <- ( m1 - m2 ) / sqrt(fac)
    if ( alternative == "unequal" ) {
      if ( logged ) {
        pvalue <- log(2) + pt( abs(stat), dof, lower.tail = FALSE, log.p = TRUE ) 
      } else  pvalue <- 2 * pt( abs(stat), dof, lower.tail = FALSE) 
    } else if ( alternative == "greater" ) {
      pvalue <- pt( stat, dof, lower.tail = FALSE, log.p = logged )	
    } else if ( alternative == "less" ) {
      pvalue <- pt( stat, dof, log.p = logged )
    }
    result <- cbind(stat, pvalue, dof)
    
  } else {
    n <- dim(x)[1]
    if ( is.null(y) ) {
      z <- x[ ina == 1, ] - x[ ina == 2, ]
    } else  z <- x - y    
    m <- Rfast::colmeans(z, parallel = parallel)
    s <- Rfast::colVars(z, std = TRUE, parallel = parallel)
    stat <- sqrt(n) * m / s
    if ( logged ) {
      pvalue <- log(2) + pt( abs(stat), n - 1, lower.tail = FALSE, log.p = TRUE )  
    } else  pvalue <- 2 * pt( abs(stat), n - 1, lower.tail = FALSE )    	
    result <- cbind(stat, pvalue)
  }
  
  result
}

################################################################################
####### BIRRA source code -- from Badgeley et al., (2015) Bioinformatics #######
################################ (deprecated) ##################################

# BIRRA=function(data, prior=0.05, num.bin=50, num.iter=10, return.all=F, plot.legend=F, grp=NULL, cor.stop=1, ...){
#   nr=nrow(data)
#   nrp=floor(nrow(data)*prior)
#   data=apply(-data,2,rank)/nr
#   
#   nc=ncol(data)
#   TPR=FPR=Bayes.factors=matrix(ncol=nc, nrow=num.bin)
#   binned.data=ceiling(data*num.bin)
# 
#   bayes.data=matrix(nrow=nrow(data), ncol=ncol(data))
#   
#   guess=apply(data,1,mean)
#   cprev=0
#   # par(mfrow=c(floor(sqrt(num.iter)), ceiling(sqrt(num.iter))), mai=rep(0.7,4))
#   for ( iter in 1:num.iter){
#     if((cor.stop-cprev)>1e-15){
#       guesslast=guess
#       oo=order(guess)
#       guess[oo[1:nrp]]=1
#       guess[oo[(nrp+1):nr]]=0
# 
#       for (i in 1:nc){  
#         for (bin in 1:num.bin){
#           frac=bin/num.bin
#           TPR=sum(guess[binned.data[,i]<=bin])
#           FPR=sum((!guess)[binned.data[,i]<=bin])
#           
#           Bayes.factors[bin,i]=log((TPR+1)/(FPR+1)/(prior/(1-prior)))
#           
#         }
#       }
#       
#       Bayes.factors=apply(Bayes.factors,2,smooth)
#       Bayes.factors=apply(Bayes.factors,2,function(x){rev(cummax(rev(x)))})
#       # Plot TPR vs bin for each data set
#       # if(is.null(grp)){
#       #   matplot(1:num.bin, Bayes.factors, type="l", lwd=2, ...)
#       # }
#       # else{
#       #   matplot(1:num.bin, Bayes.factors, type="l", lwd=2, lty=grp, col=grp)
#       # }
#       #
#       # title(paste("Iteration", iter))
#       # if (iter==1&plot.legend){
#       #   legend("topright", col=1:5, lty=1:4, legend=colnames(data), lwd=2, ncol=2)
#       # }
#       for (bin in 1:num.bin){
#         oo=order(Bayes.factors[bin,], decreasing=T)
#         Bayes.factors[bin, oo[1]]=Bayes.factors[bin, oo[2]]
#  
#       }
#       
#       for (i in 1:nc){
#         
#         bayes.data[,i]=Bayes.factors[binned.data[,i],i]
#         
#       }
# 
#       bb=exp(apply(bayes.data,1, sum))
#       f=prior/(1-prior)
#       prob=bb*f/(1+bb*f)
#       exp=sort(prob, decreasing=F)[nrp]
#       
#       guess=rank(-apply(bayes.data,1, sum))
#       cprev=cor(guess, guesslast)
#       if(is.na(cprev)){
#         message("correlation with pervious is manually set to be 0 because the standard deviation is zero")
#         cprev=0
#       }
#       else{
#         message("correlation with pervious iteration=",cprev)
#       }
#     }
#     else{
#       message("Converged");
#       break
#     }
#   }
#   if(return.all){
#     return(list(result=guess, data=bayes.data, BF=Bayes.factors))
#   }
#   else{
#     guess
#   }
# }