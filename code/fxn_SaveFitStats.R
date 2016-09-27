#fxn_SaveFitStats.R


### Create function to save model fit stats in FitPlot fxn ####################################################
SaveFitStats<-function(res, res0, k, j, i){
  
  plantType<-PLANT[k]
  traitType<-TRAIT[j]
  measType<-MEASCAT[i]
  
  est<-round(res$b['xval',], digits=2)
  pVal<-res$pval[2]
  studies<-res$k
  
  #manually calculate the pseudo R2 values
  #compute the proportional reduction in the variance components as a sort of pseudo R-squared value
  sigma2.1.full<-res$sigma2[1]
  sigma2.2.full<-res$sigma2[2]
  sigma2.1.red<-res0$sigma2[1]
  sigma2.2.red<-res0$sigma2[2]
  pseudoR2.1<-(res0$sigma2[1] - res$sigma2[1]) / res0$sigma2[1]
  pseudoR2.2<-(res0$sigma2[2] - res$sigma2[2]) / res0$sigma2[2]
  if(pseudoR2.1<0){pseudoR2.1<-0}
  if(pseudoR2.2<0 | is.na(pseudoR2.2)){pseudoR2.2<-0}
  pseudoR2.perc<-sum(res0$sigma2) - sum(res$sigma2) / sum(res0$sigma2)
  if(pseudoR2.perc<0 | is.na(pseudoR2.perc)){pseudoR2.perc<-0}
  
  result<-data.frame(plantType, traitType, measType, studies, est, pVal, pseudoR2.1, pseudoR2.2, pseudoR2.perc)
  return(result)
}
