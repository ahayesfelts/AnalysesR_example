#fxn_AddPanelTitles.R


### Create function to add panel labels to figures objects created by FitPlot ####################################################
AddPanelTitles<-function(LIST){
  
  require(ggplot2)
  
  list.figures<-list()
  list.figures1<-list()
  
  j<-0#TRAITS
  for(j in 1:length(TRAIT)){
    
    i<-0 #MEASCAT
    for(i in 1:length(MEASCAT)){
      
      paneltitle<-paste(labels[i])
      p<-LIST[['figures']][[TRAIT[j]]][[MEASCAT[i]]] + ggtitle(paneltitle)
      
      #3. save the plot panel
      list.figures[[i]]<-p
    }
    names(list.figures)<-MEASCAT
    list.figures1[[j]]<-list.figures
  }
  names(list.figures1)<-TRAIT
  
  return(list.figures1)
}
