# baseline_table_tools

```
library(flextable)

num_to_str<-function(x,digits=1)sprintf(paste0("%.",digits,"f"),x)

summarize_table<-function(...,category=NULL,missing=NULL,digits=1){
  a_list<-list(...)
  if(is.null(category))category<-sort(unique(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_list[[idx]]<-factor(a_list[[idx]],category)
  }
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  a_mat<-c()
  if(missing){
    a_mat<-cbind(a_mat,paste0("    ",c(category,"(Missing)")))
  }else{
    a_mat<-cbind(a_mat,paste0("    ",category))
  }
  for(idx in 1:length(a_list)){
    a_value<-a_list[[idx]]
    a_table<-table(a_value,useNA="no")
    if(missing){
      a_col<-c(
        paste0(a_table," (",num_to_str(100*a_table/sum(a_table),digits=1),"%)"),
        sum(is.na(a_value)))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-paste0(a_table," (",num_to_str(100*a_table/sum(a_table),digits=1),"%)")
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

summarize_percent<-function(...,true_val=TRUE,missing=NULL,digits=1){
  a_list<-list(...)
  a_mat<-c()
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  for(idx in 1:length(a_list)){
    if(missing){
      a_col<-rbind(
        paste0(sum(a_list[[idx]],na.rm=T)," (",num_to_str(100*sum(a_list[[idx]],na.rm=TRUE)/sum(!is.na(a_list[[idx]]))),"%",")"),
        paste0("(Missing: ",sum(is.na(a_list[[idx]])),")"))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-paste0(sum(a_list[[idx]])," (",num_to_str(100*sum(a_list[[idx]],na.rm=TRUE)/sum(!is.na(a_list[[idx]]))),"%",")")
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

summarize_median<-function(...,missing=NULL,digits=1){
  a_list<-list(...)
  a_mat<-c()
  if(is.null(missing))missing<-any(is.na(unlist(a_list)))
  for(idx in 1:length(a_list)){
    a_val<-a_list[[idx]]
    if(missing){
      a_col<-rbind(
        paste0(num_to_str(median(a_val,na.rm=T),digits=digits)," (",
               num_to_str(quantile(a_val,na.rm=T,0.25),digits=digits),"-",
               num_to_str(quantile(a_val,na.rm=T,0.75),digits=digits),")"),
        paste0("(Missing: ",sum(is.na(a_val)),")"))
      a_mat<-cbind(a_mat,a_col)
    }else{
      a_col<-rbind(
        paste0(num_to_str(median(a_val,na.rm=T),digits=digits)," (",
               num_to_str(quantile(a_val,na.rm=T,0.25),digits=digits),"-",
               num_to_str(quantile(a_val,na.rm=T,0.75),digits=digits),")"))
      a_mat<-cbind(a_mat,a_col)
    }
  }
  colnames(a_mat)<-NULL
  return(a_mat)
}

sort_time<-Vectorize(function(t0,t1,t2,t3,t4){
  all_names<-c("0","s1","s2","s3","5")
  all_times<-c(t0,t1,t2,t3,t4)
  unique_times<-sort(unique(all_times))
  result<-c()
  for(a_time in unique_times){
    result<-c(result,paste0(all_names[which(all_times==a_time)],collapse="="))
  }
  return(paste0(result,collapse="<"))
})
```
