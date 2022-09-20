#functions----
run_mean<-function(x,by){
  ex<-psych::describeBy(x,by)
  out<-NULL
  for(i in names(ex)){
    t<-data.frame(by=i,mean=ex[i][[1]]$mean,sd=ex[i][[1]]$sd)
    out<-rbind(out,t)
  }
  out$result<-paste0(round(as.numeric(out$mean),3)," (sd=",round(as.numeric(out$sd),3),")")
  result<-t(out[,c("by","result")])
  colnames(result)<-result[1,]
  result<-result[2,]
  result
}

run_prop<-function(x,by,bin=T){
  t<-table(x,by)
  p<-prop.table(table(x,by),2)
  if(bin==T){
    out<-t[2,]
    out<-paste0(out," (",round(100*p[2,],3),"%)")
  }else{
    out<-t
    out<-paste0(out," (",round(100*p,3),"%)")
  }
  out
}

#ct----
library(data.table)

data<-""
output<-""

final<-read.table(paste0(data,"ehr_in_both_fi_2-8.csv"),header=T,sep="\t")
names(final)<-c("OPTUM_LAB_ID","Age","Sex","Race","Year","EHR-FI","EHR-Frail")
ex<-read.table(paste0(data,"clm_fi_2-8.csv"),header=T,sep="\t")
ex<-ex[,c("OPTUM_LAB_ID","P_YEAR","FULL_FI","BIN_FI")]
names(ex)<-c("OPTUM_LAB_ID","Year","CLM-FI","CLM-Frail")
final<-merge(final,ex,by=c("OPTUM_LAB_ID","Year"),all.x=T)
ex<-read.table(paste0(data,"ehr_and_clm_fi_2-8.csv"),header=T,sep="\t")
ex<-ex[,c("OPTUM_LAB_ID","P_YEAR","FULL_FI","BIN_FI")]
names(ex)<-c("OPTUM_LAB_ID","Year","Both-FI","Both-Frail")
final<-merge(final,ex,by=c("OPTUM_LAB_ID","Year"),all.x=T)
remove(ex)
gc()

d1<-read.table(paste0(data,"ehr_in_both_source_fi_2-8.csv"),header=T,sep="\t")
names(d1)<-c("OPTUM_LAB_ID","Age","Sex","Race","Source","Year","EHR-FI","EHR-Frail")
d2<-read.table(paste0(data,"clm_source_fi_2-8.csv"),header=T,sep="\t")
d2<-d2[,c("OPTUM_LAB_ID","SOURCEID","P_YEAR","FULL_FI","BIN_FI")]
names(d2)<-c("OPTUM_LAB_ID","Source","Year","CLM-FI","CLM-Frail")
d3<-read.table(paste0(data,"ehr_and_clm_source_fi_2-8.csv"),header=T,sep="\t")
names(d3)<-c("OPTUM_LAB_ID","Source","Year","Both-FI","Both-Frail")
d1<-merge(d1,d2,by=c("OPTUM_LAB_ID","Source","Year"),all.x=T)
d1<-merge(d1,d3,by=c("OPTUM_LAB_ID","Source","Year"),all.x=T)
summary(as.factor(is.na(d1$`Both-Frail`)))
remove(d2,d3)
gc()

#tables----
pat_ehr<-NULL
pat_clm<-NULL
pat_both<-NULL
for(i in levels(as.factor(final$Year))){
  d<-final[final$Year==i,]
  ehr<-data.frame(data="EHR",Year=i,
                  mean_fi=mean(na.omit(d$`EHR-FI`)),sd_fi=sd(na.omit(d$`EHR-FI`)),max_fi=max(na.omit(d$`EHR-FI`)),min_fi=min(na.omit(d$`EHR-FI`)),median_fi=median(na.omit(d$`EHR-FI`)),L_25th=quantile(na.omit(d$`EHR-FI`),0.25)[[1]],U_75th=quantile(na.omit(d$`EHR-FI`),0.75)[[1]],IQR=IQR(na.omit(d$`EHR-FI`)),
                  count_frail=sum(na.omit(d$`EHR-Frail`)),perc_frail=100*(sum(na.omit(d$`EHR-Frail`))/nrow(d)))
  clm<-data.frame(data="CLM",Year=i,
                  mean_fi=mean(na.omit(d$`CLM-FI`)),sd_fi=sd(na.omit(d$`CLM-FI`)),max_fi=max(na.omit(d$`CLM-FI`)),min_fi=min(na.omit(d$`CLM-FI`)),median_fi=median(na.omit(d$`CLM-FI`)),L_25th=quantile(na.omit(d$`CLM-FI`),0.25)[[1]],U_75th=quantile(na.omit(d$`CLM-FI`),0.75)[[1]],IQR=IQR(na.omit(d$`CLM-FI`)),
                  count_frail=sum(na.omit(d$`CLM-Frail`)),perc_frail=100*(sum(na.omit(d$`CLM-Frail`))/nrow(d)))
  both<-data.frame(data="Both",Year=i,
                   mean_fi=mean(na.omit(d$`Both-FI`)),sd_fi=sd(na.omit(d$`Both-FI`)),max_fi=max(na.omit(d$`Both-FI`)),min_fi=min(na.omit(d$`Both-FI`)),median_fi=median(na.omit(d$`Both-FI`)),L_25th=quantile(na.omit(d$`Both-FI`),0.25)[[1]],U_75th=quantile(na.omit(d$`Both-FI`),0.75)[[1]],IQR=IQR(na.omit(d$`Both-FI`)),
                   count_frail=sum(na.omit(d$`Both-Frail`)),perc_frail=100*(sum(na.omit(d$`Both-Frail`))/nrow(d)))
  pat_ehr<-rbind(pat_ehr,ehr)
  pat_clm<-rbind(pat_clm,clm)
  pat_both<-rbind(pat_both,both)
  
}
pat_ehr$count_frail<-ifelse(pat_ehr$count_frail<11,NA,pat_ehr$count_frail)
pat_clm$count_frail<-ifelse(pat_clm$count_frail<11,NA,pat_clm$count_frail)
pat_both$count_frail<-ifelse(pat_both$count_frail<11,NA,pat_both$count_frail)


write.table(rbind(pat_ehr,pat_clm,pat_both),"./fi stats by year.csv",sep=",",col.names=T,row.names=F)


all_ehr<-NULL
all_clm<-NULL
all_both<-NULL
for(i in levels(as.factor(d1$Year))){
  d<-d1[d1$Year==i,]
  for(j in levels(as.factor(d$Source))){
    e<-d[d$Source==j,]
    ehr<-data.frame(data="EHR",Source=j,Year=i,
                    mean_fi=mean(na.omit(e$`EHR-FI`)),sd_fi=sd(na.omit(e$`EHR-FI`)),max_fi=max(na.omit(e$`EHR-FI`)),min_fi=min(na.omit(e$`EHR-FI`)),median_fi=median(na.omit(e$`EHR-FI`)),L_25th=quantile(na.omit(e$`EHR-FI`),0.25)[[1]],U_75th=quantile(na.omit(e$`EHR-FI`),0.75)[[1]],IQR=IQR(na.omit(e$`EHR-FI`)),
                    count_frail=sum(na.omit(e$`EHR-Frail`)),perc_frail=100*(sum(na.omit(e$`EHR-Frail`))/nrow(e)))
    clm<-data.frame(data="CLM",Source=j,Year=i,
                    mean_fi=mean(na.omit(e$`CLM-FI`)),sd_fi=sd(na.omit(e$`CLM-FI`)),max_fi=max(na.omit(e$`CLM-FI`)),min_fi=min(na.omit(e$`CLM-FI`)),median_fi=median(na.omit(e$`CLM-FI`)),L_25th=quantile(na.omit(e$`CLM-FI`),0.25)[[1]],U_75th=quantile(na.omit(e$`CLM-FI`),0.75)[[1]],IQR=IQR(na.omit(e$`CLM-FI`)),
                    count_frail=sum(na.omit(e$`CLM-Frail`)),perc_frail=100*(sum(na.omit(e$`CLM-Frail`))/nrow(e)))
    both<-data.frame(data="Both",Source=j,Year=i,
                     mean_fi=mean(na.omit(e$`Both-FI`)),sd_fi=sd(na.omit(e$`Both-FI`)),max_fi=max(na.omit(e$`Both-FI`)),min_fi=min(na.omit(e$`Both-FI`)),median_fi=median(na.omit(e$`Both-FI`)),L_25th=quantile(na.omit(e$`Both-FI`),0.25)[[1]],U_75th=quantile(na.omit(e$`Both-FI`),0.75)[[1]],IQR=IQR(na.omit(e$`Both-FI`)),
                     count_frail=sum(na.omit(e$`Both-Frail`)),perc_frail=100*(sum(na.omit(e$`Both-Frail`))/nrow(e)))
    all_ehr<-rbind(all_ehr,ehr)
    all_clm<-rbind(all_clm,clm)
    all_both<-rbind(all_both,both)
    
  }
  
}
all_ehr$count_frail<-ifelse(all_ehr$count_frail<11,NA,all_ehr$count_frail)
all_clm$count_frail<-ifelse(all_clm$count_frail<11,NA,all_clm$count_frail)
all_both$count_frail<-ifelse(all_both$count_frail<11,NA,all_both$count_frail)

write.table(all_ehr,"./ehr stats by sourceid and year.csv",sep=",",col.names=T,row.names=F)
write.table(all_clm,"./clm stats by sourceid and year.csv",sep=",",col.names=T,row.names=F)
write.table(all_both,"./ehr and clm stats by sourceid and year.csv",sep=",",col.names=T,row.names=F)

#plots----
library(ggplot2)

p1<-ggplot(data=all_both[all_both$Year=="2016",])+
  geom_errorbar(aes(x=Source,ymin=mean_fi-sd_fi,ymax=mean_fi+sd_fi),width=0.5,size=1.2,color="goldenrod",alpha=.8)+
  geom_point(aes(y=mean_fi,x=Source),stat="identity",fill="steelblue",color="steelblue",shape=15,size=3)+
  geom_hline(yintercept=c(mean(na.omit(all_both[all_both$Year=="2016",]$mean_fi))),size=1,color="black",lty=1,alpha=0.8)+
  geom_text(aes(6,mean(na.omit(all_both[all_both$Year=="2016",]$mean_fi))),
                label=paste0("Combined-",round(mean(na.omit(all_both[all_both$Year=="2016",]$mean_fi)),3)),
                vjust=1,size=5)+
  geom_hline(yintercept=c(mean(na.omit(all_both[all_both$Year=="2016",]$mean_fi))-mean(na.omit(all_both[all_both$Year=="2016",]$sd_fi)),
                          mean(na.omit(all_both[all_both$Year=="2016",]$mean_fi))+mean(na.omit(all_both[all_both$Year=="2016",]$sd_fi))),
             size=1,color="darkred",lty=1,alpha=0.8)+
  ylim(0,.3)+
  ggtitle(label="2016 Combined Mean FI by Source")+
  theme_minimal()+
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle=45,size=12))
lines<-d1[,c("SOURCEID","PATIENTS","COUNT_FRAIL","PERCENT_FRAIL")]
lines$source<-"CLM"
all_lines<-lines


t1<-as.data.frame(prop.table(table(d1[d1$Year==2019,]$Source,d1[d1$Year==2019,]$`EHR-Frail`),1))
t1<-t1[as.character(t1$Var2)=="1",]
t2<-as.data.frame(prop.table(table(d1[d1$Year==2019,]$Source,d1[d1$Year==2019,]$`CLM-Frail`),1))
t2<-t2[as.character(t2$Var2)=="1",]
t3<-as.data.frame(prop.table(table(d1[d1$Year==2019,]$Source,d1[d1$Year==2019,]$`Both-Frail`),1))
t3<-t3[as.character(t3$Var2)=="1",]
t1$data<-"EHR"
t2$data<-"CLM"
t3$data<-"Both"
all<-rbind(t1,t2,t3)
names(all)<-c("Source","Frail","Percent Frail","data")

p2<-ggplot(data=all[all$data!="Both",],aes(x=Source,y=100*`Percent Frail`,group=data))+
  geom_line(aes(color=data),size=1.5)+
  geom_hline(yintercept=8.012,lty=2,color="red")+
  geom_text(aes(5,8.012),label=paste0("CLM-",8.012),vjust=1,size=5)+
  geom_hline(yintercept=3.032,lty=2,color="blue")+
  geom_text(aes(5,3.032),label=paste0("EHR-",3.032),vjust=1,size=5)+
  ylim(0,50)+
  scale_color_brewer(palette="Set1")+
  ggtitle(label="2019 Percent Frail by Source")+
  theme_minimal()+
  theme(text=element_text(size=20),
        axis.text.x=element_text(angle=45,size=12),
        legend.title=element_blank())

p2

