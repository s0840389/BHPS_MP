rm(list=ls())

require(readstata13)
require(data.table)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(dplyr)
require(stringr)
require(readxl)
require(sqldf)

setwd('~/OneDrive/Documents/research/BHPS/')


# harmonised data

  load('indresp.rda')
  
  load('hhresp.rda')
  
  dt_indresp=data.table(dt_indresp)
  dt_hhresp=data.table(dt_hhresp)
  
  dt=merge(dt_indresp,dt_hhresp,by='hidp',all.x=TRUE,suffixes = c('','.hh'))
  
# initial cleaning
  
  dt=dt[fihhmngrs_dv>100&fimngrs_dv>100] # have to report some meaningful household and personal income
  
# remove duplicaets
  
  dtn2=dt[,.('n'=.N,'m'=istrtdatm,fihhmngrs_dv),by=.(pidp,istrtdaty)]
  dtn2=dtn2[n>1]
  dtn2=dtn2[order(pidp,fihhmngrs_dv)]
  
# interview date
  
  dt$intdate=as.Date(paste(dt$istrtdaty,dt$istrtdatm,15,sep='-'),'%Y-%B-%d')
  dt$year=year(dt$intdate)
  dt$month=month(dt$intdate)
  dt$qtr=quarter(dt$intdate)
  
  dt$incdate=dt$intdate-30
  dt$incyear=year(dt$incdate)
  dt$incmonth=month(dt$incdate)
  dt$incqtr=quarter(dt$incdate)
  
  fy<-function(dt){
    
    f=ifelse(dt$incqtr>=2,dt$incyear,dt$incyear-1)
      
    return(f)
  }
  
  dt$fy=fy(dt)
  
  
# head of household
  
  dtmax=dt[,.(fimngrs_dv=max(fimngrs_dv,na.rm=TRUE)),by=hidp]
  
  dtmax=merge(dtmax,dt[,.(hidp,pidp,fimngrs_dv)],by=c('hidp','fimngrs_dv'))
  dtmax$hoh=1
  
  dt=merge(dt,dtmax[,.(pidp,hoh,hidp)],by=c('pidp','hidp'),all.x=TRUE)
  dt$hoh[is.na(dt$hoh)]=0

# number of adults & equivalisation factors
  
  dt$nadults=dt$hhsize-(dt$nkids_dv)*(dt$nkids_dv>0)
  
  dt$nadults=ifelse(dt$nadults<=0,1,dt$nadults)
  
  dt$eqv_fct=1+((dt$nadults)-1)*0.7
  
# Employment status
  
  dt$jbstat=tolower(as.character(dt$jbstat))
  
  dt$jbstat=str_trim(dt$jbstat,'both')
  
  dt$E=(dt$jbstat %in% c('paid employment(ft/pt)','self employed','self-employed','employed') | dt$fimnlabgrs_dv>100)*1
  
  dt$U=(dt$jbstat %in% c('unemployed','temporarily laid off/short term working'))*1
  
# Full time
  
  dt$jbft_dv=tolower(dt$jbft_dv)
  
  dt$FT=1*(dt$jbft_dv %in% c('ft employee','full time: 30 hrs +'))
    
# Income
  
  dt$fimngrs_dv=dt$fimngrs_dv*12
  dt$fimnlabgrs_dv=dt$fimnlabgrs_dv*12
  dt$fihhmngrs_dv=12*dt$fihhmngrs_dv

  dt_cpi=read.csv('MP_Inequality/input/cpi.csv')
  dt_cpi$date2=as.Date(paste(dt_cpi$date,'1',sep=' '),'%Y %b %d')
  dt_cpi$year=year(dt_cpi$date2)
  dt_cpi$month=month(dt_cpi$date2)
  
  dt=merge(dt,dt_cpi,by.x=c('incyear','incmonth'),by.y=c('year','month'),all.x=TRUE)
  
  dt$ytot=dt$fimngrs_dv*100/dt$cpi
  dt$ylab=dt$fimnlabgrs_dv*100/dt$cpi
  dt$ytothh=dt$fihhmngrs_dv*100/dt$cpi
      
  dt$fihhmngrs_eqv_dv=dt$fihhmngrs_dv/dt$eqv_fct
  
  # merge on previous income values
  
    dtmerge=dt[,.(pidp,intdate,fihhmngrs_eqv_dv,fihhmngrs_dv,fimngrs_dv,fimnlabgrs_dv)]
    colnames(dtmerge)=paste(colnames(dtmerge),'_L',1,sep='')
    
    dt=data.table(sqldf('select t1.*,
            t2.*
            from dt as t1 
            left join dtmerge as t2 
            on t1.intdate-365 between (t2.intdate_L1-62) and (t2.intdate_L1+62)
            and t1.pidp=t2.pidp_L1'))
  

  dt$fihhmngrs_eqv_dv_00=(dt$fihhmngrs_eqv_dv+dt$fihhmngrs_eqv_dv_L1*1.04)*0.5
  dt$fimngrs_dv_00=(dt$fimngrs_dv+dt$fimngrs_dv_L1*1.04)*0.5
  dt$fimnlabgrs_dv_00=(dt$fimnlabgrs_dv+dt$fimnlabgrs_dv_L1*1.04)*0.5
  
    
  # Income quantile
  
  dt_hmrc=read_xlsx('MP_Inequality/input/Table_3.1a_2021.xlsx',sheet='Table_3_1a_before_tax',range='A5:W104')
  dt_hmrc=melt(dt_hmrc,id.vars='pct')
  dt_hmrc$year=as.numeric(substr(dt_hmrc$variable,1,4))
  dt_hmrc=dcast(dt_hmrc,year~pct)
  colnames(dt_hmrc)[2:ncol(dt_hmrc)]=paste('pct',colnames(dt_hmrc)[2:ncol(dt_hmrc)],sep='')  
  dt_hmrc=data.table(dt_hmrc)

  dt_ybha=read_xlsx('MP_Inequality/input/Table_3.1a_2021.xlsx',sheet='ybha')
  dt_ybha$rel=dt_ybha$ybha/dt_ybha$ybha[dt_ybha$year==1999]  

  dt_pct=merge(dt_ybha,dt_hmrc,all.x=TRUE)  
  
  for(y in c(min(dt_pct$year):1998)){
    
    dt_pct[dt_pct$year==y,4:ncol(dt_pct)]=dt_pct[dt_pct$year==y,'rel']*dt_pct[dt_pct$year==1999,4:ncol(dt_pct)]
    
  }
  
  dt_pct=data.table(dt_pct)
  
  dt_pct$fy=dt_pct$year  
  
  dt=merge(dt,dt_pct[,.(fy,pct10,pct20,pct30,pct40,pct50,pct60,pct70,pct80,pct90,pct95,pct99)],by='fy',all.x=TRUE)
    
  
  assign_pct=function(dt,x,out){ # function to bin incomes
    
    z=dt[[x]]
    
    y10=10*(z<=dt$pct10)+
      20*(z>dt$pct10 & z<=dt$pct20)+
      30*(z>dt$pct20 & z<=dt$pct30)+
      40*(z>dt$pct30 & z<=dt$pct40)+
      50*(z>dt$pct40 & z<=dt$pct50)+
      60*(z>dt$pct50 & z<=dt$pct60)+
      70*(z>dt$pct60 & z<=dt$pct70)+
      80*(z>dt$pct70 & z<=dt$pct80)+
      90*(z>dt$pct80 & z<=dt$pct90)+
      99*(z>dt$pct90)
    
    y2=20*(z<=dt$pct20)+
      40*(z>dt$pct20 & z<=dt$pct40)+
      60*(z>dt$pct40 & z<=dt$pct60)+
      80*(z>dt$pct60 & z<=dt$pct80)+
      90*(z>dt$pct80 & z<=dt$pct90)+
      99*(z>dt$pct90)

    dt=dt[,paste('g10',out,sep=''):=y10]
    dt=dt[,paste('g2',out,sep=''):=y2]
    return()
  }
    
    assign_pct(dt,'fihhmngrs_eqv_dv_00','_hhbar')
    assign_pct(dt,'fihhmngrs_eqv_dv','_hh')
    assign_pct(dt,'fimngrs_dv','_ind')
    assign_pct(dt,'fimngrs_dv_00','_indbar')
    assign_pct(dt,'fimnlabgrs_dv','_ind')
    assign_pct(dt,'fimnlabgrs_dv_00','_indlab')
    

  
  # Housing tenure
  
  dt$tenure_dv=tolower(as.character(dt$tenure_dv))
  
  dt$renter=1*(dt$tenure_dv %in% c('housing assoc rented','local authority rent','other rented','rented from employer',
                                   'local authority rented','rented private furnished','rented private unfurnished'))
  
  dt$owner=1*(dt$tenure_dv %in% c('owned with mortgage','owned outright'))
  
  # Future values to merge on 
  
  for(h in c(1:2)){
    
  dtmerge=dt[,.(pidp,year,month,intdate,incyear,incmonth,ytot,ylab,ytothh,jbstat,E,U,g10_hh,g2_hh,g10_ind,g2_ind,owner,renter)]
  colnames(dtmerge)=paste(colnames(dtmerge),'_H',h,sep='')
  
  query=sprintf('select t1.*,
            t2.*
            from dt as t1 
            left join dtmerge as t2 
            on t1.intdate+365*%i between (t2.intdate_H%i-62*%i) and (t2.intdate_H%i+62*%i)
            and t1.pidp=t2.pidp_H%i',h,h,h,h,h,h)
  
  dt=data.table(sqldf(query))
  
  # differenc in months between interviews/ income observations
  dt=dt[,paste('dint',h,sep='_'):=as.numeric(round(difftime(dt[[paste('intdate_H',h,sep='')]],dt[['intdate']],'days')/30.417,0))]
  
    
    # change in income
    dt=dt[,paste('dytot_H',h,sep=''):=dt[[paste('ytot_H',h,sep='')]]/dt[['ytot']]-1]
    dt=dt[,paste('dylab_H',h,sep=''):=dt[[paste('ylab_H',h,sep='')]]/dt[['ylab']]-1]
    dt=dt[,paste('dytothh_H',h,sep=''):=dt[[paste('ytothh_H',h,sep='')]]/dt[['ytothh']]-1]
    
  }


  # save dataset
  
  save(dt, file='MP_Inequality/input/dt_clean.rda')
  
  ############################################
  ## some data checks
  ###########################################

  dochecks=0
  
  if(dochecks==1){
  
    dtagg=dt[ytot>1000&is.na(ytot)==FALSE&ytot<25e6,.(ytotw=weighted.mean(ytot,indin_xw,na.rm=TRUE),
                ytot=mean(ytot,na.rm=TRUE),
                Ew=weighted.mean(E,indin_xw,na.rm=TRUE),
                E=mean(E,na.rm=TRUE),
                pct90w=sum((g10=='90')*indin_xw)/sum(indin_xw),
                pct80w=sum((g10=='80')*indin_xw)/sum(indin_xw),
                pct70w=sum((g10=='70')*indin_xw)/sum(indin_xw),
                pct30w=sum((g10=='30')*indin_xw)/sum(indin_xw),
                pct10w=sum((g10=='10')*indin_xw)/sum(indin_xw),
                pct90=mean(1*(g10=='90'),na.rm=TRUE)),by=year]
    
dt_cpi=data.table(dt_cpi)
dt_cpiy=dt_cpi[,.('cpi'=mean(cpi)),by=year]
    
dt_hmrc2=data.table(melt(dt_pct,id.vars ='year'))
dt_hmrc2=dt_hmrc2[variable!='ybha']
dt_hmrc2=dt_hmrc2[,.(ybarnom=mean(value)),by=year]    
dt_hmrc2=merge(dt_hmrc2,dt_cpiy,by='year')
dt_hmrc2$ybar=dt_hmrc2$ybarnom/dt_hmrc2$cpi*100  

dtagg=merge(dtagg,dt_hmrc2,by='year',all.x=TRUE)

fg.y=ggplot(data=dtagg)+geom_line(aes(x=year,y=ytot,color='unweighted'))  +geom_line(aes(x=year,y=ytotw,color='weighted'))  +
       geom_line(aes(x=year,y=ybar,color='HMRC'))

       fg.E=ggplot(data=dtagg)+geom_line(aes(x=year,y=E))    

fg.pct90=ggplot(data=dtagg)+geom_line(aes(x=year,y=pct90w,color='pct90')) +
  geom_line(aes(x=year,y=pct80w,color='pct80'))   +
  geom_line(aes(x=year,y=pct70w,color='pct70'))   +
  geom_line(aes(x=year,y=pct30w,color='pct30'))+
  geom_line(aes(x=year,y=pct10w,color='pct10'))


dy=dt$dytot_H1[is.na(dt$dytot_H1)==FALSE&dt$ytot>0&dt$E==1&dt$ytot>1000]



dtw=dt[is.na(dt$ytothh_H1)==FALSE&year<=2019,.(.N,sum(indin_lw)),by=year]
dtw=dtw[order(year)]

}