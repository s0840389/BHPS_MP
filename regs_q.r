rm(list=ls())

require(data.table)
require(sandwich)
require(lmtest)
require(ivreg)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(dplyr)
require(stringr)
require(readxl)

setwd('~/OneDrive/Documents/research/BHPS/MP_Inequality/')

ressuf='_q_indbar3_g2'

# data

  load('input/dt_clean.rda')

  dtagg=data.table(read_excel('input/svar_data.xlsx',sheet='monthly',range='A2:AT386'))
  
    # create lags
  dtagg$year=year(dtagg$date)
  dtagg$month=month(dtagg$date)
  
  Y=dtagg[,.(year,month,`1 year rate`,`Exchange Rate`,`Corporate Spread`,FTSE,GDP,FSScm2,`CPI core`,opec,oil,kanzig,svarshock)]
  
  colnames(Y)=c('year','month','r1yr','FX','corp_spread','FTSE','GDP','FSScm2','CPIc','opec','oil','kanzig','svarshock')
  
  Y$dr=(Y$r1yr-lag(Y$r1yr))/100
  Y$svarshock=Y$svarshock/100
  Y$FSScm2=Y$FSScm2/100
  
  Y$qtr=ceiling(Y$month/3)
  
  Yq=Y[,.(dr=sum(dr),FSScm2=sum(FSScm2),
          svarshock=sum(svarshock),
          GDP=mean(GDP),
          FTSE=mean(FTSE),
          CPIc=mean(CPIc),
          corp_spread=mean(corp_spread)
  ),by=.(year,qtr)]
  
  for(x in colnames(Yq)[3:ncol(Yq)]){
    
    for(l in c(1:4)){
      Yq=Yq[,paste(x,'_L',l,sep=''):=lag(Yq[[x]],l)]
    }
  }    
  
  Yq$dgdp=(lead(Yq$GDP,4)-Yq$GDP)/100
  
  macrovars=c('dr','FTSE','GDP','CPIc','corp_spread')
  macrocontrols=paste(rep(macrovars,4),'_L',kronecker(c(1:4),rep(1,length(macrovars))),sep='')

  # merge on macro data  
    
    mininc=75*52 # based on real unemployment benefit 
    
    dt$incqtr=ceiling(dt$incmonth/3)
    
    dtm=merge(dt,Yq,by.x=c('incyear','incqtr'),by.y=c('year','qtr'),all.x=TRUE)
    
    dtm=dtm[ytot>mininc &age_dv>=20&age_dv<=65&year>=1996&year<=2019&
              is.na(g2_hhbar)==FALSE&
              is.na(g2_indbar3)==FALSE&
              is.na(ytot_H1)==FALSE&
              is.na(ytot_H2)==FALSE&
              is.na(ylab_H1)==FALSE&
            is.na(ylab_H1)==FALSE&
              ylab>=0+
              dint_1 %in% c(9:15)&
              dint_2 %in% c(20:30)]
    
    dtm=dtm[!(year %in% c(2007,2008,2009))]
    
    dtm$dytothh_H1=ifelse(dtm$dytothh_H1<10,dtm$dytothh_H1,10)
    dtm$dytothh_H2=ifelse(dtm$dytothh_H2<10,dtm$dytothh_H2,10)
    dtm$dytot_H1=ifelse(dtm$dytot_H1<10,dtm$dytot_H1,10)
    dtm$dytot_H2=ifelse(dtm$dytot_H2<10,dtm$dytot_H2,10)
    dtm$dylab_H2=ifelse(dtm$dylab_H2<10,dtm$dylab_H2,10)
    
      wgtrebal<-function(dt){ # rebalance to above to give each year same weight and have 10% weight in each income bucket by year
        
        dtw=dt[,.(wyear=sum(indin_lw,na.rm=TRUE),
                  wyearx=sum(indin_xw,na.rm=TRUE)),by=.(year)]
        
        dtw$wyear=1000/dtw$wyear
        dtw$wyearx=1000/dtw$wyearx
        
          
        dt=merge(dt,dtw,by=c('year'),all.x=TRUE)
        
        dt$indin_lwnorm=dt$indin_lw*dt$wyear
        dt$indin_xwnorm=dt$indin_xw*dt$wyearx
        
        minxw=quantile(dt$indin_xwnorm[dt$indin_xwnorm>0],0.0,na.rm=TRUE)
        minlw=quantile(dt$indin_lwnorm[dt$indin_lwnorm>0],0.0,na.rm=TRUE)
        
        dt$indin_xwnorm[dt$indin_xwnorm<minxw]=minxw
        dt$indin_lwnorm[dt$indin_lwnorm<minlw]=minlw
        
      return(dt)        
      }
      
      dtm=wgtrebal(dtm)
  
      dtm$wgt_sel=dtm$indin_lwnorm
      
      
      # Figures
      
      fg.hor<-function(coef,V,x,q,tit=''){
        
        xloc=c(grep(x,row.names(coef))[1],grep(paste(x,':',sep=''),row.names(coef)))
        
        pe=coef[xloc,1]+coef[xloc[1],1]
        pe[1]=coef[xloc[1],1]
        
        varx=diag(V)[xloc] + V[xloc[1],xloc[1]] + 2*V[xloc[1],xloc]
        varx[1]=V[xloc[1],xloc[1]] 
        
        l68=pe-sqrt(varx)*qnorm(0.84)
        l90=pe-sqrt(varx)*qnorm(0.95)
        u68=pe+sqrt(varx)*qnorm(0.84)
        u90=pe+sqrt(varx)*qnorm(0.95)
        
        dtfg=data.frame(q,pe,l68,l90,u68,u90)  
        
        fg=ggplot(data=dtfg)+geom_line(aes(x=q,y=pe),color='darkblue',linewidth=1.2)+
          geom_ribbon(aes(x=q,ymin=l68,ymax=u68),fill='blue',alpha=0.2)+
          geom_ribbon(aes(x=q,ymin=l90,ymax=u90),fill='blue',alpha=0.1)+
          theme_minimal(base_size=12)+labs(x='Income Percentile',y='',title=tit)
        
        return(fg) 
      }
      
      
    # macro controls
    
    Xagg=as.matrix(Yq[,macrocontrols,with=FALSE])
    Xm=as.matrix(dtm[,macrocontrols,with=FALSE])
    
    # macro specification
    
    magg24=ivreg(data=Yq,lead(GDP,6)~dr+Xagg,~FSScm2+Xagg)
    V.magg24=vcovHAC(magg24)
    magg.coef24=coeftest(magg24,V.magg24)
    
    q2=c(20,40,60,80,90,99)-5
    q10=c(10,20,30,40,50,60,70,80,90,100)-5
    
    q=q2
    
  # initial check on correlation with gdp
    
    m.dgdp=summary(lm(data=dtm,dytothh_H1~dgdp,weights = wgt_sel))

  # 1 year horizon regression on total household income
    
    m.h1.ytothh=ivreg(data=dtm[hoh==1],dytothh_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
   
     m.h1.ytothh=ivreg(data=dtm[hoh==1],log(ytothh_H1)~dr*factor(g2_indbar3)+log(ytothh),~FSScm2*factor(g2_indbar3)+log(ytothh),weights = wgt_sel)
    V.h1.ytothh=vcovCL(m.h1.ytothh,~wave)
    coef.h1.ytothh=coeftest(m.h1.ytothh,V.h1.ytothh)
    fg.h1.ytothh=fg.hor(coef.h1.ytothh,V.h1.ytothh,'dr',q2,'Household Income: 12 months')
    
  # 2 year horizon regression on total household income
    
    m.h2.ytothh=ivreg(data=dtm[hoh==1],dytothh_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
    m.h2.ytothh=ivreg(data=dtm[hoh==1],log(ytothh_H2)~dr*factor(g2_indbar3)+log(ytothh),~FSScm2*factor(g2_indbar3)+log(ytothh),weights = wgt_sel)
    V.h2.ytothh=vcovCL(m.h2.ytothh,~wave)
    coef.h2.ytothh=coeftest(m.h2.ytothh,V.h2.ytothh)
    fg.h2.ytothh=fg.hor(coef.h2.ytothh,V.h2.ytothh,'dr',q2,'Household Income: 24 months')

    # 1 year horizon regression on individual income
    
    m.h1.ytot=ivreg(data=dtm,dytot_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
    V.h1.ytot=vcovCL(m.h1.ytot,~wave)
    coef.h1.ytot=coeftest(m.h1.ytot,V.h1.ytot)
    fg.h1.ytot=fg.hor(coef.h1.ytot,V.h1.ytot,'dr',q2,'Individual Income: 12 months')
        
    # 2 year horizon regression on individual income
    
    m.h2.ytot=ivreg(data=dtm,dytot_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
    V.h2.ytot=vcovCL(m.h2.ytot,~wave)
    coef.h2.ytot=coeftest(m.h2.ytot,V.h2.ytot)
    fg.h2.ytot=fg.hor(coef.h2.ytot,V.h2.ytot,'dr',q2,'Individual Income: 24 months')
    
    # 1 year horizon regression on individual labour income
    
    m.h1.ylab=ivreg(data=dtm[ylab>mininc],dylab_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
    V.h1.ylab=vcovCL(m.h1.ylab,~wave)
    coef.h1.ylab=coeftest(m.h1.ylab,V.h1.ylab)
    fg.h1.ylab=fg.hor(coef.h1.ylab,V.h1.ylab,'dr',q2,'Labour Income: 12 months')
    
    # 2 year horizon regression on individual labour income
    
    m.h2.ylab=ivreg(data=dtm[ylab>mininc],dylab_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights = wgt_sel)
    V.h2.ylab=vcovCL(m.h2.ylab,~wave)
    coef.h2.ylab=coeftest(m.h2.ylab,V.h2.ylab)
    fg.h2.ylab=fg.hor(coef.h2.ylab,V.h2.ylab,'dr',q2,'Labour Income: 24 months')
    
    # 1 year horizon regression on Employment
    
    m.h1.E=ivreg(data=dtm,E_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3)+E,weights = wgt_sel)
    V.h1.E=vcovCL(m.h1.E,~wave)
    coef.h1.E=coeftest(m.h1.E,V.h1.E)
    fg.h1.E=fg.hor(coef.h1.E,V.h1.E,'dr',q2,'Employment: 12 months')    
    
    # 2 year horizon regression on Employment
    
    m.h2.E=ivreg(data=dtm,E_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3)+E,weights = wgt_sel)
    V.h2.E=vcovCL(m.h2.E,~wave)
    coef.h2.E=coeftest(m.h2.E,V.h2.E)
    fg.h2.E=fg.hor(coef.h2.E,V.h2.E,'dr',q2,'Employment: 24 months')    
    
    # 1 year horizon regression on unemployment
    
    m.h1.U=ivreg(data=dtm,U_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3)+E,weights = wgt_sel)
    V.h1.U=vcovCL(m.h1.U,~wave)
    coef.h1.U=coeftest(m.h1.U,V.h1.U)
    fg.h1.U=fg.hor(coef.h1.U,V.h1.U,'dr',q2,'Unemployment: 12 months')    
    
    # 2 year horizon regression on unemployment
    
    m.h2.U=ivreg(data=dtm,U_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3)+E,weights = wgt_sel)
    V.h2.U=vcovCL(m.h2.U,~wave)
    coef.h2.U=coeftest(m.h2.U,V.h2.U)
    fg.h2.U=fg.hor(coef.h2.U,V.h2.U,'dr',q2,'Unemployment: 24 months')    
    
    
    # save figures
    
    
    ggsave(paste('output/ytothh_h1',ressuf,'.pdf',sep=''),fg.h1.ytothh,units = 'cm',width=15,height=12)
    ggsave(paste('output/ytothh_h2',ressuf,'.pdf',sep=''),fg.h2.ytothh,units = 'cm',width=15,height=12)
    ggsave(paste('output/ytot_h1',ressuf,'.pdf',sep=''),fg.h1.ytot,units = 'cm',width=15,height=12)
    ggsave(paste('output/ytot_h2',ressuf,'.pdf',sep=''),fg.h2.ytot,units = 'cm',width=15,height=12)
    ggsave(paste('output/ylab_h1',ressuf,'.pdf',sep=''),fg.h1.ylab,units = 'cm',width=15,height=12)
    ggsave(paste('output/ylab_h2',ressuf,'.pdf',sep=''),fg.h2.ylab,units = 'cm',width=15,height=12)
    ggsave(paste('output/E_h1',ressuf,'.pdf',sep=''),fg.h1.E,units = 'cm',width=15,height=12)
    ggsave(paste('output/E_h2',ressuf,'.pdf',sep=''),fg.h2.E,units = 'cm',width=15,height=12)
    ggsave(paste('output/U_h1',ressuf,'.pdf',sep=''),fg.h1.U,units = 'cm',width=15,height=12)
    ggsave(paste('output/U_h2',ressuf,'.pdf',sep=''),fg.h2.U,units = 'cm',width=15,height=12)
    